#' Upload files to a storage
#'
#' Uploads a local file, or the whole content of a local folder, to a
#' storage. The missing remote folders are created. The uploads run in
#' parallel and the cached copies of the uploaded paths are removed.
#'
#' @inheritParams cloud_path
#' @param path_local Local file or folder.
#' @param path Remote destination. For a file, a path ending with `/` is a
#'   folder that receives the file under its own name. For a folder, the
#'   remote folder that receives its content.
#' @param overwrite If `FALSE`, fails when one of the remote files exists.
#' @param workers Maximum number of simultaneous requests. By default the
#'   option `kcloud.r.workers`, or 4 for Nextcloud (its servers limit the
#'   request rate) and 16 for the other backends.
#'
#' @return The remote paths of the uploaded files, invisibly.
#' @family operations
#' @export
#'
#' @examples
#' st <- cloud_local(tempfile())
#' f <- tempfile(fileext = ".csv")
#' write.csv(head(iris), f)
#' cloud_upload(st, f, "iris/")
#' cloud_list(st, "iris/")
cloud_upload <- function(storage, path_local, path, overwrite = FALSE,
                         workers = workers_default(storage)) {
  check_storage(storage)
  UseMethod("cloud_upload")
}

#' @export
cloud_upload.cloud_storage <- function(storage, path_local, path,
                                       overwrite = FALSE,
                                       workers = workers_default(storage)) {
  check_writable(storage, "cloud_upload")
  check_string(path_local, "path_local")
  check_flag(overwrite, "overwrite")
  path <- remote_path(path)
  if (dir.exists(path_local)) {
    rel <- list.files(path_local, recursive = TRUE, all.files = TRUE)
    paths_local <- file.path(path_local, rel)
    paths <- paste0(dir_slash(path), rel)
    dirs <- c(if (path != "") dir_slash(path), remote_ancestors(paths))
  } else if (file.exists(path_local)) {
    paths_local <- path_local
    paths <- if (is_dir_path(path)) {
      paste0(path, basename(path_local))
    } else {
      path
    }
    dirs <- remote_ancestors(paths)
  } else {
    stop("Not found: ", path_local, call. = FALSE)
  }
  if (length(paths) == 0) {
    return(invisible(character()))
  }
  if (!overwrite) {
    exists <- cloud_exists(storage, paths, workers = workers)
    if (any(exists)) {
      stop("kcloud.r - cloud_upload - already on the storage (use ",
           "`overwrite = TRUE`): ",
           paste(utils::head(paths[exists], 5), collapse = ", "),
           if (sum(exists) > 5) ", ...", call. = FALSE)
    }
  }
  dirs <- unique(dirs)
  dirs <- dirs[order(lengths(gregexpr("/", dirs)), dirs)]
  if (length(dirs)) backend_mkdir(storage, dirs, workers = workers)
  backend_put(storage, paths_local, paths, workers = workers)
  cache_invalidate(storage, c(paths, dirs))
  invisible(paths)
}

#' Delete files or folders of a storage
#'
#' @inheritParams cloud_path
#' @param path Remote paths of files or folders. A folder is deleted with
#'   all its content.
#'
#' @return `NULL`, invisibly.
#' @family operations
#' @export
cloud_delete <- function(storage, path) {
  check_storage(storage)
  UseMethod("cloud_delete")
}

#' @export
cloud_delete.cloud_storage <- function(storage, path) {
  check_writable(storage, "cloud_delete")
  path <- remote_paths(path)
  if (any(path == "")) {
    stop("The root of a storage cannot be deleted.", call. = FALSE)
  }
  for (p in path) backend_delete(storage, p)
  cache_invalidate(storage, c(path, remote_ancestors(path)))
  invisible()
}

# Removes the cached copies of `path` and of the cached folders above it.
cache_invalidate <- function(storage, path) {
  if (inherits(storage, "cloud_local")) {
    return(invisible())
  }
  path <- unique(strip_slash(path))
  path_cache <- file.path(cloud_cache_dir(storage), path[path != ""])
  cached <- file.exists(meta_path(path_cache))
  if (any(cached)) {
    cloud_cache_clean(storage, path[path != ""][cached])
  }
  invisible()
}

#' Check that files or folders exist on a storage
#'
#' Vectorized: for many paths, each folder that contains them is listed
#' once instead of one request per path.
#'
#' @inheritParams cloud_upload
#' @param path Remote paths. A path ending with `/` must be a folder, a path
#'   without it can be a file or a folder.
#'
#' @return A logical vector, the same length as `path`.
#' @family operations
#' @export
cloud_exists <- function(storage, path, workers = workers_default(storage)) {
  check_storage(storage)
  UseMethod("cloud_exists")
}

#' @export
cloud_exists.cloud_storage <- function(storage, path,
                                       workers = workers_default(storage)) {
  path <- remote_paths(path)
  if (length(path) == 0) {
    return(logical())
  }
  root <- path == ""
  parents <- remote_dirname(path[!root])
  out <- rep(TRUE, length(path))
  if (all(root)) {
    return(out)
  }
  # Listing the folders pays off when they hold several of the paths.
  many_folders <- length(unique(parents)) > length(parents) / 4
  if (length(parents) <= 20 || many_folders) {
    stat <- backend_stat(storage, path[!root], workers = workers)
    found <- stat$exists
    is_dir <- stat$is_dir
  } else {
    listing <- backend_ls(storage, unique(parents), recursive = FALSE,
                          workers = workers, missing_ok = TRUE)
    idx <- match(strip_slash(path[!root]), strip_slash(listing$path))
    found <- !is.na(idx)
    is_dir <- listing$is_dir[idx]
  }
  out[!root] <- found & (!is_dir_path(path[!root]) | is_dir %in% TRUE)
  out
}

#' List the content of a folder of a storage
#'
#' For a large storage, list the most precise folder possible: the server
#' only filters by folder. A recursive listing lists the subfolders in
#' parallel, level by level.
#'
#' @inheritParams cloud_upload
#' @param path Remote folder. `""` is the root of the storage.
#' @param recursive If `TRUE`, lists the content of all the subfolders.
#' @param pattern Regular expression the listed paths must match, or `NULL`.
#' @param max_age Number of days the listing is cached and reused. `0` (the
#'   default) does not use the cache.
#'
#' @return A data.frame with the columns `path` (relative to the root of the
#'   storage, folders ending with `/`), `is_dir`, `size` (bytes), `modified`
#'   (POSIXct, UTC) and `etag`.
#' @family operations
#' @export
#'
#' @examples
#' st <- cloud_local(tempfile())
#' f <- tempfile(fileext = ".csv")
#' write.csv(head(iris), f)
#' cloud_upload(st, f, "a/b/")
#' cloud_list(st, recursive = TRUE)
cloud_list <- function(storage, path = "", recursive = FALSE, pattern = NULL,
                       max_age = 0, workers = workers_default(storage)) {
  check_storage(storage)
  UseMethod("cloud_list")
}

#' @export
cloud_list.cloud_storage <- function(storage, path = "", recursive = FALSE,
                                     pattern = NULL, max_age = 0,
                                     workers = workers_default(storage)) {
  path <- dir_slash(remote_path(path))
  check_flag(recursive, "recursive")
  check_number(max_age, "max_age")
  use_cache <- max_age > 0 && !inherits(storage, "cloud_local")
  listing <- if (use_cache) {
    list_cache_read(storage, path, recursive, max_age)
  }
  if (is.null(listing)) {
    listing <- backend_ls(storage, path, recursive = recursive,
                          workers = workers)
    if (use_cache) list_cache_write(storage, path, recursive, listing)
  }
  if (!is.null(pattern)) {
    listing <- listing[grepl(pattern, listing$path), , drop = FALSE]
    rownames(listing) <- NULL
  }
  listing
}
