#' Local path of a file of a storage
#'
#' Gives a local path that any reading function can use (`read.csv()`,
#' `readxl::read_excel()`, `terra::rast()`...):
#'
#' - for a [cloud_local()] storage, the path of the file in its root,
#'   without any copy (the file may not exist yet, e.g. to write it);
#' - for a remote storage, the file (or the folder, when the path ends with
#'   `/`) is downloaded into the cache (see [cloud_cache_dir()]) and its
#'   path in the cache is returned.
#'
#' A cached file is used without any request during `max_age` days. After
#' that, its ETag is checked on the server with one light request: if the
#' file did not change, it is not downloaded again. If the server cannot be
#' reached, the cached copy is used with a warning.
#'
#' @param storage A storage object (see [cloud_nextcloud()]).
#' @param ... Parts of the remote path, joined with `/`. A trailing `/`
#'   designates a folder.
#' @param max_age Number of days the cached copy is used without checking
#'   the server. `NULL` uses the `max_age` of the storage.
#'
#' @return The local path of the file or folder.
#' @family operations
#' @export
#'
#' @examples
#' root <- tempfile()
#' dir.create(root)
#' st <- cloud_local(root)
#' cloud_path(st, "Raw", "obs.csv")
#'
#' \dontrun{
#' st <- cloud_nextcloud("https://nextcloud.example.org/s/AbCdEf123")
#' read.csv(cloud_path(st, "Raw/obs.csv"))
#' # A folder
#' list.files(cloud_path(st, "Raw/2023/"))
#' }
cloud_path <- function(storage, ..., max_age = NULL) {
  check_storage(storage)
  UseMethod("cloud_path")
}

#' @export
cloud_path.cloud_local <- function(storage, ..., max_age = NULL) {
  path <- strip_slash(remote_path(...))
  if (path == "") storage$root else file.path(storage$root, path)
}

#' @export
cloud_path.cloud_storage <- function(storage, ..., max_age = NULL) {
  path <- remote_path(...)
  if (path == "") {
    stop("cloud_path() cannot cache the whole storage: give a file or a ",
         "folder, or use cloud_download().", call. = FALSE)
  }
  max_age <- max_age %||% storage$max_age
  check_number(max_age, "max_age")
  log_base <- paste0("kcloud.r - cloud_path - ", storage$name)
  is_dir <- is_dir_path(path)
  path_cache <- file.path(cloud_cache_dir(storage), strip_slash(path))
  meta <- meta_read(path_cache)
  cached <- !is.null(meta) &&
    (if (is_dir) dir.exists(path_cache) else file.exists(path_cache) &&
       !dir.exists(path_cache))

  if (cached) {
    if (now_seconds() - as.numeric(meta$checked) < max_age * 86400) {
      meta_touch(path_cache, meta)
      return(path_cache)
    }
    stat <- tryCatch(
      backend_stat(storage, path, workers = 1L),
      error = function(e) e
    )
    if (inherits(stat, "error")) {
      warning(log_base, " - cannot check ", path, " on the server, the ",
              "cached copy is used: ", conditionMessage(stat), call. = FALSE)
      return(path_cache)
    }
    if (!stat$exists) {
      stop(log_base, " - not found on the server: ", path, call. = FALSE)
    }
    if (!is.na(stat$etag) && identical(stat$etag, meta$etag)) {
      meta$checked <- meta$accessed <- now_seconds()
      meta_write(path_cache, meta)
      return(path_cache)
    }
  }

  message(log_base, " - download ", path)
  dir.create(dirname(path_cache), showWarnings = FALSE, recursive = TRUE)
  tmp <- temp_sibling(path_cache)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  if (is_dir) {
    stat <- backend_stat(storage, path, workers = 1L)
    if (!stat$exists) {
      stop(log_base, " - not found on the server: ", path, call. = FALSE)
    }
    backend_get_dir(storage, path, tmp)
    etag <- stat$etag
  } else {
    etag <- backend_get(storage, path, tmp, workers = 1L)
  }
  replace_path(tmp, path_cache)
  now <- now_seconds()
  meta_write(path_cache, list(
    path = path, etag = etag, checked = now, accessed = now
  ))
  size <- path_size(path_cache) + path_size(meta_path(path_cache))
  cache_evict(storage, size, keep = path_cache)
  path_cache
}

#' URL of a file of a storage
#'
#' Gives the URL of a remote file, without downloading it. A folder (path
#' ending with `/`) is given as a ZIP archive with Nextcloud.
#'
#' @inheritParams cloud_path
#'
#' @return The URL.
#' @family operations
#' @export
#'
#' @examples
#' st <- cloud_nextcloud("https://nextcloud.example.org/s/AbCdEf123")
#' cloud_url(st, "Raw/obs.csv")
cloud_url <- function(storage, ...) {
  check_storage(storage)
  UseMethod("cloud_url")
}

#' @export
cloud_url.cloud_storage <- function(storage, ...) {
  stop("The ", storage$backend, " backend has no URL.", call. = FALSE)
}

#' Download a file or a folder of a storage
#'
#' Copies a remote file or folder to a chosen local path, without using the
#' cache.
#'
#' @inheritParams cloud_path
#' @param path Remote path. A trailing `/` designates a folder.
#' @param path_local Local destination: the file, or the folder that
#'   receives the content of the remote folder.
#' @param overwrite If `FALSE`, fails when `path_local` exists.
#'
#' @return `path_local`, invisibly.
#' @family operations
#' @export
cloud_download <- function(storage, path, path_local, overwrite = FALSE) {
  check_storage(storage)
  UseMethod("cloud_download")
}

#' @export
cloud_download.cloud_storage <- function(storage, path, path_local,
                                         overwrite = FALSE) {
  path <- remote_path(path)
  check_string(path_local, "path_local")
  check_flag(overwrite, "overwrite")
  if (file.exists(path_local) && !overwrite) {
    stop("Already exists (use `overwrite = TRUE`): ", path_local,
         call. = FALSE)
  }
  path_local <- sub("(.)[/\\\\]+$", "\\1", path_local)
  dir.create(dirname(path_local), showWarnings = FALSE, recursive = TRUE)
  tmp <- temp_sibling(path_local)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)
  if (is_dir_path(path)) {
    backend_get_dir(storage, path, tmp)
  } else {
    backend_get(storage, path, tmp, workers = 1L)
  }
  replace_path(tmp, path_local)
  invisible(path_local)
}
