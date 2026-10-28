local_full_path <- function(storage, path) {
  path <- strip_slash(path)
  ifelse(path == "", storage$root, file.path(storage$root, path))
}

local_listing <- function(storage, full, exists = NULL) {
  info <- file.info(full, extra_cols = FALSE)
  is_dir <- info$isdir %in% TRUE
  rel <- substring(full, nchar(storage$root) + 2)
  data.frame(
    path = ifelse(is_dir & rel != "", paste0(rel, "/"), rel),
    is_dir = is_dir,
    size = ifelse(is_dir, NA_real_, as.numeric(info$size)),
    modified = as.POSIXct(info$mtime, tz = "UTC"),
    etag = NA_character_,
    stringsAsFactors = FALSE
  )
}

#' @export
backend_get.cloud_local <- function(storage, path, path_local, workers) {
  src <- local_full_path(storage, path)
  missing <- !file.exists(src)
  if (any(missing)) {
    stop("kcloud.r - ", storage$name, " - not found: ",
         paste(path[missing], collapse = ", "), call. = FALSE)
  }
  for (d in unique(dirname(path_local))) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  ok <- file.copy(src, path_local, overwrite = TRUE)
  if (!all(ok)) {
    stop("Copy failed: ", paste(path[!ok], collapse = ", "), call. = FALSE)
  }
  rep(NA_character_, length(path))
}

#' @export
backend_get_dir.cloud_local <- function(storage, path, path_local) {
  src <- local_full_path(storage, path)
  if (!dir.exists(src)) {
    stop("kcloud.r - ", storage$name, " - folder not found: ", path,
         call. = FALSE)
  }
  dir.create(path_local, showWarnings = FALSE, recursive = TRUE)
  content <- list.files(src, full.names = TRUE, all.files = TRUE,
                        no.. = TRUE)
  ok <- file.copy(content, path_local, recursive = TRUE, overwrite = TRUE)
  if (!all(ok)) stop("Copy failed: ", path, call. = FALSE)
  invisible(path_local)
}

#' @export
backend_put.cloud_local <- function(storage, path_local, path, workers) {
  dest <- local_full_path(storage, path)
  for (d in unique(dirname(dest))) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  ok <- file.copy(path_local, dest, overwrite = TRUE)
  if (!all(ok)) {
    stop("Copy failed: ", paste(path[!ok], collapse = ", "), call. = FALSE)
  }
  invisible()
}

#' @export
backend_mkdir.cloud_local <- function(storage, path, workers) {
  for (d in local_full_path(storage, path)) {
    dir.create(d, showWarnings = FALSE, recursive = TRUE)
  }
  invisible()
}

#' @export
backend_delete.cloud_local <- function(storage, path) {
  full <- local_full_path(storage, path)
  if (!file.exists(full)) {
    stop("kcloud.r - ", storage$name, " - not found: ", path, call. = FALSE)
  }
  unlink(full, recursive = TRUE, force = TRUE)
  invisible()
}

#' @export
backend_stat.cloud_local <- function(storage, path, workers) {
  full <- local_full_path(storage, path)
  listing <- local_listing(storage, full)
  listing$path <- path
  listing$exists <- file.exists(full)
  listing
}

#' @export
backend_ls.cloud_local <- function(storage, path, recursive, workers,
                                   missing_ok = FALSE) {
  dfs <- lapply(path, function(p) {
    full <- local_full_path(storage, p)
    if (!dir.exists(full)) {
      if (missing_ok) return(NULL)
      stop("kcloud.r - ", storage$name, " - folder not found: ", p,
           call. = FALSE)
    }
    content <- list.files(full, recursive = recursive, include.dirs = TRUE,
                          all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (length(content) == 0) return(NULL)
    local_listing(storage, content)
  })
  bind_listings(dfs)
}
