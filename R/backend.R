# Internal generics implemented by each backend. The exported functions
# (cloud_path(), cloud_list()...) are built on them: the folders, the cache,
# max_age, max_size and read_only are handled once, in the core.
#
# - backend_get(storage, path, path_local, workers): downloads the files
#   `path` to `path_local` (same length) and returns their ETags (NA when
#   unknown).
# - backend_get_dir(storage, path, path_local): downloads the content of the
#   folder `path` into the new folder `path_local`.
# - backend_put(storage, path_local, path, workers): uploads files, the
#   remote folders already exist.
# - backend_mkdir(storage, path, workers): creates remote folders, from the
#   shallowest, existing ones are ignored.
# - backend_delete(storage, path): deletes a remote file or folder.
# - backend_stat(storage, path, workers): listing (see cloud_list()) of the
#   paths themselves, with a column `exists`.
# - backend_ls(storage, path, recursive, workers, missing_ok): listing of the
#   content of the folders `path`.

backend_get <- function(storage, path, path_local, workers) {
  UseMethod("backend_get")
}

backend_get_dir <- function(storage, path, path_local) {
  UseMethod("backend_get_dir")
}

# Default: list the folder, then download its files in parallel.
#' @export
backend_get_dir.cloud_storage <- function(storage, path, path_local) {
  listing <- backend_ls(storage, dir_slash(path), recursive = TRUE,
                        workers = workers_default(storage))
  dir.create(path_local, showWarnings = FALSE, recursive = TRUE)
  rel <- substring(listing$path, nchar(dir_slash(path)) + 1)
  for (d in rel[listing$is_dir]) {
    dir.create(file.path(path_local, d), showWarnings = FALSE,
               recursive = TRUE)
  }
  files <- !listing$is_dir
  if (any(files)) {
    backend_get(storage, listing$path[files],
                file.path(path_local, rel[files]), workers_default(storage))
  }
  invisible(path_local)
}

backend_put <- function(storage, path_local, path, workers) {
  UseMethod("backend_put")
}

backend_mkdir <- function(storage, path, workers) {
  UseMethod("backend_mkdir")
}

backend_delete <- function(storage, path) {
  UseMethod("backend_delete")
}

backend_stat <- function(storage, path, workers) {
  UseMethod("backend_stat")
}

backend_ls <- function(storage, path, recursive, workers, missing_ok = FALSE) {
  UseMethod("backend_ls")
}
