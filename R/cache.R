#' Cache of a storage
#'
#' `cloud_cache_dir()` gives the folder where the files of a remote storage
#' are cached. `cloud_cache_clean()` removes all of it, or only some paths.
#'
#' @details
#' The root of the cache is, by order of precedence:
#'
#' 1. the `cache` argument of the storage (or the `cache` key of its
#'    configuration);
#' 2. the environment variable `KCLOUD_CACHE`;
#' 3. `tools::R_user_dir("kcloud.r", "cache")`.
#'
#' Each storage has its own subfolder, named after the storage. The cache is
#' persistent: it is kept from one R session to another. Next to each cached
#' file (or folder), a small `.kcloud` file records its ETag and the dates of
#' its last check and last read. The cache stays consistent when several R
#' processes use it at the same time.
#'
#' @param storage A storage object (see [cloud_nextcloud()]).
#' @param path Remote paths to remove from the cache. `NULL` removes the
#'   whole cache of the storage.
#'
#' @return `cloud_cache_dir()`: the path of the cache folder.
#'   `cloud_cache_clean()`: `NULL`, invisibly.
#' @export
#'
#' @examples
#' st <- cloud_nextcloud("https://nextcloud.example.org/s/AbCdEf123")
#' cloud_cache_dir(st)
cloud_cache_dir <- function(storage) {
  check_storage(storage)
  if (inherits(storage, "cloud_local")) {
    stop("A local storage has no cache.", call. = FALSE)
  }
  root <- storage$cache
  if (is.null(root)) {
    root <- Sys.getenv("KCLOUD_CACHE")
    if (!nzchar(root)) root <- tools::R_user_dir("kcloud.r", "cache")
  }
  file.path(path.expand(root), storage$name)
}

#' @rdname cloud_cache_dir
#' @export
cloud_cache_clean <- function(storage, path = NULL) {
  check_storage(storage)
  if (inherits(storage, "cloud_local")) {
    return(invisible())
  }
  dir <- cloud_cache_dir(storage)
  targets <- if (is.null(path)) {
    dir
  } else {
    path <- strip_slash(remote_paths(path))
    path_cache <- file.path(dir, path[path != ""])
    c(path_cache, meta_path(path_cache))
  }
  unlink(targets, recursive = TRUE, force = TRUE)
  if (any(file.exists(targets))) {
    stop("Clean data cache failed in: ",
         paste(targets[file.exists(targets)], collapse = ", "), call. = FALSE)
  }
  cache_size_forget(dir)
  invisible()
}

# Sidecar metadata -------------------------------------------------------------

meta_path <- function(path_cache) {
  paste0(path_cache, ".kcloud")
}

meta_read <- function(path_cache) {
  file <- meta_path(path_cache)
  if (!file.exists(file)) {
    return(NULL)
  }
  tryCatch(jsonlite::read_json(file), error = function(e) NULL)
}

meta_write <- function(path_cache, meta) {
  file <- meta_path(path_cache)
  tmp <- temp_sibling(file)
  jsonlite::write_json(meta, tmp, auto_unbox = TRUE, digits = NA,
                       null = "null")
  replace_path(tmp, file)
}

now_seconds <- function() {
  as.numeric(Sys.time())
}

# Records a read, at most once an hour to spare the writes.
meta_touch <- function(path_cache, meta) {
  now <- now_seconds()
  if (now - (meta$accessed %||% 0) > 3600) {
    meta$accessed <- now
    meta_write(path_cache, meta)
  }
  invisible()
}

# Entry size and eviction ------------------------------------------------------

# Approximate size of the cache of each storage in this session, to avoid
# scanning the cache after each download.
cache_state <- new.env(parent = emptyenv())

cache_size_forget <- function(dir) {
  if (exists(dir, envir = cache_state, inherits = FALSE)) {
    rm(list = dir, envir = cache_state)
  }
}

path_size <- function(path) {
  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE,
                        all.files = TRUE)
    sum(file.size(files), na.rm = TRUE)
  } else {
    sum(file.size(path), na.rm = TRUE)
  }
}

# Adds `size` bytes to the cache of `storage` and removes the entries read
# the least recently when the cache is over max_size (down to 80 %). The
# entries in `keep` are never removed.
cache_evict <- function(storage, size, keep = character()) {
  max_bytes <- storage$max_size * 1024^3
  if (!is.finite(max_bytes)) {
    return(invisible())
  }
  dir <- cloud_cache_dir(storage)
  total <- cache_state[[dir]]
  total <- if (is.null(total)) path_size(dir) else total + size
  if (total > max_bytes) {
    # The approximate size is over: scan the cache for the true one.
    total <- path_size(dir)
  }
  if (total > max_bytes) {
    metas <- list.files(dir, pattern = "[.]kcloud$", recursive = TRUE,
                        full.names = TRUE, all.files = TRUE)
    entries <- sub("[.]kcloud$", "", metas)
    accessed <- vapply(metas, function(m) {
      meta <- tryCatch(jsonlite::read_json(m), error = function(e) list())
      as.numeric(meta$accessed %||% 0)
    }, numeric(1), USE.NAMES = FALSE)
    candidates <- order(accessed)
    candidates <- candidates[!entries[candidates] %in% keep]
    for (i in candidates) {
      if (total <= 0.8 * max_bytes) break
      if (!file.exists(entries[i])) next
      total <- total - path_size(entries[i])
      unlink(c(entries[i], metas[i]), recursive = TRUE, force = TRUE)
    }
  }
  assign(dir, total, envir = cache_state)
  invisible()
}

# Listing cache ----------------------------------------------------------------

list_cache_file <- function(storage, path, recursive) {
  key <- paste0(path, "|", recursive)
  hash <- digest_string(key)
  file.path(cloud_cache_dir(storage), ".lists", paste0(hash, ".rds"))
}

digest_string <- function(x) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(x, tmp, useBytes = TRUE)
  unname(tools::md5sum(tmp))
}

list_cache_read <- function(storage, path, recursive, max_age) {
  file <- list_cache_file(storage, path, recursive)
  if (!file.exists(file)) {
    return(NULL)
  }
  cached <- tryCatch(readRDS(file), error = function(e) NULL)
  if (is.null(cached) || now_seconds() - cached$time > max_age * 86400) {
    return(NULL)
  }
  cached$listing
}

list_cache_write <- function(storage, path, recursive, listing) {
  file <- list_cache_file(storage, path, recursive)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  tmp <- temp_sibling(file)
  saveRDS(list(time = now_seconds(), listing = listing), tmp)
  replace_path(tmp, file)
}
