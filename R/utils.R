`%||%` <- function(x, y) if (is.null(x)) y else x

# Joins path parts with "/" into a remote path relative to the storage root.
# Backslashes become "/", repeated and leading "/" are removed, and a
# trailing "/" is kept: it marks a folder.
remote_path <- function(...) {
  parts <- unlist(list(...), use.names = FALSE)
  if (length(parts) == 0) {
    return("")
  }
  if (!is.character(parts) || anyNA(parts)) {
    stop("The parts of a path must be character strings.", call. = FALSE)
  }
  path <- paste(parts, collapse = "/")
  path <- gsub("\\\\", "/", path)
  path <- gsub("/+", "/", path)
  path <- sub("^/", "", path)
  if (any(strsplit(path, "/", fixed = TRUE)[[1]] %in% c(".", ".."))) {
    stop("`.` and `..` are not allowed in a remote path: ", path,
         call. = FALSE)
  }
  path
}

remote_paths <- function(path) {
  vapply(path, remote_path, character(1), USE.NAMES = FALSE)
}

is_dir_path <- function(path) {
  path == "" | grepl("/$", path)
}

strip_slash <- function(path) {
  sub("/$", "", path)
}

# Folder path ending with "/", or "" for the root.
dir_slash <- function(path) {
  ifelse(path == "" | grepl("/$", path), path, paste0(path, "/"))
}

# Parent folder of remote paths, ending with "/" ("" for the root).
remote_dirname <- function(path) {
  parent <- sub("[^/]*$", "", strip_slash(path))
  parent
}

# All the ancestor folders of remote paths, from the shallowest, without the
# root.
remote_ancestors <- function(path) {
  parents <- unique(remote_dirname(path))
  parents <- parents[parents != ""]
  out <- character()
  for (parent in parents) {
    parts <- strsplit(strip_slash(parent), "/", fixed = TRUE)[[1]]
    out <- c(out, vapply(
      seq_along(parts),
      function(i) paste0(paste(parts[seq_len(i)], collapse = "/"), "/"),
      character(1)
    ))
  }
  out <- unique(out)
  out[order(lengths(regmatches(out, gregexpr("/", out))), out)]
}

# Escapes each segment of a remote path for a URL, keeping the "/".
escape_path <- function(path) {
  vapply(path, function(p) {
    if (p == "") {
      return("")
    }
    parts <- strsplit(p, "/", fixed = TRUE)[[1]]
    out <- paste(curl::curl_escape(parts), collapse = "/")
    if (grepl("/$", p)) paste0(out, "/") else out
  }, character(1), USE.NAMES = FALSE)
}

sanitize_name <- function(x) {
  gsub("[^A-Za-z0-9._-]+", "_", x)
}

url_host <- function(url) {
  sub("^https?://([^/]+).*$", "\\1", url)
}

check_string <- function(x, arg) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a single non-empty string.", call. = FALSE)
  }
  invisible(x)
}

check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(x)
}

check_number <- function(x, arg, min = 0) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < min) {
    stop("`", arg, "` must be a number >= ", min, ".", call. = FALSE)
  }
  invisible(x)
}

# Empty listing, with the columns of cloud_list().
empty_listing <- function() {
  data.frame(
    path = character(),
    is_dir = logical(),
    size = numeric(),
    modified = as.POSIXct(character(), tz = "UTC"),
    etag = character(),
    stringsAsFactors = FALSE
  )
}

bind_listings <- function(dfs) {
  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0) {
    return(empty_listing())
  }
  df <- do.call(rbind, dfs)
  df <- df[order(df$path), , drop = FALSE]
  rownames(df) <- NULL
  df
}

# Replaces `to` by `from` (file or folder), as atomically as the file system
# allows: a reader sees the old or the new version, never a partial one.
replace_path <- function(from, to) {
  if (file.exists(to)) {
    old <- paste0(to, ".old-", Sys.getpid(), "-", sample.int(1e9, 1))
    if (!file.rename(to, old)) {
      unlink(to, recursive = TRUE, force = TRUE)
    }
    on.exit(unlink(old, recursive = TRUE, force = TRUE), add = TRUE)
  }
  if (!file.rename(from, to)) {
    # Another device, or a file locked on Windows: copy instead.
    ok <- if (dir.exists(from)) {
      dir.create(to, showWarnings = FALSE, recursive = TRUE)
      all(file.copy(
        list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE),
        to, recursive = TRUE, overwrite = TRUE
      ))
    } else {
      file.copy(from, to, overwrite = TRUE)
    }
    unlink(from, recursive = TRUE, force = TRUE)
    if (!ok) stop("Cannot write: ", to, call. = FALSE)
  }
  invisible(to)
}

# Temporary path next to `path`, on the same file system.
temp_sibling <- function(path) {
  paste0(path, ".tmp-", Sys.getpid(), "-", sample.int(1e9, 1))
}

# Nextcloud servers limit the request rate: fewer simultaneous requests.
workers_default <- function(storage = NULL) {
  getOption(
    "kcloud.r.workers",
    if (inherits(storage, "cloud_nextcloud")) 4L else 16L
  )
}
