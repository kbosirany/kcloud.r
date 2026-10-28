new_cloud_storage <- function(backend, name, read_only, ..., cache = NULL,
                              max_age = 7, max_size = 10) {
  check_string(name, "name")
  check_flag(read_only, "read_only")
  if (!is.null(cache)) check_string(cache, "cache")
  check_number(max_age, "max_age")
  check_number(max_size, "max_size")
  structure(
    list(
      backend = backend,
      name = sanitize_name(name),
      read_only = read_only,
      ...,
      cache = if (is.null(cache)) NULL else path.expand(cache),
      max_age = max_age,
      max_size = max_size
    ),
    class = c(paste0("cloud_", backend), "cloud_storage")
  )
}

#' Local storage
#'
#' A folder of the local disk (or of a mounted network drive), used with the
#' same functions as the remote storages. Nothing is cached: [cloud_path()]
#' returns the path of the file in `root`.
#'
#' @param root Path of the folder that is the root of the storage. `~` is
#'   expanded.
#' @param read_only If `TRUE`, [cloud_upload()] and [cloud_delete()] fail.
#' @param name Name of the storage, used in the messages. By default the
#'   name of `root`.
#'
#' @return A `cloud_local` storage object.
#' @family storages
#' @export
#'
#' @examples
#' st <- cloud_local(tempdir())
#' st
cloud_local <- function(root, read_only = FALSE, name = NULL) {
  check_string(root, "root")
  root <- sub("(.)[/\\\\]+$", "\\1", path.expand(root))
  new_cloud_storage(
    "local", name %||% basename(root), read_only,
    root = root, max_age = Inf, max_size = Inf
  )
}

#' Nextcloud storage
#'
#' Files of a Nextcloud server, read and written through WebDAV. Two kinds
#' of access are supported:
#'
#' - a **public link** (`https://host/s/TOKEN`): no account is needed, and
#'   the storage is read only by default;
#' - an **account** (`https://host`): `user` and an application password
#'   (Nextcloud settings > Security > Devices & sessions) are required.
#'
#' @param url Public link of a shared folder (`https://host/s/TOKEN`), or URL
#'   of the server (`https://host`) to use an account.
#' @param user User name of the account. By default the environment variable
#'   `KCLOUD_NEXTCLOUD_USER`. Ignored with a public link.
#' @param password Password of the account. Prefer `password_env`: never
#'   write a password in a script.
#' @param password_env Name of the environment variable holding the
#'   password, used when `password` is `NULL`.
#' @param root Folder of the account (or of the shared folder) used as the
#'   root of the storage.
#' @param read_only If `TRUE`, [cloud_upload()] and [cloud_delete()] fail.
#'   By default `TRUE` for a public link and `FALSE` for an account.
#' @param name Name of the storage, used in the messages and as the name of
#'   its cache folder. By default built from the host and the token (or the
#'   user and `root`).
#' @param cache Folder of the cache. See [cloud_cache_dir()].
#' @param max_age Number of days a cached file is used without checking
#'   that it did not change on the server. `0` checks at each call, `Inf`
#'   never checks.
#' @param max_size Maximum size of the cache of this storage, in GB. Beyond
#'   it, the files read the least recently are removed. `Inf` disables the
#'   limit.
#'
#' @return A `cloud_nextcloud` storage object.
#' @family storages
#' @export
#'
#' @examples
#' cloud_nextcloud("https://nextcloud.example.org/s/AbCdEf123")
#'
#' \dontrun{
#' # Account, with the password in KCLOUD_NEXTCLOUD_PASSWORD
#' cloud_nextcloud("https://nextcloud.example.org", user = "me",
#'                 root = "Projects/data")
#' }
cloud_nextcloud <- function(url,
                            user = NULL,
                            password = NULL,
                            password_env = "KCLOUD_NEXTCLOUD_PASSWORD",
                            root = "",
                            read_only = NULL,
                            name = NULL,
                            cache = NULL,
                            max_age = 7,
                            max_size = 10) {
  check_string(url, "url")
  url <- sub("/+$", "", url)
  if (!grepl("^https?://", url)) {
    stop("`url` must start with http:// or https://: ", url, call. = FALSE)
  }
  root <- strip_slash(remote_path(root))
  share <- regmatches(url, regexec("^(https?://.+)/s/([A-Za-z0-9]+)$", url))
  share <- share[[1]]
  if (length(share) == 3) {
    url_dav <- paste0(share[2], "/public.php/dav/files/", share[3])
    user <- NULL
    auth <- NULL
    read_only <- read_only %||% TRUE
    name <- name %||% paste(url_host(url), share[3], root, sep = "_")
  } else {
    user <- user %||% Sys.getenv("KCLOUD_NEXTCLOUD_USER")
    check_string(password_env, "password_env")
    password <- password %||% Sys.getenv(password_env)
    if (!nzchar(user)) {
      stop("A Nextcloud account needs `user` (or the environment variable ",
           "KCLOUD_NEXTCLOUD_USER).", call. = FALSE)
    }
    if (!nzchar(password)) {
      stop("A Nextcloud account needs a password: set the environment ",
           "variable ", password_env, ".", call. = FALSE)
    }
    url_dav <- paste0(url, "/remote.php/dav/files/", curl::curl_escape(user))
    auth <- make_auth_basic(user, password)
    read_only <- read_only %||% FALSE
    name <- name %||% paste(url_host(url), user, root, sep = "_")
  }
  if (nzchar(root)) {
    url_dav <- paste0(url_dav, "/", escape_path(root))
  }
  new_cloud_storage(
    "nextcloud", sub("_$", "", name), read_only,
    url = url, url_dav = url_dav, user = user, root = root, auth = auth,
    cache = cache, max_age = max_age, max_size = max_size
  )
}

# The password stays in the closure: it is neither printed nor shown by
# str().
make_auth_basic <- function(user, password) {
  force(user)
  force(password)
  function(req) httr2::req_auth_basic(req, user, password)
}

#' @export
print.cloud_storage <- function(x, ...) {
  cat("<cloud_storage> ", x$backend, ": ", x$name, "\n", sep = "")
  cat("  location:  ", storage_location(x), "\n", sep = "")
  cat("  read only: ", x$read_only, "\n", sep = "")
  if (!inherits(x, "cloud_local")) {
    cat("  cache:     ", cloud_cache_dir(x), "\n", sep = "")
    cat("             max_age ", x$max_age, " days, max_size ", x$max_size,
        " GB\n", sep = "")
  }
  invisible(x)
}

storage_location <- function(storage) {
  switch(
    storage$backend,
    local = storage$root,
    nextcloud = paste0(
      storage$url,
      if (!is.null(storage$user)) paste0(" (user ", storage$user, ")"),
      if (nzchar(storage$root)) paste0(", root ", storage$root)
    ),
    storage$name
  )
}

check_storage <- function(storage) {
  if (!inherits(storage, "cloud_storage")) {
    stop("`storage` must be a storage created by cloud_local(), ",
         "cloud_nextcloud() or cloud_from_config().", call. = FALSE)
  }
  invisible(storage)
}

check_writable <- function(storage, what) {
  if (storage$read_only) {
    stop("kcloud.r - ", what, " - the storage '", storage$name,
         "' is read only.", call. = FALSE)
  }
  invisible(storage)
}
