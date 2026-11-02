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

#' S3 storage
#'
#' Files of an S3 bucket (AWS, or any S3 compatible server: MinIO, Ceph,
#' OVH...), read and written with signed HTTP requests. The paths of the
#' storage are the keys of the bucket below `prefix`.
#'
#' For a bucket with millions of keys, list the most precise folder
#' possible with [cloud_list()]: the server only filters by prefix, and
#' returns at most 1000 keys per request.
#'
#' @param bucket Name of the bucket.
#' @param prefix Folder of the bucket used as the root of the storage.
#' @param region Region of the bucket. By default the environment variable
#'   `AWS_DEFAULT_REGION`, or `us-east-1`.
#' @param endpoint URL of the S3 server, for a server that is not AWS
#'   (`https://host`). By default the AWS endpoint of `region`.
#' @param anonymous If `TRUE`, the requests are not signed: public bucket.
#'   Otherwise the credentials are read from the environment variables
#'   named by `access_key_env` and `secret_access_key_env`, and from
#'   `AWS_SESSION_TOKEN` when it is set.
#' @param access_key_env,secret_access_key_env Names of the environment
#'   variables holding the credentials. Never write a secret in a script.
#' @param read_only If `TRUE`, [cloud_upload()] and [cloud_delete()] fail.
#'   By default `TRUE` when `anonymous`.
#' @inheritParams cloud_nextcloud
#'
#' @return A `cloud_s3` storage object.
#' @family storages
#' @export
#'
#' @examples
#' # Public bucket of Meteo-France (DRIAS and EXPLORE2 climate projections)
#' cloud_s3("meteofrance-drias", endpoint = "https://object.files.data.gouv.fr",
#'          anonymous = TRUE)
cloud_s3 <- function(bucket,
                     prefix = "",
                     region = NULL,
                     endpoint = NULL,
                     anonymous = FALSE,
                     access_key_env = "AWS_ACCESS_KEY_ID",
                     secret_access_key_env = "AWS_SECRET_ACCESS_KEY",
                     read_only = NULL,
                     name = NULL,
                     cache = NULL,
                     max_age = 7,
                     max_size = 10) {
  check_string(bucket, "bucket")
  check_flag(anonymous, "anonymous")
  prefix <- dir_slash(remote_path(prefix))
  region <- region %||% Sys.getenv("AWS_DEFAULT_REGION")
  if (!nzchar(region)) region <- "us-east-1"
  if (is.null(endpoint)) {
    endpoint <- paste0("https://s3.", region, ".amazonaws.com")
  }
  check_string(endpoint, "endpoint")
  endpoint <- sub("/+$", "", endpoint)
  if (!grepl("^https?://", endpoint)) {
    stop("`endpoint` must start with http:// or https://: ", endpoint,
         call. = FALSE)
  }
  auth <- NULL
  if (!anonymous) {
    access_key <- Sys.getenv(access_key_env)
    secret_key <- Sys.getenv(secret_access_key_env)
    if (!nzchar(access_key) || !nzchar(secret_key)) {
      stop("An S3 account needs the environment variables ",
           access_key_env, " and ", secret_access_key_env,
           " (or `anonymous = TRUE` for a public bucket).", call. = FALSE)
    }
    token <- Sys.getenv("AWS_SESSION_TOKEN")
    auth <- make_auth_s3(access_key, secret_key,
                         if (nzchar(token)) token, region)
  }
  read_only <- read_only %||% anonymous
  name <- name %||% paste(bucket, strip_slash(prefix), sep = "_")
  new_cloud_storage(
    "s3", sub("_$", "", name), read_only,
    bucket = bucket, prefix = prefix, region = region, endpoint = endpoint,
    auth = auth, cache = cache, max_age = max_age, max_size = max_size
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
    s3 = paste0(storage$endpoint, "/", storage$bucket,
                if (nzchar(storage$prefix)) paste0("/", storage$prefix)),
    storage$name
  )
}

check_storage <- function(storage) {
  if (!inherits(storage, "cloud_storage")) {
    stop("`storage` must be a storage created by cloud_local(), ",
         "cloud_nextcloud(), cloud_s3() or cloud_from_config().", call. = FALSE)
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
