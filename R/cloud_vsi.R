#' GDAL path of a file of a storage
#'
#' Gives a path that GDAL reads straight from the storage, with HTTP range
#' requests: `terra::rast()`, `stars::read_stars()` or `sf::read_sf()` only
#' fetch the bytes they need. For a big file this beats [cloud_path()] by
#' far when a small part of it is read: a point of a 2.6 GB NetCDF takes
#' a few seconds instead of a full download.
#'
#' How much is read depends on how the file is stored: a NetCDF whose chunks
#' are not compressed lets GDAL read only the pixels asked for, while
#' compressed chunks are read whole. A file chunked as one map per time step
#' is fast for maps and slow for the time series of a single pixel (one
#' request per time step: run them in parallel processes).
#'
#' @details
#' - A local storage gives the path of the file.
#' - A public S3 bucket (`anonymous = TRUE`) gives a `/vsicurl/` path.
#' - An S3 account gives a `/vsis3/` path, signed by GDAL. For that, the
#'   GDAL environment variables `AWS_S3_ENDPOINT`, `AWS_HTTPS`,
#'   `AWS_VIRTUAL_HOSTING`, `AWS_REGION` and the credentials are set for
#'   the R session: they are shared by all the S3 storages, the last call
#'   wins.
#'
#' @inheritParams cloud_path
#'
#' @return A path to give to GDAL.
#' @family operations
#' @export
#'
#' @examples
#' st <- cloud_s3("meteofrance-drias",
#'                endpoint = "https://object.files.data.gouv.fr",
#'                anonymous = TRUE)
#' cloud_vsi(st, "EXPLORE2-Hydro/file.nc")
#'
#' \dontrun{
#' r <- terra::rast(cloud_vsi(st, "EXPLORE2-Hydro/file.nc"))
#' }
cloud_vsi <- function(storage, ...) {
  check_storage(storage)
  UseMethod("cloud_vsi")
}

#' @export
cloud_vsi.cloud_storage <- function(storage, ...) {
  stop("The ", storage$backend, " backend has no GDAL path: use ",
       "cloud_path() to download the file.", call. = FALSE)
}

#' @export
cloud_vsi.cloud_local <- function(storage, ...) {
  file.path(storage$root, remote_path(...))
}

#' @export
cloud_vsi.cloud_s3 <- function(storage, ...) {
  path <- remote_path(...)
  if (is_dir_path(path)) {
    stop("A GDAL path designates a file, not a folder: ", path,
         call. = FALSE)
  }
  if (storage$anonymous) {
    return(paste0("/vsicurl/", cloud_url(storage, path)))
  }
  endpoint <- storage$endpoint
  token <- Sys.getenv("AWS_SESSION_TOKEN")
  vars <- c(
    AWS_S3_ENDPOINT = sub("^https?://", "", endpoint),
    AWS_HTTPS = if (startsWith(endpoint, "https")) "YES" else "NO",
    AWS_VIRTUAL_HOSTING = "FALSE",
    AWS_REGION = storage$region,
    AWS_ACCESS_KEY_ID = Sys.getenv(storage$access_key_env),
    AWS_SECRET_ACCESS_KEY = Sys.getenv(storage$secret_access_key_env)
  )
  if (nzchar(token)) vars[["AWS_SESSION_TOKEN"]] <- token
  do.call(Sys.setenv, as.list(vars))
  paste0("/vsis3/", storage$bucket, "/", s3_key(storage, path))
}
