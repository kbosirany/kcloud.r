#' Read the configuration of a package
#'
#' Reads the default configuration of a package (its `inst/config.yml`) and
#' completes it with the user configuration, if any. Replaces
#' `fairify::loadConfig()`.
#'
#' @details
#' The files use the format of the package \pkg{config}: profiles (`default`
#' and others), active profile chosen by `R_CONFIG_ACTIVE`, `!expr` values.
#' The user file overrides the items of the default configuration.
#'
#' When `path_user` is a bare file name (the default), it is searched in the
#' working directory, then in its parents up to the root of the project
#' (first folder holding a `DESCRIPTION` file, a `.git` folder or an
#' `.Rproj` file).
#'
#' @param path_default Path of the default configuration, typically
#'   `system.file("config.yml", package = "mypkg")`. `NULL` to only read the
#'   user configuration.
#' @param path_user Path or name of the user configuration file. `NULL` or
#'   `""` to ignore it.
#' @param profile Configuration profile to use.
#'
#' @return The configuration, as a list.
#' @seealso [cloud_from_config()] to create a storage from it.
#' @export
#'
#' @examples
#' f <- tempfile(fileext = ".yml")
#' writeLines(c(
#'   "default:",
#'   "  storage:",
#'   "    data:",
#'   "      backend: local",
#'   "      root: ~/data"
#' ), f)
#' cfg <- load_config(f, path_user = NULL)
#' cfg$storage$data$root
load_config <- function(path_default = NULL,
                        path_user = "config.yml",
                        profile = Sys.getenv("R_CONFIG_ACTIVE", "default")) {
  check_string(profile, "profile")
  cfg <- list()
  if (!is.null(path_default)) {
    check_string(path_default, "path_default")
    if (!file.exists(path_default)) {
      stop("Configuration file not found: ", path_default, call. = FALSE)
    }
    cfg <- read_config_file(path_default, profile)
  }
  if (!is.null(path_user) && nzchar(path_user)) {
    path_user <- find_user_config(path_user)
    if (!is.null(path_user)) {
      message("kcloud.r - load_config - read user configuration from: ",
              path_user)
      cfg <- config::merge(cfg, read_config_file(path_user, profile))
    }
  }
  cfg
}

read_config_file <- function(file, profile) {
  cfg <- config::get(file = file, config = profile, use_parent = FALSE)
  attributes(cfg) <- attributes(cfg)["names"]
  cfg
}

# Searches a bare file name from the working directory up to the project
# root.
find_user_config <- function(file) {
  if (file != basename(file)) {
    return(if (file.exists(file)) file else NULL)
  }
  dir <- normalizePath(getwd(), winslash = "/")
  repeat {
    candidate <- file.path(dir, file)
    if (file.exists(candidate)) {
      return(candidate)
    }
    is_root <- file.exists(file.path(dir, "DESCRIPTION")) ||
      dir.exists(file.path(dir, ".git")) ||
      length(list.files(dir, pattern = "[.]Rproj$")) > 0
    parent <- dirname(dir)
    if (is_root || parent == dir) {
      return(NULL)
    }
    dir <- parent
  }
}

#' Create a storage from a configuration
#'
#' @details
#' The storages are described in the `storage` section of the configuration,
#' each one under a name:
#'
#' ```yaml
#' default:
#'   storage:
#'     data:
#'       backend: nextcloud
#'       url_env: MYPKG_DATA_URL
#'       url: "https://nextcloud.example.org/s/AbCdEf123"
#' local:
#'   storage:
#'     data:
#'       backend: local
#'       root: ~/data/mypkg
#' ```
#'
#' The other keys of a storage are the arguments of its constructor
#' ([cloud_local()], [cloud_nextcloud()]).
#'
#' Any key `x` can be given by an environment variable with a key `x_env`
#' holding the name of the variable: when the variable is set and not
#' empty, its value replaces `x`. Secrets (`password`) can only come from
#' environment variables: a configuration holding one is refused.
#'
#' The `data: {mode, local, remote}` section of fairify is still read, as
#' the storage `data`, with a message inviting to migrate.
#'
#' @param cfg The configuration: a list (as returned by [load_config()]),
#'   its `storage` section, the description of one storage, or the path of
#'   a YAML file.
#' @param name Name of the storage in the `storage` section.
#' @param profile Configuration profile, when `cfg` is a file.
#'
#' @return A storage object.
#' @family storages
#' @export
#'
#' @examples
#' cfg <- list(storage = list(data = list(backend = "local", root = "~/data")))
#' cloud_from_config(cfg)
cloud_from_config <- function(cfg, name = "data",
                              profile = Sys.getenv("R_CONFIG_ACTIVE",
                                                   "default")) {
  check_string(name, "name")
  if (is.character(cfg)) {
    check_string(cfg, "cfg")
    cfg <- read_config_file(cfg, profile)
  }
  if (!is.list(cfg)) {
    stop("`cfg` must be a list or the path of a YAML file.", call. = FALSE)
  }
  spec <- storage_spec(cfg, name)
  storage_from_spec(spec, name)
}

storage_spec <- function(cfg, name) {
  if (!is.null(cfg$backend)) {
    return(cfg)
  }
  if (!is.null(cfg$storage)) {
    cfg <- cfg$storage
  } else if (!is.null(cfg$data$mode)) {
    return(legacy_spec(cfg$data, name))
  }
  if (!is.null(cfg[[name]]$backend)) {
    return(cfg[[name]])
  }
  stop("No storage '", name, "' with a `backend` key in the configuration.",
       call. = FALSE)
}

legacy_state <- new.env(parent = emptyenv())

# Converts the `data` section of fairify.
legacy_spec <- function(data, name) {
  if (name != "data") {
    stop("The `data` section of fairify only describes the storage 'data'.",
         call. = FALSE)
  }
  if (is.null(legacy_state$warned)) {
    message("kcloud.r - cloud_from_config - the `data: {mode, local, ",
            "remote}` section of fairify is deprecated: describe the ",
            "storage in `storage: data: {backend, ...}`.")
    legacy_state$warned <- TRUE
  }
  switch(
    data$mode,
    local = list(backend = "local", root = data$local,
                 read_only = !isTRUE(data$write_results)),
    remote = list(backend = "nextcloud", url = data$remote),
    stop("`data$mode` must be 'local' or 'remote', not: ", data$mode,
         call. = FALSE)
  )
}

secret_keys <- c("password", "secret_access_key")

storage_from_spec <- function(spec, name) {
  secrets <- intersect(names(spec), secret_keys)
  if (length(secrets)) {
    stop("The storage '", name, "' has a secret in its configuration (",
         secrets[1], "): give it with an environment variable, `",
         secrets[1], "_env: VARIABLE_NAME`.", call. = FALSE)
  }
  backend <- spec$backend
  spec$backend <- NULL
  constructor <- switch(
    backend,
    local = cloud_local,
    nextcloud = cloud_nextcloud,
    s3 = ,
    rclone = stop("The ", backend, " backend is not implemented yet.",
                  call. = FALSE),
    stop("Unknown backend for the storage '", name, "': ", backend,
         call. = FALSE)
  )
  spec <- resolve_env_keys(spec, names(formals(constructor)))
  unknown <- setdiff(names(spec), names(formals(constructor)))
  if (length(unknown)) {
    stop("Unknown keys for the ", backend, " storage '", name, "': ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  do.call(constructor, spec)
}

# Replaces each key `x_env` by the key `x`, valued from the environment
# variable it names, when that variable is set. The `x_env` keys that are
# arguments of the constructor (password_env) are kept as they are.
resolve_env_keys <- function(spec, args) {
  keys <- grep("_env$", names(spec), value = TRUE)
  keys <- setdiff(keys, args)
  for (key in keys) {
    value <- Sys.getenv(spec[[key]])
    if (nzchar(value)) {
      spec[[sub("_env$", "", key)]] <- utils::type.convert(value,
                                                           as.is = TRUE)
    }
    spec[[key]] <- NULL
  }
  spec
}
