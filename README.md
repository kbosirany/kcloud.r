# kcloud.r

<!-- badges: start -->
[![R-CMD-check](https://github.com/kbosirany/kcloud.r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kbosirany/kcloud.r/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

One API to read and write data files wherever they are stored: on a local
disk or on Nextcloud (public link or account), and soon in S3 buckets and
rclone remotes.

- **One storage object, the same functions** for every backend:
  `cloud_path()`, `cloud_list()`, `cloud_exists()`, `cloud_upload()`,
  `cloud_download()`, `cloud_delete()`.
- **Persistent cache**: a remote file is downloaded once. After `max_age`
  days, its ETag is checked with one light request and the file is only
  downloaded again if it changed. The cache is bounded by `max_size` (the
  files read the least recently go first) and is safe with parallel
  workers.
- **Fast on large storages**: recursive listings and transfers run in
  parallel, listings can be cached, and `cloud_exists()` lists each folder
  once for many paths.
- **Configuration** in `config.yml`, with named storages, values from
  environment variables and secrets kept out of the files.

It replaces `getDataPath()`, `loadConfig()` and `cleanDataCache()` of
fairify.

## Installation

```r
# install.packages("remotes")
remotes::install_github("kbosirany/kcloud.r@dev")
```

## Usage

```r
library(kcloud.r)

st <- cloud_nextcloud("https://nextcloud.example.org/s/AbCdEf123")

cloud_list(st, "Raw/")
obs <- read.csv(cloud_path(st, "Raw/2023/obs.csv"))  # downloaded once
dir_2023 <- cloud_path(st, "Raw/2023/")               # a whole folder
cloud_exists(st, c("Raw/2023/obs.csv", "Raw/2024/obs.csv"))
cloud_cache_clean(st)
```

With a Nextcloud account, the password comes from an environment variable
(an application password, from the Nextcloud security settings):

```r
# KCLOUD_NEXTCLOUD_PASSWORD set in ~/.Renviron
st <- cloud_nextcloud("https://nextcloud.example.org", user = "me",
                      root = "Projects/outputs")
cloud_upload(st, "outputs/", "run_01/")
```

## Configuration

Describe the storages in the `config.yml` of a package or a project:

```yaml
default:
  storage:
    data:
      backend: nextcloud
      url_env: MYPKG_DATA_URL        # this variable wins when set
      url: "https://nextcloud.example.org/s/AbCdEf123"
      max_age: 7                     # days before checking the server
      max_size: 10                   # GB of cache

local:                               # R_CONFIG_ACTIVE=local
  storage:
    data:
      backend: local
      root: ~/data/mypkg
```

```r
cfg <- load_config(system.file("config.yml", package = "mypkg"))
st <- cloud_from_config(cfg, "data")
```

`load_config()` completes the default configuration with the
`config.yml` found in the working directory or its parents, up to the
project root. Any key `x` can be given by an environment variable with
`x_env`. Passwords can only come from environment variables.

## Migration from fairify

Keep the internal functions of the package, so that the code calling them
is unchanged:

```r
loadConfig <- function(path_user = "config.yml") {
  kcloud.r::load_config(
    system.file("config.yml", package = "mypkg"),
    path_user = path_user
  )
}

getDataPath <- function(path, ..., cfg = loadConfig()) {
  storage <- kcloud.r::cloud_from_config(cfg, "data")
  kcloud.r::cloud_path(storage, path, ...)
}
```

The `data: {mode, local, remote}` section of fairify is still read during
the migration.

## Options and environment variables

| Name                        | Use                                     |
|-----------------------------|-----------------------------------------|
| `KCLOUD_CACHE`              | Root of the cache (see `cloud_cache_dir()`) |
| `KCLOUD_NEXTCLOUD_USER`     | Default Nextcloud user                  |
| `KCLOUD_NEXTCLOUD_PASSWORD` | Default Nextcloud password              |
| `kcloud.r.workers` (option) | Simultaneous requests (4 for Nextcloud, 16 otherwise) |
| `KCLOUD_TEST_NEXTCLOUD_URL` | Public link for the integration tests   |
