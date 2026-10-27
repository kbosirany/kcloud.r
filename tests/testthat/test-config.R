write_yaml_lines <- function(dir, name, lines) {
  f <- file.path(dir, name)
  writeLines(lines, f)
  f
}

test_that("load_config() merges the user configuration found upward", {
  dir <- withr::local_tempdir()
  default <- write_yaml_lines(dir, "default.yml", c(
    "default:",
    "  storage:",
    "    data:",
    "      backend: nextcloud",
    "      url: https://nc.test/s/TOKEN",
    "  crop:",
    "    tbase: 6",
    "local:",
    "  crop:",
    "    tbase: 8"
  ))
  project <- file.path(dir, "project")
  dir.create(file.path(project, "reports"), recursive = TRUE)
  file.create(file.path(project, "DESCRIPTION"))
  write_yaml_lines(project, "config.yml", c(
    "default:",
    "  storage:",
    "    data:",
    "      backend: local",
    "      root: ~/data"
  ))
  withr::local_dir(file.path(project, "reports"))

  expect_message(cfg <- load_config(default), "user configuration")
  expect_equal(cfg$storage$data$backend, "local")
  expect_equal(cfg$storage$data$root, "~/data")
  expect_equal(cfg$crop$tbase, 6)

  cfg <- load_config(default, path_user = NULL, profile = "local")
  expect_equal(cfg$storage$data$backend, "nextcloud")
  expect_equal(cfg$crop$tbase, 8)

  # The search stops at the project root
  withr::local_dir(dir)
  expect_silent(cfg <- load_config(default))
  expect_equal(cfg$storage$data$backend, "nextcloud")
  expect_error(load_config(file.path(dir, "nope.yml")), "not found")
})

test_that("cloud_from_config() creates a named storage", {
  cfg <- list(storage = list(
    data = list(backend = "local", root = "~/data"),
    inputs = list(backend = "nextcloud", url = "https://nc.test/s/TOKEN",
                  max_age = 1)
  ))
  expect_s3_class(cloud_from_config(cfg), "cloud_local")
  st <- cloud_from_config(cfg, "inputs")
  expect_s3_class(st, "cloud_nextcloud")
  expect_equal(st$max_age, 1)
  expect_s3_class(cloud_from_config(cfg$storage, "inputs"), "cloud_nextcloud")
  expect_s3_class(cloud_from_config(cfg$storage$data), "cloud_local")
  expect_error(cloud_from_config(cfg, "outputs"), "No storage 'outputs'")

  f <- write_yaml_lines(withr::local_tempdir(), "config.yml", c(
    "default:",
    "  storage:",
    "    data:",
    "      backend: local",
    "      root: /data"
  ))
  expect_equal(cloud_from_config(f)$root, "/data")
})

test_that("`x_env` keys are read from the environment", {
  cfg <- list(storage = list(data = list(
    backend = "nextcloud",
    url = "https://nc.test/s/DEFAULT",
    url_env = "MYPKG_DATA_URL",
    max_age_env = "MYPKG_MAX_AGE"
  )))
  withr::local_envvar(MYPKG_DATA_URL = "", MYPKG_MAX_AGE = "")
  st <- cloud_from_config(cfg)
  expect_match(st$url_dav, "DEFAULT$")
  expect_equal(st$max_age, 7)

  withr::local_envvar(MYPKG_DATA_URL = "https://nc.test/s/FROMENV",
                      MYPKG_MAX_AGE = "0")
  st <- cloud_from_config(cfg)
  expect_match(st$url_dav, "FROMENV$")
  expect_equal(st$max_age, 0)
})

test_that("secrets, unknown keys and backends are refused", {
  cfg <- list(backend = "nextcloud", url = "https://nc.test", user = "me",
              password = "s3cr3t")
  expect_error(cloud_from_config(cfg), "password_env")

  withr::local_envvar(MY_PASSWORD = "s3cr3t")
  cfg$password <- NULL
  cfg$password_env <- "MY_PASSWORD"
  expect_s3_class(cloud_from_config(cfg), "cloud_nextcloud")

  expect_error(cloud_from_config(list(backend = "local", root = "/d",
                                      typo = 1)), "Unknown keys.*typo")
  expect_error(cloud_from_config(list(backend = "s3")), "not implemented")
  expect_error(cloud_from_config(list(backend = "ftp")), "Unknown backend")
})

test_that("the `data` section of fairify is still read", {
  legacy_state$warned <- NULL
  cfg <- list(data = list(mode = "remote", local = "/d",
                          remote = "https://nc.test/s/TOKEN",
                          write_results = FALSE))
  expect_message(st <- cloud_from_config(cfg), "deprecated")
  expect_s3_class(st, "cloud_nextcloud")
  expect_silent(cloud_from_config(cfg))

  cfg$data$mode <- "local"
  st <- cloud_from_config(cfg)
  expect_equal(st$root, "/d")
  expect_true(st$read_only)
  expect_error(cloud_from_config(cfg, "outputs"), "only describes")
  cfg$data$mode <- "cloud"
  expect_error(cloud_from_config(cfg), "'local' or 'remote'")
})
