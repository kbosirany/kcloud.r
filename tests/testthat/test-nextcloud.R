test_that("cloud_nextcloud() builds the WebDAV URL of a public link", {
  st <- cloud_nextcloud("https://nc.test/s/AbC123/")
  expect_s3_class(st, c("cloud_nextcloud", "cloud_storage"))
  expect_equal(st$url_dav, "https://nc.test/public.php/dav/files/AbC123")
  expect_equal(st$name, "nc.test_AbC123")
  expect_true(st$read_only)
  expect_null(st$auth)
  expect_equal(cloud_url(st, "Raw/a b.csv"),
               paste0(st$url_dav, "/Raw/a%20b.csv"))
  expect_equal(cloud_url(st, "Raw/"), paste0(st$url_dav, "/Raw/?accept=zip"))

  st <- cloud_nextcloud("https://nc.test/nextcloud/s/AbC123", root = "Raw")
  expect_equal(st$url_dav,
               "https://nc.test/nextcloud/public.php/dav/files/AbC123/Raw")
  expect_equal(st$name, "nc.test_AbC123_Raw")
})

test_that("cloud_nextcloud() uses an account without showing its password", {
  withr::local_envvar(KCLOUD_NEXTCLOUD_USER = "", MY_PASSWORD = "")
  expect_error(cloud_nextcloud("https://nc.test"), "needs `user`")
  expect_error(
    cloud_nextcloud("https://nc.test", user = "me",
                    password_env = "MY_PASSWORD"),
    "MY_PASSWORD"
  )
  withr::local_envvar(KCLOUD_NEXTCLOUD_USER = "me", MY_PASSWORD = "s3cr3t")
  st <- cloud_nextcloud("https://nc.test", password_env = "MY_PASSWORD",
                        root = "Projects/data")
  expect_equal(st$url_dav,
               "https://nc.test/remote.php/dav/files/me/Projects/data")
  expect_false(st$read_only)
  expect_false(any(grepl("s3cr3t", capture.output(print(st), str(st)))))
  req <- st$auth(httr2::request("https://nc.test"))
  headers <- httr2::req_get_headers(req, redacted = "reveal")
  expect_match(headers$Authorization, "^Basic ")
  expect_error(cloud_nextcloud("nc.test/s/AbC"), "http")
})

test_that("cloud_list() parses the WebDAV listing, recursively", {
  server <- local_fake_nextcloud(list(
    "Raw/2023/obs.csv" = "1,2",
    "Raw/2023/meteo.csv" = "3",
    "Raw/readme.txt" = "hello",
    "Params/p.csv" = "p"
  ))
  st <- fake_storage()
  listing <- cloud_list(st)
  expect_equal(listing$path, c("Params/", "Raw/"))
  expect_true(all(listing$is_dir))

  listing <- cloud_list(st, "Raw", recursive = TRUE)
  expect_equal(listing$path, c("Raw/2023/", "Raw/2023/meteo.csv",
                               "Raw/2023/obs.csv", "Raw/readme.txt"))
  expect_equal(listing$size, c(4, 1, 3, 5))
  expect_equal(listing$etag[4], server$etags[["Raw/readme.txt"]])
  expect_equal(format(listing$modified[1], tz = "UTC"),
               "2026-10-21 08:36:27")

  expect_equal(nrow(cloud_list(st, recursive = TRUE, pattern = "csv$")), 3)
  expect_error(cloud_list(st, "nope/"), "HTTP 404 not found")
})

test_that("cloud_list() caches the listing with max_age", {
  local_cache()
  server <- local_fake_nextcloud(list("a.csv" = "a"))
  st <- fake_storage()
  cloud_list(st, max_age = 1)
  n <- length(server$requests)
  fake_set_file(server, "b.csv", "b")
  expect_equal(cloud_list(st, max_age = 1)$path, "a.csv")
  expect_length(server$requests, n)
  expect_equal(cloud_list(st)$path, c("a.csv", "b.csv"))
})

test_that("cloud_path() downloads once, then checks the ETag", {
  local_cache()
  server <- local_fake_nextcloud(list("Raw/obs.csv" = "v1"))
  st <- fake_storage()

  expect_message(p <- cloud_path(st, "Raw", "obs.csv"), "download Raw/obs.csv")
  expect_equal(readLines(p, warn = FALSE), "v1")
  expect_equal(p, file.path(cloud_cache_dir(st), "Raw/obs.csv"))
  expect_true(file.exists(paste0(p, ".kcloud")))

  # Fresh: no request
  n <- length(server$requests)
  expect_silent(cloud_path(st, "Raw/obs.csv"))
  expect_length(server$requests, n)

  # Stale but unchanged: one PROPFIND, no download
  expect_silent(cloud_path(st, "Raw/obs.csv", max_age = 0))
  expect_equal(server$requests[-seq_len(n)], "PROPFIND Raw/obs.csv")

  # Changed on the server: downloaded again
  fake_set_file(server, "Raw/obs.csv", "v2")
  expect_message(p <- cloud_path(st, "Raw/obs.csv", max_age = 0), "download")
  expect_equal(readLines(p, warn = FALSE), "v2")
})

test_that("cloud_path() uses the cached copy when the server is offline", {
  local_cache()
  server <- local_fake_nextcloud(list("obs.csv" = "v1"))
  st <- fake_storage(max_age = 0)
  p <- suppressMessages(cloud_path(st, "obs.csv"))
  server$offline <- TRUE
  expect_warning(p2 <- cloud_path(st, "obs.csv"), "cached copy is used")
  expect_equal(p2, p)
  expect_error(suppressMessages(cloud_path(st, "other.csv")))
})

test_that("cloud_path() fails on a missing file and leaves no file", {
  local_cache()
  local_fake_nextcloud(list("obs.csv" = "v1"))
  st <- fake_storage()
  expect_error(suppressMessages(cloud_path(st, "nope.csv")), "HTTP 404")
  expect_equal(list.files(cloud_cache_dir(st), all.files = TRUE,
                          no.. = TRUE), character())
  expect_error(cloud_path(st, ""), "whole storage")
})

test_that("cloud_path() downloads a folder as a ZIP", {
  skip_if_no_zip()
  local_cache()
  server <- local_fake_nextcloud(list(
    "Raw/2023/obs.csv" = "1", "Raw/2023/sub/x.csv" = "2"
  ))
  st <- fake_storage()
  d <- suppressMessages(cloud_path(st, "Raw/2023/"))
  expect_true(dir.exists(d))
  expect_setequal(list.files(d, recursive = TRUE), c("obs.csv", "sub/x.csv"))

  n <- length(server$requests)
  expect_silent(cloud_path(st, "Raw/2023/", max_age = 0))
  expect_equal(server$requests[-seq_len(n)], "PROPFIND Raw/2023")

  fake_set_file(server, "Raw/2023/new.csv", "3")
  d <- suppressMessages(cloud_path(st, "Raw/2023/", max_age = 0))
  expect_setequal(list.files(d, recursive = TRUE),
                  c("new.csv", "obs.csv", "sub/x.csv"))

  dest <- file.path(withr::local_tempdir(), "copy")
  cloud_download(st, "Raw/2023/", dest)
  expect_setequal(list.files(dest, recursive = TRUE),
                  c("new.csv", "obs.csv", "sub/x.csv"))
})

test_that("cloud_exists() is vectorized, with stat or listing", {
  server <- local_fake_nextcloud(stats::setNames(
    as.list(rep("x", 30)), sprintf("d/f%02d.csv", 1:30)
  ))
  st <- fake_storage()
  expect_equal(cloud_exists(st, c("d/", "d/f01.csv", "d/f01.csv/", "nope")),
               c(TRUE, TRUE, FALSE, FALSE))
  n <- length(server$requests)
  paths <- c(sprintf("d/f%02d.csv", 1:30), "d/nope.csv", "e/nope.csv")
  expect_equal(cloud_exists(st, paths), c(rep(TRUE, 30), FALSE, FALSE))
  # One listing per folder instead of 32 requests
  expect_setequal(server$requests[-seq_len(n)], c("PROPFIND d", "PROPFIND e"))
})

test_that("cloud_upload() creates the folders and invalidates the cache", {
  local_cache()
  server <- local_fake_nextcloud(list("out/old.csv" = "old"))
  st <- fake_storage(read_only = FALSE)
  src <- withr::local_tempdir()
  dir.create(file.path(src, "sub"))
  writeLines("a", file.path(src, "a.csv"))
  writeLines("b", file.path(src, "sub", "b.csv"))

  cached <- suppressMessages(cloud_path(st, "out/old.csv"))
  cloud_upload(st, src, "out/run 1/")
  # writeLines() ends the lines with \r\n on Windows
  uploaded <- rawToChar(server$files[["out/run 1/sub/b.csv"]])
  expect_equal(sub("\r?\n$", "", uploaded), "b")
  expect_true(all(c("run 1", "run 1/sub") %in% sub("^out/", "",
                                                   server$dirs)))
  mkcol <- grep("^MKCOL", server$requests, value = TRUE)
  expect_equal(mkcol, c("MKCOL out", "MKCOL out/run 1", "MKCOL out/run 1/sub"))

  expect_error(cloud_upload(st, src, "out/run 1/"), "already on the storage")

  f <- file.path(src, "a.csv")
  writeLines("new", f)
  cloud_upload(st, f, "out/old.csv", overwrite = TRUE)
  expect_false(file.exists(cached))
  expect_equal(readLines(suppressMessages(cloud_path(st, "out/old.csv"))),
               "new")

  cloud_delete(st, "out/run 1/")
  expect_false(cloud_exists(st, "out/run 1/"))
})

test_that("a public link is read only by default", {
  local_fake_nextcloud()
  st <- fake_storage()
  f <- withr::local_tempfile(lines = "a")
  expect_error(cloud_upload(st, f, "a.csv"), "read only")
})
