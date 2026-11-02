test_that("cloud_s3() builds a path-style storage", {
  withr::local_envvar(AWS_DEFAULT_REGION = "")
  st <- cloud_s3("bkt", prefix = "a/b", endpoint = "https://s3.test/",
                 anonymous = TRUE)
  expect_s3_class(st, c("cloud_s3", "cloud_storage"))
  expect_equal(st$prefix, "a/b/")
  expect_true(st$read_only)
  expect_equal(st$name, "bkt_a_b")
  expect_equal(cloud_url(st, "x y.nc"), "https://s3.test/bkt/a/b/x%20y.nc")
  st <- cloud_s3("bkt", anonymous = TRUE)
  expect_equal(st$endpoint, "https://s3.us-east-1.amazonaws.com")
  withr::local_envvar(AWS_DEFAULT_REGION = "eu-west-3")
  expect_equal(cloud_s3("bkt", anonymous = TRUE)$endpoint,
               "https://s3.eu-west-3.amazonaws.com")
  expect_error(cloud_s3("bkt", endpoint = "s3.test", anonymous = TRUE),
               "http")
})

test_that("cloud_s3() signs the requests without showing the secret", {
  withr::local_envvar(AWS_ACCESS_KEY_ID = "", AWS_SECRET_ACCESS_KEY = "")
  expect_error(cloud_s3("bkt"), "AWS_ACCESS_KEY_ID")
  withr::local_envvar(AWS_ACCESS_KEY_ID = "AKIA",
                      AWS_SECRET_ACCESS_KEY = "s3cr3t")
  st <- cloud_s3("bkt", endpoint = s3_test_endpoint)
  expect_false(st$read_only)
  expect_false(any(grepl("s3cr3t", capture.output(print(st), str(st)))))
  req <- s3_request(st, "a.nc")
  signed <- httr2::req_dry_run(req, quiet = TRUE, redact_headers = FALSE)
  expect_match(signed$headers$authorization,
               "^AWS4-HMAC-SHA256 Credential=AKIA/")
  anon <- httr2::req_dry_run(s3_request(fake_s3_storage(), "a.nc"),
                             quiet = TRUE)
  expect_null(anon$headers$authorization)
})

test_that("cloud_list() pages through a listing and sets folders", {
  server <- local_fake_s3(list(
    "p/a.nc" = "1", "p/b.nc" = "22", "p/sub/c.nc" = "333", "other/d.nc" = "4"
  ))
  withr::local_options(kcloud.r.s3_page_size = 2)
  st <- fake_s3_storage(prefix = "p")
  ls <- cloud_list(st)
  expect_equal(ls$path, c("a.nc", "b.nc", "sub/"))
  expect_equal(ls$is_dir, c(FALSE, FALSE, TRUE))
  expect_equal(ls$size, c(1, 2, NA))
  expect_equal(ls$etag[1:2], c("e1", "e2"))
  expect_s3_class(ls$modified, "POSIXct")
  # Two keys per page: the listing took two requests.
  expect_equal(sum(server$requests == "GET "), 2)
  expect_equal(cloud_list(st, recursive = TRUE)$path,
               c("a.nc", "b.nc", "sub/", "sub/c.nc"))
  expect_equal(cloud_list(st, "sub/")$path, "sub/c.nc")
  expect_equal(cloud_list(st, recursive = TRUE, pattern = "c[.]nc")$path,
               "sub/c.nc")
  expect_error(cloud_list(st, "nope/"), "not found")
})

test_that("cloud_list() lists the subfolders in parallel, level by level", {
  keys <- paste0(outer(paste0("m", 1:3), paste0("s", 1:2), paste, sep = "/"),
                 "/f.nc")
  server <- local_fake_s3(stats::setNames(as.list(rep("x", 6)), keys))
  st <- fake_s3_storage()
  ls <- cloud_list(st, recursive = TRUE)
  expect_equal(sum(!ls$is_dir), 6)
  expect_equal(sum(ls$is_dir), 9)
  # root + 3 models + 6 scenarios: one request per folder, no more.
  expect_equal(sum(server$requests == "GET "), 10)
})

test_that("cloud_exists() tells files and folders apart", {
  server <- local_fake_s3(list("d/a.nc" = "1"))
  st <- fake_s3_storage()
  expect_equal(cloud_exists(st, c("d/a.nc", "d/", "d", "d/b.nc", "x/")),
               c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(cloud_exists(st, ""), TRUE)
})

test_that("cloud_path() downloads once, then checks the ETag", {
  local_cache()
  server <- local_fake_s3(list("d/a.csv" = "x,y\n1,2\n"))
  st <- fake_s3_storage(max_age = 0)
  expect_message(path <- cloud_path(st, "d", "a.csv"), "download")
  expect_equal(readLines(path), c("x,y", "1,2"))
  expect_silent(cloud_path(st, "d/a.csv"))
  expect_equal(sum(server$requests == "GET d/a.csv"), 1)
  server$files[["d/a.csv"]] <- charToRaw("x,y\n3,4\n")
  server$etags[["d/a.csv"]] <- "new"
  expect_message(path <- cloud_path(st, "d/a.csv"), "download")
  expect_equal(readLines(path)[2], "3,4")
  expect_error(cloud_path(st, "d/zz.csv"), "404|not found")
})

test_that("cloud_download() fetches a folder", {
  server <- local_fake_s3(list("d/a.csv" = "1", "d/s/b.csv" = "2"))
  st <- fake_s3_storage()
  dest <- file.path(withr::local_tempdir(), "out")
  cloud_download(st, "d/", dest)
  expect_equal(sort(list.files(dest, recursive = TRUE)),
               c("a.csv", "s/b.csv"))
})

test_that("cloud_upload() and cloud_delete() work on a writable storage", {
  server <- local_fake_s3(list("d/old.csv" = "0", "d/s/b.csv" = "2"))
  st <- fake_s3_storage(read_only = FALSE)
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines("x", f)
  cloud_upload(st, f, "d/new.csv")
  expect_equal(rawToChar(server$files[["d/new.csv"]]), "x\n")
  expect_error(cloud_upload(st, f, "d/new.csv"), "already")
  cloud_delete(st, "d/new.csv")
  expect_false("d/new.csv" %in% names(server$files))
  cloud_delete(st, "d")
  expect_length(server$files, 0)
  expect_error(cloud_upload(fake_s3_storage(), f, "d/"), "read only")
})

test_that("cloud_from_config() builds an S3 storage", {
  cfg <- list(backend = "s3", bucket = "bkt", endpoint = s3_test_endpoint,
              anonymous = TRUE)
  expect_s3_class(cloud_from_config(cfg), "cloud_s3")
  expect_error(cloud_from_config(c(cfg, secret_access_key = "x")), "secret")
})

test_that("a recursive listing switches to flat listings, same result", {
  keys <- c(paste0("t", 1:4, "/a/b/c/f.nc"), "t1/a/g.nc", "r.nc")
  server <- local_fake_s3(stats::setNames(as.list(rep("x", 6)), keys))
  st <- fake_s3_storage()
  withr::local_options(kcloud.r.workers = 16)
  deep <- cloud_list(st, recursive = TRUE)
  n_deep <- sum(server$requests == "GET ")
  # 4 folders keep one worker busy at a time: the folders are listed flat.
  withr::local_options(kcloud.r.workers = 1)
  server$requests <- character()
  flat <- cloud_list(st, recursive = TRUE)
  expect_equal(flat, deep)
  expect_equal(sum(server$requests == "GET "), 5)
  expect_lt(sum(server$requests == "GET "), n_deep)
  expect_true(all(c("t1/", "t1/a/", "t1/a/b/c/") %in% flat$path))
  expect_equal(sum(flat$is_dir), 16)
  expect_equal(cloud_list(st, "t1/", recursive = TRUE)$path,
               c("t1/a/", "t1/a/b/", "t1/a/b/c/", "t1/a/b/c/f.nc",
                 "t1/a/g.nc"))
})

test_that("a big file is downloaded in parallel chunks", {
  local_cache()
  withr::local_options(kcloud.r.s3_chunk_size = 4)
  server <- local_fake_s3(list("big.bin" = "0123456789", "small" = "abc",
                               "empty" = ""))
  st <- fake_s3_storage()
  expect_equal(readChar(suppressMessages(cloud_path(st, "big.bin")), 99),
               "0123456789")
  expect_equal(server$ranges, c("bytes=0-3", "bytes=4-7", "bytes=8-9"))
  # The chunks are removed once appended.
  expect_length(list.files(file.path(cloud_cache_dir(st), ".parts")), 0)
  # A file that fits in a chunk, or that is empty, needs a single request.
  server$ranges <- character()
  expect_equal(readChar(suppressMessages(cloud_path(st, "small")), 9), "abc")
  expect_equal(server$ranges, "bytes=0-3")
  expect_equal(file.size(suppressMessages(cloud_path(st, "empty"))), 0)
})

test_that("an interrupted download resumes where it stopped", {
  local_cache()
  withr::local_options(kcloud.r.s3_chunk_size = 4)
  server <- local_fake_s3(list("big.bin" = "0123456789"))
  st <- fake_s3_storage()
  server$refuse <- "bytes=8-9"
  expect_error(suppressMessages(cloud_path(st, "big.bin")), "403")
  expect_false(file.exists(file.path(cloud_cache_dir(st), "big.bin")))
  server$refuse <- character()
  server$ranges <- character()
  expect_equal(readChar(suppressMessages(cloud_path(st, "big.bin")), 99),
               "0123456789")
  # Only the first chunk and the missing one are requested again.
  expect_equal(server$ranges, c("bytes=0-3", "bytes=8-9"))
})

test_that("a file that changes during its download is not assembled", {
  local_cache()
  withr::local_options(kcloud.r.s3_chunk_size = 4)
  server <- local_fake_s3(list("big.bin" = "0123456789"))
  st <- fake_s3_storage()
  # The first chunk is read, then the file changes.
  trace_get <- function(req) {
    answer <- fake_s3_answer(server, req)
    if (identical(req$headers$Range, "bytes=0-3")) {
      server$etags[["big.bin"]] <- "changed"
    }
    answer
  }
  httr2::local_mocked_responses(trace_get)
  expect_error(suppressMessages(cloud_path(st, "big.bin")), "changed")
  expect_false(dir.exists(file.path(cloud_cache_dir(st), ".parts",
                                    "e1-10")))
})

test_that("a file bigger than the cache limit is refused", {
  local_cache()
  withr::local_options(kcloud.r.s3_chunk_size = 4)
  server <- local_fake_s3(list("big.bin" = "0123456789"))
  st <- fake_s3_storage(max_size = 1e-9)
  expect_error(suppressMessages(cloud_path(st, "big.bin")), "max_size")
  # Refused after the first chunk: nothing else was requested.
  expect_equal(server$ranges, "bytes=0-3")
})

test_that("cloud_vsi() gives the path GDAL reads with range requests", {
  st <- cloud_s3("bkt", prefix = "p", endpoint = "https://s3.test",
                 anonymous = TRUE)
  expect_equal(cloud_vsi(st, "a", "x y.nc"),
               "/vsicurl/https://s3.test/bkt/p/a/x%20y.nc")
  expect_error(cloud_vsi(st, "a/"), "file")

  withr::local_envvar(AWS_ACCESS_KEY_ID = "AKIA", AWS_SECRET_ACCESS_KEY = "k",
                      AWS_S3_ENDPOINT = NA, AWS_VIRTUAL_HOSTING = NA)
  st <- cloud_s3("bkt", prefix = "p", endpoint = "http://minio.test:9000")
  expect_equal(cloud_vsi(st, "a.nc"), "/vsis3/bkt/p/a.nc")
  expect_equal(Sys.getenv("AWS_S3_ENDPOINT"), "minio.test:9000")
  expect_equal(Sys.getenv("AWS_HTTPS"), "NO")
  expect_equal(Sys.getenv("AWS_VIRTUAL_HOSTING"), "FALSE")

  loc <- cloud_local(withr::local_tempdir())
  expect_equal(cloud_vsi(loc, "a", "b.nc"), file.path(loc$root, "a/b.nc"))
  expect_error(cloud_vsi(cloud_nextcloud("https://nc.test/s/T"), "a"),
               "no GDAL path")
})
