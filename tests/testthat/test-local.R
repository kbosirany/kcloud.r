local_storage <- function(env = parent.frame(), ...) {
  root <- withr::local_tempdir(.local_envir = env)
  cloud_local(root, ...)
}

write_local <- function(dir, name, content = "x") {
  f <- file.path(dir, name)
  dir.create(dirname(f), showWarnings = FALSE, recursive = TRUE)
  writeLines(content, f)
  f
}

test_that("cloud_local() expands the root and prints", {
  st <- cloud_local("~/data/", name = "my data")
  expect_s3_class(st, c("cloud_local", "cloud_storage"))
  expect_equal(st$root, path.expand("~/data"))
  expect_equal(st$name, "my_data")
  expect_output(print(st), "local: my_data")
})

test_that("cloud_path() gives the path in the root without copying", {
  st <- local_storage()
  expect_equal(cloud_path(st, "Raw", "obs.csv"),
               file.path(st$root, "Raw/obs.csv"))
  expect_equal(cloud_path(st, "Raw/"), file.path(st$root, "Raw"))
  expect_equal(cloud_path(st), st$root)
  expect_false(file.exists(cloud_path(st, "Raw/obs.csv")))
  expect_error(cloud_url(st, "a.csv"), "no URL")
  expect_error(cloud_cache_dir(st), "no cache")
})

test_that("upload, list, exists, download and delete work", {
  st <- local_storage()
  src <- withr::local_tempdir()
  write_local(src, "a.csv", "a")
  write_local(src, "sub/b.csv", "b")

  uploaded <- cloud_upload(st, src, "data/")
  expect_setequal(uploaded, c("data/a.csv", "data/sub/b.csv"))

  listing <- cloud_list(st, "data")
  expect_equal(listing$path, c("data/a.csv", "data/sub/"))
  expect_equal(listing$is_dir, c(FALSE, TRUE))
  expect_named(listing, c("path", "is_dir", "size", "modified", "etag"))
  expect_s3_class(listing$modified, "POSIXct")

  listing <- cloud_list(st, recursive = TRUE, pattern = "[.]csv$")
  expect_equal(listing$path, c("data/a.csv", "data/sub/b.csv"))

  expect_equal(
    cloud_exists(st, c("data/a.csv", "data/sub/", "data/sub", "data/a.csv/",
                       "nope.csv", "")),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )

  dest <- file.path(withr::local_tempdir(), "copy")
  cloud_download(st, "data/", dest)
  expect_setequal(list.files(dest, recursive = TRUE),
                  c("a.csv", "sub/b.csv"))
  expect_error(cloud_download(st, "data/", dest), "Already exists")
  f <- file.path(dirname(dest), "b.csv")
  cloud_download(st, "data/sub/b.csv", f)
  expect_equal(readLines(f), "b")

  expect_error(cloud_upload(st, src, "data/"), "already on the storage")
  write_local(src, "a.csv", "a2")
  cloud_upload(st, file.path(src, "a.csv"), "data/", overwrite = TRUE)
  expect_equal(readLines(cloud_path(st, "data/a.csv")), "a2")

  cloud_delete(st, "data/sub/")
  expect_false(cloud_exists(st, "data/sub/"))
  expect_error(cloud_delete(st, ""), "root")
})

test_that("cloud_exists() lists the folders for many paths", {
  st <- local_storage()
  for (i in 1:30) write_local(st$root, sprintf("d/f%02d.csv", i))
  paths <- c(sprintf("d/f%02d.csv", 1:30), "d/nope.csv", "e/nope.csv")
  expect_equal(cloud_exists(st, paths), c(rep(TRUE, 30), FALSE, FALSE))
})

test_that("a read-only storage refuses to write", {
  st <- local_storage(read_only = TRUE)
  f <- write_local(withr::local_tempdir(), "a.csv")
  expect_error(cloud_upload(st, f, "a.csv"), "read only")
  expect_error(cloud_delete(st, "a.csv"), "read only")
})

test_that("missing paths give clear errors", {
  st <- local_storage()
  expect_error(cloud_list(st, "nope/"), "folder not found")
  expect_error(cloud_download(st, "nope.csv", tempfile()), "not found")
  expect_error(cloud_upload(st, tempfile(), "a.csv"), "Not found")
  expect_error(cloud_path("not a storage", "a.csv"), "must be a storage")
})
