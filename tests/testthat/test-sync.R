# S3 source (https://s3.test) and Nextcloud destination (https://nc.test)
# served by the same mock.
local_fake_s3_and_nextcloud <- function(s3_files, nc_files = list(),
                                        env = parent.frame()) {
  s3 <- local_fake_s3(s3_files, env = env)
  nc <- local_fake_nextcloud(nc_files, env = env)
  httr2::local_mocked_responses(function(req) {
    if (grepl("^https://s3[.]test", req$url)) {
      fake_s3_answer(s3, req)
    } else {
      fake_nextcloud_answer(nc, req)
    }
  }, env = env)
  list(s3 = s3, nc = nc)
}

test_that("cloud_sync() copies a folder to a local storage, then skips it", {
  local_cache()
  server <- local_fake_s3(list("d/a.nc" = "11", "d/s/b.nc" = "222",
                               "other/c.nc" = "3"))
  from <- fake_s3_storage()
  to <- cloud_local(withr::local_tempdir())
  expect_message(res <- cloud_sync(from, to, "d/"), "2 of 2 files")
  expect_equal(res$status, c("copied", "copied"))
  expect_equal(readChar(file.path(to$root, "d/a.nc"), 9), "11")
  expect_equal(readChar(file.path(to$root, "d/s/b.nc"), 9), "222")
  # Nothing is left in the destination but the files.
  expect_equal(sort(list.files(to$root, recursive = TRUE, all.files = TRUE)),
               c("d/a.nc", "d/s/b.nc"))
  server$requests <- character()
  expect_message(res <- cloud_sync(from, to, "d/"), "0 of 2 files")
  expect_equal(res$status, c("skipped", "skipped"))
  expect_false(any(grepl("^GET d/", server$requests)))
  # A file with another size is copied again.
  writeLines("xxxx", file.path(to$root, "d/a.nc"))
  res <- suppressMessages(cloud_sync(from, to, "d/"))
  expect_equal(res$status, c("copied", "skipped"))
})

test_that("cloud_sync() copies from S3 to Nextcloud, with a pattern", {
  local_cache()
  both <- local_fake_s3_and_nextcloud(
    list("d/a.nc" = "11", "d/b.csv" = "22", "d/s/c.nc" = "333"),
    list("keep.txt" = "k")
  )
  from <- fake_s3_storage()
  to <- fake_storage(read_only = FALSE)
  res <- suppressMessages(
    cloud_sync(from, to, "d/", to_path = "copy/", pattern = "[.]nc$")
  )
  expect_equal(res$path, c("a.nc", "s/c.nc"))
  expect_equal(rawToChar(both$nc$files[["copy/a.nc"]]), "11")
  expect_equal(rawToChar(both$nc$files[["copy/s/c.nc"]]), "333")
  expect_false("copy/b.csv" %in% names(both$nc$files))
  # Nothing is deleted from the destination.
  expect_true("keep.txt" %in% names(both$nc$files))
  expect_length(list.files(tempdir(), pattern = "^kcloud-sync-"), 0)
})

test_that("cloud_sync() reports a failing file and copies the others", {
  local_cache()
  server <- local_fake_s3(list("d/a.nc" = "11", "d/b.nc" = "22",
                               "d/c.nc" = "33"))
  server$deny <- "d/b.nc"
  to <- cloud_local(withr::local_tempdir())
  expect_warning(
    res <- suppressMessages(cloud_sync(fake_s3_storage(), to, "d/")),
    "1 files failed"
  )
  expect_equal(res$status, c("copied", "failed", "copied"))
  expect_match(res$error[2], "403")
  server$deny <- character()
  res <- suppressMessages(cloud_sync(fake_s3_storage(), to, "d/"))
  expect_equal(res$status, c("skipped", "copied", "skipped"))
})

test_that("cloud_sync() with dry_run copies nothing", {
  local_cache()
  local_fake_s3(list("d/a.nc" = "11"))
  to <- cloud_local(withr::local_tempdir())
  res <- suppressMessages(cloud_sync(fake_s3_storage(), to, "d/",
                                     dry_run = TRUE))
  expect_equal(res$status, "todo")
  expect_length(list.files(to$root), 0)
  expect_error(cloud_sync(fake_s3_storage(), fake_s3_storage()), "read only")
})

test_that("cloud_sync() assembles big files in chunks", {
  local_cache()
  withr::local_options(kcloud.r.s3_chunk_size = 4)
  local_fake_s3(list("d/big.bin" = "0123456789"))
  to <- cloud_local(withr::local_tempdir())
  suppressMessages(cloud_sync(fake_s3_storage(), to, "d/"))
  expect_equal(readChar(file.path(to$root, "d/big.bin"), 99), "0123456789")
})
