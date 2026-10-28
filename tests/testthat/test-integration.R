# Tests on a real Nextcloud public link, only run when the environment
# variable KCLOUD_TEST_NEXTCLOUD_URL holds such a link (read only tests).

test_that("a real Nextcloud public link can be listed and read", {
  url <- Sys.getenv("KCLOUD_TEST_NEXTCLOUD_URL")
  skip_if(!nzchar(url), "KCLOUD_TEST_NEXTCLOUD_URL is not set")
  skip_on_cran()
  local_cache()
  st <- cloud_nextcloud(url)
  listing <- cloud_list(st)
  expect_gt(nrow(listing), 0)
  files <- cloud_list(st, listing$path[listing$is_dir][1])
  files <- files[!files$is_dir, ]
  skip_if(nrow(files) == 0, "no file in the first folder")
  f <- files[which.min(files$size), ]
  p <- suppressMessages(cloud_path(st, f$path))
  expect_equal(file.size(p), f$size)
  expect_true(cloud_exists(st, f$path))
})
