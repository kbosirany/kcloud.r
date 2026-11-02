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

# Public Meteo-France bucket (DRIAS, EXPLORE2), only run when the environment
# variable KCLOUD_TEST_ONLINE is set (read only tests).
test_that("the public DRIAS S3 bucket can be listed and read", {
  skip_if(!nzchar(Sys.getenv("KCLOUD_TEST_ONLINE")),
          "KCLOUD_TEST_ONLINE is not set")
  skip_on_cran()
  local_cache()
  st <- cloud_s3("meteofrance-drias",
                 endpoint = "https://object.files.data.gouv.fr",
                 anonymous = TRUE)
  expect_true("EXPLORE2-Hydro/" %in% cloud_list(st)$path)
  files <- cloud_list(st, "EXPLORE2-Hydro/EXPLORE2-2024_SIM2/",
                      recursive = TRUE)
  files <- files[!files$is_dir & files$size > 0, ]
  expect_gt(nrow(files), 0)
  f <- files[which.min(files$size), ]
  p <- suppressMessages(cloud_path(st, f$path))
  expect_equal(file.size(p), f$size)
  expect_true(cloud_exists(st, f$path))
})
