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

test_that("terra reads a part of a remote DRIAS NetCDF through GDAL", {
  skip_if(!nzchar(Sys.getenv("KCLOUD_TEST_ONLINE")),
          "KCLOUD_TEST_ONLINE is not set")
  skip_if_not_installed("terra")
  skip_on_cran()
  st <- cloud_s3("meteofrance-drias",
                 endpoint = "https://object.files.data.gouv.fr",
                 anonymous = TRUE)
  f <- paste0(
    "EXPLORE2-Hydro/EXPLORE2-2024_AQUI-FR/CNRM-CERFACS-CNRM-CM5/",
    "CNRM-ALADIN63/historical/niveau_GrilleFrance_CNRM-CERFACS-CNRM-CM5_",
    "historical_r1i1p1_CNRM-ALADIN63_v2_ADAMONT-France_BRGM-AquiFR_month_",
    "19580801-20050731.nc"
  )
  r <- terra::rast(cloud_vsi(st, f))
  expect_gte(terra::nlyr(r), 564)
  # One time step is one chunk: a few MB are read, not the 2.6 GB.
  v <- terra::values(r[[1]])
  expect_length(v, 814 * 704)
})
