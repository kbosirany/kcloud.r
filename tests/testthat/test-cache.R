test_that("cloud_cache_dir() follows the cache precedence", {
  withr::local_envvar(KCLOUD_CACHE = "")
  st <- cloud_nextcloud(nc_test_url)
  expect_equal(cloud_cache_dir(st),
               file.path(tools::R_user_dir("kcloud.r", "cache"), st$name))
  withr::local_envvar(KCLOUD_CACHE = "/env/cache")
  expect_equal(cloud_cache_dir(st), file.path("/env/cache", st$name))
  st <- cloud_nextcloud(nc_test_url, cache = "/arg/cache")
  expect_equal(cloud_cache_dir(st), file.path("/arg/cache", st$name))
})

test_that("cloud_cache_clean() removes all or some paths", {
  local_cache()
  local_fake_nextcloud(list("a.csv" = "a", "b/c.csv" = "c"))
  st <- fake_storage()
  pa <- suppressMessages(cloud_path(st, "a.csv"))
  pc <- suppressMessages(cloud_path(st, "b/c.csv"))
  cloud_cache_clean(st, "a.csv")
  expect_false(file.exists(pa))
  expect_false(file.exists(paste0(pa, ".kcloud")))
  expect_true(file.exists(pc))
  cloud_cache_clean(st)
  expect_false(dir.exists(cloud_cache_dir(st)))
  expect_invisible(cloud_cache_clean(cloud_local(tempdir())))
})

test_that("the files read the least recently are evicted beyond max_size", {
  local_cache()
  content <- strrep("x", 400)
  local_fake_nextcloud(list("a.csv" = content, "b.csv" = content,
                            "c.csv" = content))
  # 3 x 400 bytes and their metadata exceed 1300 bytes: removing one file
  # is enough to go under 80 %
  st <- fake_storage(max_size = 1300 / 1024^3)
  pa <- suppressMessages(cloud_path(st, "a.csv"))
  pb <- suppressMessages(cloud_path(st, "b.csv"))
  # a.csv is read again: b.csv becomes the least recently read
  meta <- meta_read(pb)
  meta$accessed <- 1
  meta_write(pb, meta)
  pc <- suppressMessages(cloud_path(st, "c.csv"))
  expect_true(file.exists(pa))
  expect_false(file.exists(pb))
  expect_false(file.exists(paste0(pb, ".kcloud")))
  expect_true(file.exists(pc))
})

test_that("the metadata of a read is written at most once an hour", {
  local_cache()
  local_fake_nextcloud(list("a.csv" = "a"))
  st <- fake_storage()
  p <- suppressMessages(cloud_path(st, "a.csv"))
  accessed <- meta_read(p)$accessed
  cloud_path(st, "a.csv")
  expect_equal(meta_read(p)$accessed, accessed)
  meta <- meta_read(p)
  meta$accessed <- accessed - 7200
  meta_write(p, meta)
  cloud_path(st, "a.csv")
  expect_gt(meta_read(p)$accessed, accessed - 7200)
})
