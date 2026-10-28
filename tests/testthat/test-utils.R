test_that("remote_path() joins and normalizes the parts", {
  expect_equal(remote_path("Raw", "PRESTI", "obs.csv"), "Raw/PRESTI/obs.csv")
  expect_equal(remote_path("/Raw//PRESTI/"), "Raw/PRESTI/")
  expect_equal(remote_path("Raw\\PRESTI\\obs.csv"), "Raw/PRESTI/obs.csv")
  expect_equal(remote_path(), "")
  expect_equal(remote_path(""), "")
  expect_error(remote_path("Raw/../secret"), "not allowed")
  expect_error(remote_path(1), "character")
})

test_that("folder helpers handle the trailing slash", {
  expect_equal(is_dir_path(c("", "a/", "a")), c(TRUE, TRUE, FALSE))
  expect_equal(dir_slash(c("", "a", "a/")), c("", "a/", "a/"))
  expect_equal(remote_dirname(c("a/b/c.csv", "a/b/", "c.csv")),
               c("a/b/", "a/", ""))
  expect_equal(remote_ancestors(c("a/b/c.csv", "a/d.csv", "e.csv")),
               c("a/", "a/b/"))
})

test_that("escape_path() escapes each segment", {
  expect_equal(escape_path("Raw data/é 1.csv"),
               "Raw%20data/%C3%A9%201.csv")
  expect_equal(escape_path("a/"), "a/")
  expect_equal(escape_path(""), "")
})
