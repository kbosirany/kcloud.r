# The package must run on Linux, Windows and macOS.

test_that("the sources use no absolute home path nor shell command", {
  dir_r <- test_path("..", "..", "R")
  skip_if(!dir.exists(dir_r), "sources not available")
  code <- unlist(lapply(list.files(dir_r, full.names = TRUE), readLines))
  code <- code[!grepl("^\\s*#", code)]
  expect_false(any(grepl("/home/", code, fixed = TRUE)))
  expect_false(any(grepl("\\bsystem\\(|\\bshell\\(", code)))
})
