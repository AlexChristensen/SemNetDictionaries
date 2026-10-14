test_that("the installed datasets stay within the size budget", {

  # The lazy-load database only exists in an installed package
  rdb <- system.file("data", "Rdata.rdb", package = "SemNetDictionaries")
  skip_if(!nzchar(rdb), "package is not installed")

  # 3.2 MB in version 0.3.0 (every dataset is xz compressed); update the budget
  # when a dataset is added on purpose
  expect_lt(file.size(rdb) / 1e6, 3.6)

})
