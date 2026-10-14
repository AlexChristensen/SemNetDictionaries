test_that("find.dictionaries() finds package dictionaries", {

  all <- find.dictionaries()

  expect_named(all, c("names", "files"))
  expect_true(all(dictionaries(TRUE) %in% all$names))
  expect_identical(all$files[match(dictionaries(TRUE), all$names)],
                   rep("package", length(dictionaries(TRUE))))

  animals <- find.dictionaries("animals")
  expect_identical(animals$names, "animals")
  expect_identical(animals$files, "package")

})

test_that("find.dictionaries() finds environment and file dictionaries", {

  env <- globalenv()
  assign("found.dictionary", c("a", "b"), envir = env)
  withr::defer(rm("found.dictionary", envir = env))

  expect_identical(find.dictionaries("found"), list(names = "found", files = "envir"))

  dir <- withr::local_tempdir()
  saveRDS(c("a", "b"), file.path(dir, "infile.dictionary.rds"))

  res <- find.dictionaries("infile", add.path = dir)
  expect_identical(res$names, "infile")
  expect_identical(basename(res$files), "infile.dictionary.rds")

  # Not found without the directory
  expect_identical(find.dictionaries("infile"), list(names = character(0), files = character(0)))

})

test_that("find.dictionaries() returns nothing for unknown names", {

  expect_identical(
    find.dictionaries("not_a_dictionary"),
    list(names = character(0), files = character(0))
  )

})
