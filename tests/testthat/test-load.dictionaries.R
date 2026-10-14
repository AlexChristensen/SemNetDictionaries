test_that("load.dictionaries() returns sorted, unique character vectors", {

  for(name in dictionaries(TRUE)){

    words <- load.dictionaries(name)

    expect_type(words, "character")
    expect_gt(length(words), 0)
    expect_false(anyNA(words))
    expect_identical(words, unique(sort(words)))

  }

})

test_that("load.dictionaries() combines dictionaries", {

  combined <- load.dictionaries("animals", "fruits")

  expect_identical(
    combined,
    unique(sort(c(load.dictionaries("animals"), load.dictionaries("fruits"))))
  )

  # Order of input and repeated calls (cache) do not matter
  expect_identical(load.dictionaries("fruits", "animals"), combined)
  expect_identical(load.dictionaries("animals", "fruits"), combined)

})

test_that("load.dictionaries() accepts names in different forms", {

  animals <- load.dictionaries("animals")

  expect_identical(load.dictionaries("Animals"), animals)
  expect_identical(load.dictionaries(" animals "), animals)
  expect_identical(load.dictionaries("animals.dictionary"), animals)
  expect_identical(load.dictionaries(c("animals", "animals")), animals)

})

test_that("load.dictionaries() single letter dictionaries", {

  d <- load.dictionaries("d")

  expect_true(all(startsWith(d, "d")))
  expect_true(all(d %in% load.dictionaries("general")))
  expect_identical(load.dictionaries("D"), d)

  expect_identical(
    load.dictionaries("a", "d"),
    unique(sort(c(load.dictionaries("a"), d)))
  )

  expect_true(all(load.dictionaries("animals") %in% load.dictionaries("animals", "a")))

})

test_that("load.dictionaries() errors without input", {

  expect_error(load.dictionaries(), "At least one dictionary")
  expect_error(load.dictionaries(NA_character_), "At least one dictionary")

})

test_that("load.dictionaries() warns about dictionaries that are not found", {

  expect_warning(
    res <- load.dictionaries("not_a_dictionary"),
    class = "semnet_dictionary_not_found"
  )
  expect_identical(res, character(0))

  # Found dictionaries are still loaded
  expect_warning(
    res <- load.dictionaries("animals", "not_a_dictionary"),
    "'not_a_dictionary' was not found"
  )
  expect_identical(res, load.dictionaries("animals"))

  # More than one letter is not a letter dictionary
  expect_warning(load.dictionaries("ab"), class = "semnet_dictionary_not_found")

})

test_that("load.dictionaries() treats more than 10 words as a corpus", {

  corpus <- c("A", "dog", "cat", "I", "bird", "the", "horse", "a", "cow", "pig", "fish")

  # Single letters in a corpus used to fail
  expect_identical(
    load.dictionaries(corpus),
    c("a", "bird", "cat", "cow", "dog", "fish", "horse", "i", "pig", "the")
  )

  # Names of 11 dictionaries are not a corpus
  names <- dictionaries(TRUE)[1:11]
  expect_identical(
    load.dictionaries(names),
    unique(sort(unlist(lapply(names, load.dictionaries))))
  )

})

test_that("load.dictionaries() finds dictionaries in the global environment", {

  env <- globalenv()
  assign("zebra.dictionary", c("stripe", "mane", "Hoof"), envir = env)
  withr::defer(rm("zebra.dictionary", envir = env))

  expect_identical(load.dictionaries("zebra"), sort(c("stripe", "mane", "Hoof")))
  expect_identical(
    load.dictionaries("zebra", "stop_words"),
    unique(sort(c("stripe", "mane", "Hoof", load.dictionaries("stop_words"))))
  )

  # A name that merely contains "dictionary" is not a dictionary
  assign("my_dictionaryX", c("nope"), envir = env)
  withr::defer(rm("my_dictionaryX", envir = env))
  expect_warning(load.dictionaries("my_"), class = "semnet_dictionary_not_found")

})

test_that("load.dictionaries() finds dictionary files", {

  dir <- withr::local_tempdir()
  saveRDS(c("pear", "kiwi"), file.path(dir, "fruity.dictionary.rds"))

  # add.path
  expect_identical(load.dictionaries("fruity", add.path = dir), c("kiwi", "pear"))

  # Working directory
  withr::with_dir(dir, expect_identical(load.dictionaries("fruity"), c("kiwi", "pear")))

  # Not found elsewhere
  expect_warning(load.dictionaries("fruity"), class = "semnet_dictionary_not_found")

  expect_error(load.dictionaries("fruity", add.path = file.path(dir, "nope")), "does not exist")

  # add.path is not used (or checked) for dictionaries in the package
  expect_identical(
    load.dictionaries("animals", add.path = file.path(dir, "nope")),
    load.dictionaries("animals")
  )

})

test_that("the cache cannot be changed by modifying results", {

  first <- load.dictionaries("animals")
  copy <- first
  first[1] <- "changed"

  expect_identical(load.dictionaries("animals"), copy)

})
