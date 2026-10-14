test_that("append.dictionary() creates a dictionary", {

  expect_message(
    words <- append.dictionary(c("Words", "are", "fun", "are")),
    "Dictionary has been created"
  )

  expect_identical(words, c("are", "fun", "words"))

})

test_that("append.dictionary() adds to the dictionary the user has", {

  base <- c("apple", "cherry", "plum")

  expect_message(
    words <- append.dictionary("Banana", base, textcleaner = TRUE),
    "ADDED TO DICTIONARY: 'banana'"
  )
  expect_identical(words, c("apple", "banana", "cherry", "plum"))

  # Word already in the dictionary
  expect_silent(same <- append.dictionary("plum", base, textcleaner = TRUE))
  expect_identical(same, base)

  # Any name for the dictionary
  expect_identical(
    suppressMessages(append.dictionary("zebra", base, textcleaner = TRUE)),
    c("apple", "cherry", "plum", "zebra")
  )

  # The literal word "dictionary" is a new word
  expect_identical(
    suppressMessages(append.dictionary("dictionary", base, textcleaner = TRUE)),
    c("apple", "cherry", "dictionary", "plum")
  )

  # Named dictionary
  full_dictionary <- base
  expect_identical(
    suppressMessages(append.dictionary("zebra", full_dictionary, textcleaner = TRUE)),
    c("apple", "cherry", "plum", "zebra")
  )
  expect_identical(
    suppressMessages(append.dictionary(full_dictionary, "zebra", textcleaner = TRUE)),
    c("apple", "cherry", "plum", "zebra")
  )

})

test_that("append.dictionary() equals sorting the combined dictionary", {

  full_dictionary <- load.dictionaries("cocaspell")
  set.seed(42)

  for(i in 1:20){

    # Words that are or are not in the dictionary, with upper case
    word <- paste0(sample(letters, sample(2:9, 1), TRUE), collapse = "")
    word <- c(word, toupper(sample(full_dictionary, 1)))

    result <- suppressMessages(
      append.dictionary(word, full_dictionary, dictionary.name = "full", textcleaner = TRUE)
    )

    expect_identical(result, unique(sort(c(full_dictionary, tolower(word)))))
    full_dictionary <- result

  }

})

test_that("append.dictionary() handles dictionaries that are not clean", {

  messy <- c("Zebra", "apple", "apple", "Cherry")

  expect_identical(
    suppressMessages(append.dictionary("banana", messy, textcleaner = TRUE)),
    c("apple", "banana", "cherry", "zebra")
  )

  # Many words
  many <- paste0("word", 1:100)
  expect_identical(
    suppressMessages(append.dictionary(many, c("apple", "plum"), textcleaner = TRUE)),
    unique(sort(c("apple", "plum", many)))
  )

})

test_that("append.dictionary() saves dictionaries to files", {

  dir <- withr::local_tempdir()

  expect_message(
    res <- append.dictionary(c("words", "are", "fun"), dictionary.name = "test",
                             save.location = "path", path = dir),
    "A new dictionary file was created"
  )
  expect_null(res)
  expect_identical(readRDS(file.path(dir, "test.dictionary.rds")), c("are", "fun", "words"))

  # Update
  expect_message(
    append.dictionary(c("more", "words"), dictionary.name = "test",
                      save.location = "path", path = dir),
    "has been updated"
  )
  expect_identical(
    readRDS(file.path(dir, "test.dictionary.rds")),
    c("are", "fun", "more", "words")
  )

  # Working directory and loading it back
  withr::with_dir(dir, {
    suppressMessages(append.dictionary("wd", dictionary.name = "wdtest", save.location = "wd"))
    expect_identical(load.dictionaries("wdtest"), "wd")
  })

  expect_error(append.dictionary("a", save.location = "path"), "'path' must be specified")
  expect_error(append.dictionary("a", save.location = "path", path = file.path(dir, "no")),
               "does not exist")

})

test_that("append.dictionary(package = TRUE) is defunct", {

  expect_error(append.dictionary("a", package = TRUE), "defunct")

})

test_that("append.dictionary(textcleaner = TRUE) warns when words of the dictionary argument are not saved", {

  dir <- withr::local_tempdir()
  suppressMessages(append.dictionary(c("apple", "plum"), dictionary.name = "fruit",
                                     save.location = "path", path = dir))

  # The file exists and the dictionary argument has words it does not have
  full_dictionary <- c("apple", "banana", "cherry", "plum")

  expect_warning(
    suppressMessages(append.dictionary(full_dictionary, dictionary.name = "fruit",
                                       save.location = "path", path = dir, textcleaner = TRUE)),
    class = "semnet_dictionary_words_not_saved"
  )
  expect_identical(readRDS(file.path(dir, "fruit.dictionary.rds")), c("apple", "plum"))

  warned <- tryCatch(
    suppressMessages(append.dictionary(full_dictionary, dictionary.name = "fruit",
                                       save.location = "path", path = dir, textcleaner = TRUE)),
    semnet_dictionary_words_not_saved = function(w) w
  )
  expect_identical(warned$words, c("banana", "cherry"))
  expect_match(conditionMessage(warned), "2 word\\(s\\)")

  # No warning when the file has all the words of the dictionary argument
  expect_no_warning(
    suppressMessages(append.dictionary("apple", dictionary.name = "fruit",
                                       save.location = "path", path = dir, textcleaner = TRUE))
  )
  expect_no_warning(
    suppressMessages(append.dictionary(c("apple", "plum"), c("plum"), dictionary.name = "fruit",
                                       save.location = "path", path = dir, textcleaner = TRUE))
  )

  # New words under a name that is not a dictionary are added without a warning
  words <- c("cherry")
  expect_no_warning(
    suppressMessages(append.dictionary(words, dictionary.name = "fruit",
                                       save.location = "path", path = dir, textcleaner = TRUE))
  )
  expect_identical(readRDS(file.path(dir, "fruit.dictionary.rds")), c("apple", "cherry", "plum"))

  # Without textcleaner everything is added
  expect_no_warning(
    suppressMessages(append.dictionary(full_dictionary, dictionary.name = "fruit",
                                       save.location = "path", path = dir))
  )
  expect_identical(readRDS(file.path(dir, "fruit.dictionary.rds")),
                   c("apple", "banana", "cherry", "plum"))

  # In the global environment (no file), nothing is dropped
  expect_no_warning(
    suppressMessages(append.dictionary("kiwi", full_dictionary, textcleaner = TRUE))
  )

})
