test_that("dictionaries() lists the shipped dictionaries", {

  expected <- c(
    "animals", "coca", "cocaspell", "fruits", "general", "good",
    "hot", "hunspell", "jobs", "most_common", "stop_words", "vegetables"
  )

  expect_identical(dictionaries(TRUE), expected)

  # Monikers and brit2us are not dictionaries
  expect_false(any(c("brit2us", "animals.moniker") %in% dictionaries(TRUE)))

})

test_that("dictionaries() prints unless quiet = TRUE", {

  expect_silent(dictionaries(quiet = TRUE))
  expect_output(
    expect_message(dictionaries(quiet = FALSE), "Single letter"),
    "animals"
  )
  expect_output(
    expect_message(res <- dictionaries(), "Single letter"),
    "animals"
  )
  expect_identical(res, dictionaries(TRUE))

})
