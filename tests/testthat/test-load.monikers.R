test_that("load.monikers() returns a named character vector", {

  for(name in c("animals", "fruits", "vegetables", "jobs", "hot", "good")){

    monikers <- load.monikers(name)

    expect_type(monikers, "character")
    expect_gt(length(monikers), 0)
    expect_false(is.null(names(monikers)))
    expect_false(anyNA(names(monikers)))
    expect_false(is.unsorted(unname(monikers)))

  }

  animals <- load.monikers("animals")

  # Names are the target word (+ number) and values are variants
  expect_true("gator" %in% animals)
  expect_identical(
    unique(gsub("[[:digit:]]+", "", names(animals[animals == "gator"]))),
    "alligator"
  )

})

test_that("load.monikers() list has the variants of each target word", {

  monikers <- load.monikers("animals", vector = FALSE)

  expect_type(monikers, "list")
  expect_true(all(vapply(monikers, is.character, logical(1))))
  expect_true("gator" %in% monikers[["alligator"]])

  # Same information as the vector
  expect_identical(sort(unlist(monikers, use.names = FALSE)),
                   sort(unname(load.monikers("animals"))))

})

test_that("load.monikers() combines monikers", {

  expect_identical(
    load.monikers(c("animals", "fruits")),
    sort(unlist(list(
      load.monikers("animals", vector = FALSE),
      load.monikers("fruits", vector = FALSE)
    )))
  )

  expect_length(
    load.monikers(c("animals", "fruits"), vector = FALSE),
    length(load.monikers("animals", vector = FALSE)) +
      length(load.monikers("fruits", vector = FALSE))
  )

})

test_that("load.monikers() is empty without monikers", {

  for(name in c("general", "hunspell", "most_common", "stop_words", "not_a_dictionary")){

    expect_identical(load.monikers(name), character(0))
    expect_identical(load.monikers(name, vector = FALSE), list())

  }

  # Other dictionaries are skipped
  expect_identical(load.monikers(c("animals", "general")), load.monikers("animals"))

})

test_that("load.monikers() accepts names in different forms", {

  animals <- load.monikers("animals")

  expect_identical(load.monikers("Animals"), animals)
  expect_identical(load.monikers("animals.moniker"), animals)

})

test_that("load.monikers() has no monikers for a corpus", {

  corpus <- c("A", "dog", "cat", "I", "bird", "the", "horse", "a", "cow", "pig", "fish")

  expect_identical(load.monikers(corpus), character(0))

})
