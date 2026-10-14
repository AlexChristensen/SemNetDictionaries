categories <- c("animals", "fruits", "vegetables", "jobs", "hot", "good")

get_dictionary <- function(name) get(paste0(name, ".dictionary"), envir = asNamespace("SemNetDictionaries"))
get_moniker <- function(name) get(paste0(name, ".moniker"), envir = asNamespace("SemNetDictionaries"))

test_that("every shipped dictionary is a character vector without missing values", {

  for(name in dictionaries(TRUE)){

    words <- load.dictionaries(name)

    expect_type(words, "character")
    expect_false(anyNA(words), info = name)
    expect_false(any(!nzchar(words)), info = name)

  }

})

test_that("category dictionaries are clean", {

  for(name in categories){

    words <- get_dictionary(name)

    expect_type(words, "character")
    expect_false(anyNA(words), info = name)
    expect_false(any(!nzchar(words)), info = name)
    expect_identical(words, tolower(words), info = name)
    expect_identical(words, trimws(words), info = name)
    expect_false(any(grepl("  ", words, fixed = TRUE)), info = name)
    expect_identical(words, unique(words), info = name)
    expect_identical(words, sort(words, method = "radix"), info = name)

  }

})

test_that("category dictionaries do not contain stop words or single letters", {

  stop_words <- load.dictionaries("stop_words")

  for(name in categories){

    words <- get_dictionary(name)

    expect_false(any(nchar(words) == 1L), info = name)
    expect_identical(intersect(words, setdiff(stop_words, "it")), character(0), info = name)

  }

})

test_that("category monikers are named lists of character vectors", {

  for(name in categories){

    moniker <- get_moniker(name)

    expect_type(moniker, "list")
    expect_false(is.null(names(moniker)), info = name)
    expect_false(anyNA(names(moniker)), info = name)
    expect_identical(names(moniker), unique(names(moniker)), info = name)
    expect_true(all(vapply(moniker, is.character, logical(1))), info = name)
    expect_true(all(lengths(moniker) > 0L), info = name)

  }

})

test_that("moniker targets are dictionary entries", {

  for(name in categories){

    targets <- names(get_moniker(name))

    expect_identical(setdiff(targets, get_dictionary(name)), character(0), info = name)
    expect_false(any(grepl("[[:digit:]]", targets)), info = name)

  }

})

test_that("moniker variants are lower case, unique, and not dictionary entries", {

  for(name in categories){

    moniker <- get_moniker(name)
    variants <- unlist(moniker, use.names = FALSE)

    expect_false(anyNA(variants), info = name)
    expect_false(any(!nzchar(variants)), info = name)
    expect_identical(variants, tolower(variants), info = name)
    expect_identical(variants, trimws(variants), info = name)

    # A variant belongs to one target only
    expect_identical(variants, unique(variants), info = name)

    # The dictionary accepts words before monikers are tried
    expect_identical(intersect(variants, get_dictionary(name)), character(0), info = name)

    # A target is not a variant of another target
    expect_identical(intersect(variants, names(moniker)), character(0), info = name)

  }

})

test_that("load.monikers() matches the shipped monikers", {

  for(name in categories){

    flat <- load.monikers(name)
    moniker <- get_moniker(name)

    expect_length(flat, length(unlist(moniker)))
    expect_identical(sort(unname(flat)), sort(unlist(moniker, use.names = FALSE)))

    # Every target can be recovered the way 'SemNetCleaner' does it
    targets <- unique(tolower(gsub("[[:digit:]]+", "", names(flat))))
    expect_identical(sort(targets), sort(names(moniker)))

  }

})

test_that("hot and good only share words with a sense in both", {

  expect_identical(
    sort(intersect(get_dictionary("hot"), get_dictionary("good"))),
    c("beautiful")
  )

})

test_that("brit2us is a named list", {

  expect_type(brit2us, "list")
  expect_false(is.null(names(brit2us)))

})

test_that("the shipped moniker examples work", {

  animals <- load.monikers("animals")
  expect_identical(unname(animals[animals == "bigfoot"]), "bigfoot")
  expect_identical(
    unique(gsub("[[:digit:]]+", "", names(animals[animals == "bigfoot"]))),
    "sasquatch"
  )

  jobs <- load.monikers("jobs")
  expect_identical(
    unique(gsub("[[:digit:]]+", "", names(jobs[jobs == "ceo"]))),
    character(0)
  )
  expect_identical(
    unique(gsub("[[:digit:]]+", "", names(jobs[jobs == "chief executive officer"]))),
    "ceo"
  )

})

test_that("word form monikers (coca, cocaspell) follow their rules", {

  for(name in c("coca", "cocaspell")){

    moniker <- get_moniker(name)
    dictionary <- get_dictionary(name)
    targets <- names(moniker)
    variants <- unlist(moniker, use.names = FALSE)

    expect_type(moniker, "list")
    expect_true(all(vapply(moniker, is.character, logical(1))), info = name)

    # SemNetCleaner lower cases and removes digits from the target it finds
    expect_identical(targets, tolower(targets), info = name)
    expect_identical(targets, unique(targets), info = name)
    expect_false(any(grepl("[[:digit:]]", targets)), info = name)
    expect_false(any(grepl("[[:digit:]]", variants)), info = name)
    expect_identical(setdiff(targets, dictionary), character(0), info = name)

    expect_false(anyNA(variants), info = name)
    expect_false(any(!nzchar(variants)), info = name)
    expect_identical(variants, tolower(variants), info = name)

    # A form is listed once per word (a word can be its own form: best_guess() matches the variants)
    expect_false(any(vapply(moniker, anyDuplicated, integer(1)) > 0L), info = name)

  }

  # Examples that were capitalized or merged in 0.2.1
  coca <- get_moniker("coca")
  expect_identical(coca[["french"]], c("french", "frenches"))
  expect_false("French" %in% names(coca))
  expect_true(all(c("bay", "bays", "baying", "bayed") %in% coca[["bay"]]))

})

test_that("the flattened word form monikers keep the target in the names", {

  flat <- load.monikers("cocaspell")

  expect_identical(
    unique(gsub("[[:digit:]]+", "", names(flat[flat == "were"])))[1],
    "be"
  )

})
