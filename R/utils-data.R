#' Package-level cache
#'
#' @description Environment that holds work that is expensive to repeat: the
#' names of the shipped datasets, each shipped dictionary after it has been
#' sorted and de-duplicated, flattened monikers, single-letter dictionaries,
#' and the last dictionary returned by \code{append.dictionary} (see there).
#' Only data that ships with the package is cached. Dictionaries from the
#' global environment or from files can change between calls, so they are
#' always read fresh.
#'
#' @noRd
# Cache----
# Updated 14.10.2026
.sndict_cache <- new.env(parent = emptyenv())

#' Number of entries beyond which a character vector is treated as a corpus
#' rather than as a set of dictionary names
#' @noRd
.corpus_threshold <- 10L

#' Environment holding the datasets shipped in \code{data/}
#'
#' @description Works for installed packages and for \code{pkgload::load_all()}
#' (unlike \code{system.file("data", "Rdata.rds")}, which only exists after
#' installation). Reading from this environment does not go through
#' \code{utils::data()}, which rebuilds data tables on every call.
#'
#' @noRd
# Lazy data environment----
# Updated 14.10.2026
.lazydata_env <- function()
{
    getNamespaceInfo(asNamespace("SemNetDictionaries"), "lazydata")
}

#' Names of shipped datasets that end in a given suffix, without the suffix
#' @noRd
# Shipped names----
# Updated 14.10.2026
.shipped_names <- function(suffix)
{
    key <- paste0("names", suffix)

    if(is.null(.sndict_cache[[key]]))
    {
        all.names <- sort(ls(.lazydata_env(), all.names = TRUE))
        pattern <- paste0(gsub(".", "\\.", suffix, fixed = TRUE), "$")
        target <- grep(pattern, all.names, value = TRUE)
        .sndict_cache[[key]] <- sub(pattern, "", target)
    }

    .sndict_cache[[key]]
}

#' Fetch a shipped dataset by its full object name (e.g., "animals.dictionary")
#' @noRd
# Get shipped data----
# Updated 14.10.2026
.get_shipped <- function(object.name)
{
    get(object.name, envir = .lazydata_env(), inherits = FALSE)
}

#' Shipped dictionary, sorted and unique, computed once per session
#' @noRd
# Sorted shipped dictionary----
# Updated 14.10.2026
.sorted_dictionary <- function(name)
{
    key <- paste0("dict:", name)

    if(is.null(.sndict_cache[[key]]))
    {
        words <- .get_shipped(paste0(name, ".dictionary"))

        # Sorting is only needed when the dictionary is not sorted already
        # (checking is much faster than sorting)
        if(anyNA(words) || is.unsorted(words, strictly = TRUE))
        {words <- unique(sort(words))}

        .sndict_cache[[key]] <- words
    }

    .sndict_cache[[key]]
}

#' Words in \code{general.dictionary} that start with a letter, computed once
#' per letter and session
#' @noRd
# Single letter dictionary----
# Updated 14.10.2026
.letter_dictionary <- function(letter)
{
    key <- paste0("letter:", letter)

    if(is.null(.sndict_cache[[key]]))
    {
        general <- .sorted_dictionary("general")
        .sndict_cache[[key]] <- general[startsWith(general, letter)]
    }

    .sndict_cache[[key]]
}

#' Shipped moniker, read once per session
#' @noRd
# Shipped moniker----
# Updated 14.10.2026
.shipped_moniker <- function(name)
{
    .get_shipped(paste0(name, ".moniker"))
}

#' Flattened shipped moniker (named vector sorted by value), computed once
#' @noRd
# Flattened moniker----
# Updated 14.10.2026
.flat_moniker <- function(name)
{
    key <- paste0("moniker:", name)

    if(is.null(.sndict_cache[[key]]))
    {.sndict_cache[[key]] <- sort(unlist(list(.shipped_moniker(name))))}

    .sndict_cache[[key]]
}

#' Clean a user-supplied dictionary or moniker name
#'
#' @description Trims white space and removes a trailing \code{.dictionary},
#' \code{.dictionary.rds} or \code{.moniker}
#'
#' @noRd
# Clean names----
# Updated 14.10.2026
.clean_name <- function(x, type = c("dictionary", "moniker"))
{
    type <- match.arg(type)
    pattern <- paste0("\\.", type, "(\\.rds)?$")
    sub(pattern, "", trimws(as.character(x)), ignore.case = TRUE)
}

#' Files named \code{<name>.dictionary.rds} in the places custom dictionaries
#' are saved by \code{append.dictionary}
#'
#' @description Checks (without recursing) the directory given in
#' \code{add.path}, the working directory, and \code{tempdir()}, in that order
#'
#' @return Full path of the first file found or \code{NA_character_}
#'
#' @noRd
# Find dictionary file----
# Updated 14.10.2026
.find_dictionary_file <- function(name, add.path = NULL)
{
    dirs <- unique(c(add.path, getwd(), tempdir()))
    candidates <- file.path(dirs, paste0(name, ".dictionary.rds"))
    found <- candidates[file.exists(candidates)]

    if(length(found) == 0L) NA_character_ else found[1L]
}

#' Resolve \code{add.path}, opening a directory chooser for \code{"choose"}
#' @noRd
# Resolve additional path----
# Updated 14.10.2026
.resolve_add_path <- function(add.path)
{
    if(is.null(add.path)) return(NULL)

    if(!is.character(add.path) || length(add.path) != 1L || is.na(add.path))
    {stop("'add.path' must be a single path or \"choose\".", call. = FALSE)}

    if(identical(add.path, "choose"))
    {add.path <- .choose_directory()}

    if(!dir.exists(add.path))
    {stop("'add.path' does not exist: ", add.path, call. = FALSE)}

    normalizePath(add.path, winslash = "/")
}

#' Interactive directory chooser that fails with a useful message
#' @noRd
# Choose directory----
# Updated 14.10.2026
.choose_directory <- function()
{
    if(requireNamespace("tcltk", quietly = TRUE))
    {
        chosen <- tryCatch(
            paste(as.character(tcltk::tkchooseDirectory()), collapse = " "),
            error = function(e)
            {
                stop("A directory could not be chosen with 'tcltk' (", conditionMessage(e),
                     "). Provide the path instead.", call. = FALSE)
            }
        )
    }else{
        stop("Choosing a directory requires the 'tcltk' package. ",
             "Provide the path instead.", call. = FALSE)
    }

    if(length(chosen) != 1L || is.na(chosen) || !nzchar(chosen))
    {stop("No directory was chosen.", call. = FALSE)}

    chosen
}
