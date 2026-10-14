#' Load Dictionaries
#' @description A wrapper function to load dictionaries into
#' the 'SemNetCleaner' package. Searches for dictionaries in the
#' \code{\link{SemNetDictionaries}} package, \code{R}'s global
#' environment, and a few directories on your computer.
#' Outputs a unique word list that is combined from all dictionaries entered
#' in the \code{...} argument
#'
#' @param ... Character.
#' Dictionaries to load. Dictionary names are not case sensitive for the
#' dictionaries in \code{\link{SemNetDictionaries}} and for single letters.
#' A \code{".dictionary"} suffix is ignored.
#'
#' Dictionaries in your global environment
#' MUST be objects called \code{"*.dictionary"} (see examples)
#'
#' More than 10 values that are not all dictionary names are treated as a
#' corpus: the (lower case) words are returned as a sorted, unique vector
#'
#' @param add.path Character.
#' Path to a directory with additional \code{"*.dictionary.rds"} files
#' (see \code{\link{append.dictionary}}). The directory is NOT searched
#' recursively. Set to \code{"choose"} to open an interactive directory explorer
#'
#' Dictionaries are looked for in this order: the \code{\link{SemNetDictionaries}}
#' package (see \code{\link[SemNetDictionaries]{dictionaries}}), the global
#' environment, \code{add.path}, the working directory, and \code{tempdir()}
#' (see \code{\link[SemNetDictionaries]{find.dictionaries}})
#'
#' @return Returns a vector of unique words that have been combined
#' and alphabetized from the specified dictionaries.
#' If a dictionary cannot be found, a warning of class
#' \code{"semnet_dictionary_not_found"} is given and the dictionary is left out
#' (an empty \code{character} vector is returned when none are found)
#'
#' @examples
#' # Find dictionaries to load
#' dictionaries()
#'
#' # Load "animals" dictionary
#' load.dictionaries("animals")
#'
#' # Create a dictionary
#' new.dictionary <- append.dictionary("words", "are", "fun")
#'
#' # Load created dictionary
#' load.dictionaries("new")
#'
#' # Load animals and new dictionary
#' load.dictionaries("animals", "new")
#'
#' # Single letter dictionary
#' load.dictionaries("d")
#'
#' # Multiple letters dictionary
#' load.dictionaries("a", "d")
#'
#' # Category and letters dictionary
#' load.dictionaries("animals", "a")
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @export
# Load Dictionary Function
# Updated 14.10.2026
load.dictionaries <- function (..., add.path = NULL)
{
    # List dictionaries
    dictionary <- unlist(list(...), use.names = FALSE)

    # Check for input
    if(length(dictionary) == 0L)
    {stop("At least one dictionary must be specified.", call. = FALSE)}

    # Ensure character and remove missing values
    dictionary <- as.character(dictionary)
    dictionary <- dictionary[!is.na(dictionary)]

    if(length(dictionary) == 0L)
    {stop("At least one dictionary must be specified.", call. = FALSE)}

    # Clean names
    clean <- .clean_name(dictionary, "dictionary")

    # Dictionaries in 'SemNetDictionaries' (not case sensitive) and single letters
    sndict <- dictionaries(TRUE)
    is.pkg <- tolower(clean) %in% sndict
    is.letter <- !is.pkg & tolower(clean) %in% letters

    # Additional path (only needed for dictionaries that do not ship with the package)
    if(!all(is.pkg | is.letter))
    {add.path <- .resolve_add_path(add.path)
    }else{add.path <- NULL}

    # Set in case of corpus being used
    if(length(dictionary) > .corpus_threshold)
    {
        # Names are never repeated across dictionaries, so only a vector that
        # consists of known names is not a corpus
        known <- is.pkg | is.letter |
            vapply(clean, .user_dictionary_exists, logical(1), add.path = add.path)

        if(!all(known))
        {return(unique(sort(tolower(dictionary))))}
    }

    # Initialize results
    dict.list <- vector("list", length(clean))
    not.found <- logical(length(clean))

    # Cache key for combinations of shipped dictionaries
    shipped <- is.pkg | is.letter
    key <- NULL

    if(all(shipped))
    {
        key <- paste(sort(unique(tolower(clean))), collapse = "|")
        cached <- .sndict_cache$combinations[[key]]

        if(!is.null(cached))
        {return(cached)}
    }

    # Load each dictionary
    for(i in seq_along(clean))
    {
        if(is.pkg[i])
        {dict.list[[i]] <- .sorted_dictionary(tolower(clean[i]))
        }else if(is.letter[i])
        {dict.list[[i]] <- .letter_dictionary(tolower(clean[i]))
        }else{
            user <- .load_user_dictionary(clean[i], add.path)

            if(is.null(user))
            {not.found[i] <- TRUE
            }else{dict.list[[i]] <- user}
        }
    }

    # Let user know about missing dictionaries
    if(any(not.found))
    {
        absent <- unique(clean[not.found])

        warning(structure(
            class = c("semnet_dictionary_not_found", "warning", "condition"),
            list(
                message = paste0(
                    paste0("'", absent, "'", collapse = ", "),
                    if(length(absent) == 1L) " was" else " were",
                    " not found in existing dictionaries. Dictionaries are looked for in ",
                    "'SemNetDictionaries', the global environment (as '<name>.dictionary'), ",
                    "and as '<name>.dictionary.rds' in 'add.path', the working directory, and tempdir()."
                ),
                call = NULL,
                dictionary = absent
            )
        ))
    }

    # Combine into vector and alphabetize (each piece is already sorted and unique)
    pieces <- dict.list[!not.found]

    if(length(pieces) == 0L)
    {full.dict <- character(0)
    }else if(length(pieces) == 1L)
    {full.dict <- pieces[[1L]]
    }else{full.dict <- unique(sort(unlist(pieces, use.names = FALSE)))}

    # Store combinations of shipped dictionaries (small, first in first out)
    if(!is.null(key) && length(pieces) > 0L)
    {
        combinations <- .sndict_cache$combinations

        if(is.null(combinations))
        {combinations <- list()}

        combinations[[key]] <- full.dict
        .sndict_cache$combinations <- combinations[utils::tail(seq_along(combinations), 8L)]
    }

    return(full.dict)
}
#----

#' Does a dictionary from the global environment or a file exist?
#' @noRd
# User dictionary exists----
# Updated 14.10.2026
.user_dictionary_exists <- function(name, add.path = NULL)
{
    object <- paste0(name, ".dictionary")

    (exists(object, envir = globalenv(), inherits = FALSE) &&
            is.character(get(object, envir = globalenv(), inherits = FALSE))) ||
        !is.na(.find_dictionary_file(name, add.path))
}

#' Load a dictionary from the global environment or from a file
#'
#' @return Sorted unique words or \code{NULL} if the dictionary does not exist
#'
#' @noRd
# Load user dictionary----
# Updated 14.10.2026
.load_user_dictionary <- function(name, add.path = NULL)
{
    object <- paste0(name, ".dictionary")

    # Global environment
    if(exists(object, envir = globalenv(), inherits = FALSE))
    {
        words <- get(object, envir = globalenv(), inherits = FALSE)

        if(is.character(words))
        {return(unique(sort(words)))}
    }

    # Files
    file <- .find_dictionary_file(name, add.path)

    if(!is.na(file))
    {
        words <- readRDS(file)

        if(is.character(words))
        {return(unique(sort(words)))}
    }

    NULL
}
