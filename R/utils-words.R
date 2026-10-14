#' Is a vector a clean dictionary?
#'
#' @description A clean dictionary is a character vector without missing
#' values that is lower case, sorted, and unique. Inserting into a clean
#' dictionary does not require sorting it again.
#'
#' @noRd
# Clean dictionary check----
# Updated 14.10.2026
.is_clean_dictionary <- function(x)
{
    is.character(x) && !anyNA(x) && !is.unsorted(x, strictly = TRUE) &&
        identical(x, tolower(x))
}

#' Position at which a word would be inserted into a sorted vector
#'
#' @description Binary search with the same (locale) collation that
#' \code{sort} uses.
#'
#' @return Index of the first element that is not smaller than \code{word}
#' (\code{length(x) + 1} if every element is smaller)
#'
#' @noRd
# Insertion position----
# Updated 14.10.2026
.insertion_position <- function(x, word)
{
    low <- 1L
    high <- length(x) + 1L

    while(low < high)
    {
        mid <- (low + high) %/% 2L

        if(x[mid] < word)
        {low <- mid + 1L
        }else{high <- mid}
    }

    low
}

#' Add words to a dictionary
#'
#' @description Equivalent to \code{unique(sort(tolower(c(base, new))))}.
#' When \code{base} is the dictionary that was returned by the previous call
#' (the usual case when words are added one after another) or is otherwise a
#' clean dictionary, a few new words are inserted by binary search instead of
#' sorting the whole dictionary again.
#'
#' @param base Character vector. Existing dictionary
#' @param new Character vector. Words to add
#' @param new.only Boolean. \code{TRUE} if \code{new} is known to not contain
#' words that are in \code{base}, which skips looking them up again
#'
#' @return Sorted, unique, lower case character vector
#'
#' @noRd
# Combine words----
# Updated 14.10.2026
.combine_words <- function(base, new, new.only = FALSE)
{
    # Lower case new words
    new <- unique(sort(tolower(as.character(new))))

    # Check whether the dictionary needs to be cleaned
    clean <- identical(base, .sndict_cache$last) || .is_clean_dictionary(base)

    if(clean)
    {
        # Remove words that are already in the dictionary
        if(!new.only)
        {new <- new[!new %in% base]}

        if(length(new) == 0L)
        {result <- base
        }else if(length(new) <= 64L)
        {
            # Insert new words
            pos <- vapply(new, function(w) .insertion_position(base, w), integer(1),
                          USE.NAMES = FALSE)
            index <- pos + seq_along(new) - 1L
            result <- character(length(base) + length(new))
            result[index] <- new
            result[-index] <- base
        }else{result <- unique(sort(c(base, new)))}

    }else{result <- unique(sort(tolower(c(as.character(base), new))))}

    # Remember the result so that the next call can skip the check
    .sndict_cache$last <- result

    result
}
