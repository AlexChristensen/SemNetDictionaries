#' Load Monikers
#' @description A wrapper function to load monikers into
#' the 'SemNetCleaner' package. Searches for monikers in \code{R}'s
#'  \code{\link{SemNetDictionaries}} package. Outputs a unique word list
#' that is combined from all dictionaries entered in the \code{moniker} argument
#' 
#' @param moniker Character vector.
#' monikers to load (must be a dictionary in
#' \code{\link[SemNetDictionaries]{dictionaries}}).
#' Names that do not have a moniker (e.g., \code{"general"}) or that are not
#' in the package (e.g., your own dictionaries) are skipped. Names are not
#' case sensitive and a \code{".moniker"} suffix is ignored.
#' More than 10 values that are not all dictionary names are treated as a corpus,
#' which does not have monikers
#' 
#' @param vector Boolean.
#' Should output be a vector? If \code{FALSE},
#' then output is a list.
#' Defaults to \code{TRUE}
#' 
#' @return If \code{vector = TRUE}, a \code{character} vector with the
#' variants (misspellings, plurals, alternate names) sorted alphabetically. The
#' names are the target word, followed by a number when a target word has more than one variant
#' (e.g., \code{"alligator2"}). If \code{vector = FALSE}, a named list with the
#' variants of each target word. An empty vector (list) is returned when no
#' moniker is found
#' 
#' @details A moniker table maps variants of a word (common misspellings,
#' plurals, spacing and hyphenation, abbreviations, and alternate names) to
#' the word that is in the dictionary (the \emph{target}). In
#' 'SemNetCleaner', a response that is not in the dictionary is converted to
#' the target of the variant that matches it. Because responses that are in
#' the dictionary are accepted as they are, a variant is never also an entry in
#' the dictionary of the same category.
#' 
#' New variants are welcome. Submit them at
#' \href{https://github.com/AlexChristensen/SemNetDictionaries/issues/new/choose}{https://github.com/AlexChristensen/SemNetDictionaries}
#' 
#' @examples 
#' #find dictionaries to load
#' dictionaries()
#' 
#' #load "animals" monikers
#' load.monikers("animals")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
# Load moniker Function
# Updated 14.10.2026
load.monikers <- function (moniker, vector = TRUE)
{
    # Ensure character
    moniker <- as.character(unlist(moniker, use.names = FALSE))
    moniker <- moniker[!is.na(moniker)]

    # Names of dictionaries in 'SemNetDictionaries'
    sndict <- dictionaries(TRUE)

    # Clean names
    clean <- tolower(.clean_name(moniker, "moniker"))

    # Corpus (monikers only exist for named dictionaries)
    if(length(moniker) > .corpus_threshold && !all(clean %in% c(sndict, letters)))
    {clean <- character(0)}

    # Monikers that exist in 'SemNetDictionaries'
    target <- unique(clean[clean %in% .shipped_names(".moniker")])

    # Combine into vector and alphabetize
    if(vector)
    {
        if(length(target) == 0L)
        {return(character(0))}

        if(length(target) == 1L)
        {return(.flat_moniker(target))}

        return(sort(unlist(lapply(target, .shipped_moniker), use.names = TRUE)))
    }

    # Combine into list
    if(length(target) == 0L)
    {return(list())}

    unlist(lapply(target, .shipped_moniker), recursive = FALSE)
}
#----
