#' List Names of Dictionaries in 'SemNetDictionaries'
#' @description A wrapper function to identify all dictionaries included in 
#' \code{\link{SemNetDictionaries}}
#' 
#' @param quiet Boolean.
#' Determines whether the return should be quiet (does not print dictionaries).
#' Defaults to \code{FALSE}
#' 
#' @return Returns the names of dictionaries in \code{\link{SemNetDictionaries}}
#' (invisibly unless \code{quiet = TRUE})
#' 
#' @examples
#' # List names of dictionaries in 'SemNetDictionaries'
#' dictionaries()
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{find.dictionaries}} to find where dictionaries are stored,
#' \code{\link{append.dictionary}} to create a new dictionary
#' 
#' @export
# Find Dictionaries
# Updated 14.10.2026
dictionaries <- function (quiet = FALSE)
{
    # Names of the dictionaries shipped with the package
    dict.names <- .shipped_names(".dictionary")

    # Quiet return
    if(isTRUE(quiet))
    {return(dict.names)}

    # Print dictionary names to user
    print(dict.names)

    # Message for single letter dictionaries
    message('Single letter dictionaries can also be used (e.g., "f")')

    # Suppress return
    invisible(dict.names)
}
#----
