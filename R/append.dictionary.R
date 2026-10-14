#' Appendix Dictionary
#' @description A function designed to create post-hoc dictionaries in the
#' \code{\link{SemNetDictionaries}} package. This allows for new semantic categories or word lists
#' to be saved for future use (i.e., your own personal dictionary).
#' Dictionaries created using this function can either be saved as an R object to your global
#' environment or as a .rds file on your current computer. Open-source community-derived
#' dictionaries can be uploaded to and downloaded from
#' \href{https://github.com/AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}
#'
#' @param ... Character vector.
#' A vector of words to create or add to a dictionary
#'
#' @param dictionary.name Character.
#' Name of dictionary to create or add words to.
#' Defaults to \code{"appendix"}.
#' Input a name to create or add to an existing dictionary.
#' This function with automatically name files with the \code{"*.dictionary.rds"} suffix
#'
#' @param save.location Character.
#' A choice for where to store appendix dictionary.
#' Defaults to \code{"envir"}.
#'
#' \itemize{
#'
#' \item \code{"envir"}: Returns dictionary as a vector object to
#' \code{R}'s global environment
#'
#' \item \code{"wd"}: Saves dictionary to working directory.
#' Useful for storing dictionaries alongside projects
#'
#' \item \code{"choose"}: User chooses a directory for more permanent
#' storage. This will allow you to use this dictionary in the future
#'
#' \item \code{"path"}: User specifies a path to a directory if it is already known.
#' This will allow direct updates to the directory and bypass the prompts in
#' the save/update menus. This will also allow you to use
#' this dictionary in the future
#'
#' }
#'
#' @param path Character.
#' A path to an existing directory.
#' Only necessary for \code{save.location = "path"}
#'
#' @param textcleaner Boolean.
#' Argument for skipping asking to save the dictionary twice.
#' Defaults to \code{FALSE}.
#' If \code{TRUE}, then asking to save the dictionary will be skipped.
#' The existing dictionary is the argument in \code{...} whose expression
#' contains "dictionary" (e.g., \code{full_dictionary}); if there is none, it is
#' all arguments after the first one. Everything else is treated as new words.
#' When the dictionary file already exists, only the new words are added to it. The
#' words of the dictionary argument that the file does not have are not saved, which
#' gives a warning of class \code{"semnet_dictionary_words_not_saved"}
#'
#' @param package Boolean.
#' Defunct. It was used to update the package's own dictionaries and is no longer available
#'
#' @return If \code{save.location = "envir"}, the sorted, unique, lower case
#' words (vector). Otherwise, the dictionary is saved to a file and \code{NULL}
#' is returned invisibly
#'
#' @details Words are added to a dictionary that is already sorted, unique, and
#' lower case (such as the vector returned by a previous call) without sorting
#' the dictionary again, which makes adding words one at a time fast.
#'
#' Appendix dictionaries are useful for storing spelling
#' definitions that are not available in the \code{\link{SemNetDictionaries}}
#' package. This function enables the storage of personalized dictionaries,
#' which can be used in combination with other dictionaries to facilitate
#' the cleaning of text data.
#'
#' Dictionaries are either stored in \code{R}'s global environment,
#' where they will be deleted once \code{R} is closed (unless you save them),
#' or in a directory you choose. A menu will pop-up asking whether you would like to
#' save or update your dictionary.
#' You have two options:
#'
#' \itemize{
#'
#' \item \code{Yes} (or \code{1}): Gives this function permission to
#' save (or update) your dictionary to a chosen directory.
#' If \code{save.location = "envir"}, your file will
#' be deleted after closing \code{R}
#'
#' \item \code{No} (or \code{2}): Does NOT give this function permission to save
#' your dictionary to your computer. \code{save.location = "envir"} will
#' always return your dictionary as a vector object to \code{R}'s
#' global environment
#'
#' }
#'
#' To save your dictionary file, you can either:
#'
#' \itemize{
#'
#' \item Manually save: Use \link{saveRDS} and save using the \code{"*.dictionary"} suffix
#'
#' \item \code{save.location = "choose"}: A file explorer menu will pop-up and a
#' directory can be manually selected
#'
#' \item \code{save.location = "path"}: The file will automatically be saved to the
#' directory you provide
#'
#' }
#'
#' Note that \code{save.location = "choose"} and \code{save.location = "path"} will
#' automatically update your dictionary if there is a file with the same name enter
#' into the \code{dictionary.name} argument.
#'
#' To find where your dictionaries are stored, use the
#' \code{\link[SemNetDictionaries]{find.dictionaries}} function.
#' These dictionaries are only stored on
#' your private computer and must either be publicly shared or
#' transferred to other computers in order to use them elsewhere.
#' If you would like to share a dictionary for others to use, then please submit
#' a pull request or post an issue with your dictionary on my GitHub:
#' \href{https://github.com/AlexChristensen/SemNetDictionaries}{AlexChristensen/SemNetDictionaries}.
#'
#' @examples
#' # Create a dictionary
#' new.dictionary <- append.dictionary(c("words","are","fun"), save.location = "envir")
#'
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#'
#' @seealso \code{\link{find.dictionaries}} to find where dictionaries are stored,
#' \code{\link{dictionaries}} to identify dictionaries in
#' \code{\link{SemNetDictionaries}}
#'
#' @importFrom utils menu
#'
#' @export
# Appendix Dictionary
# Updated 14.10.2026
append.dictionary <- function(...,
                              dictionary.name = "appendix",
                              save.location = c("envir","wd","choose","path"),
                              path = NULL,
                              textcleaner = FALSE,
                              package = FALSE)
{
    # Defunct maintainer option
    if(isTRUE(package))
    {
        stop("'package = TRUE' is defunct. It was used to update the package's own dictionaries.",
             call. = FALSE)
    }

    # Get words
    word.list <- list(...)

    # Save location for appendix dictionary
    if(missing(save.location))
    {save.location <- "envir"
    }else{save.location <- match.arg(save.location)}

    # Separate the existing dictionary from new words
    if(textcleaner)
    {
        dicts <- .dictionary_arguments(substitute(list(...)), length(word.list))

        if(length(dicts) != 0L)
        {
            # Existing dictionary (new words are not in it)
            if(length(dicts) == 1L)
            {dict.words <- word.list[[dicts]]
            }else{dict.words <- unique(sort(tolower(unlist(word.list[dicts], use.names = FALSE))))}

            new.words <- tolower(unlist(word.list[-dicts], use.names = FALSE))
        }else{
            dict.words <- NULL
            new.words <- tolower(unlist(word.list, use.names = FALSE))
        }

        # Words that are not in the dictionary
        new.words <- unique(new.words[!new.words %in% dict.words])
    }

    # Where the dictionary is stored
    if(save.location == "envir")
    {
        # Files in environment
        sav.files <- ls(envir = globalenv())

        # Create appendix dictionary alias
        append.data <- paste(dictionary.name, "dictionary", sep = ".")

    }else{
        if(save.location == "wd")
        {
            # Path to working directory
            path <- getwd()

        }else if(save.location == "choose")
        {
            # Let user select path
            path <- .choose_directory()

        }else if(save.location == "path") # Check if path exists
        {
            # Stop function if path is not input
            if(is.null(path))
            {stop("A 'path' must be specified.")}

            if(!dir.exists(path))
            {stop("'path' does not exist.")}
        }

        # Save location with path
        sav.loc <- path

        # Identify saved files in save location
        sav.files <- list.files(sav.loc)

        # Create appendix dictionary alias
        append.data <- paste(dictionary.name, "dictionary", "rds", sep = ".")
    }

    # Check if appendix dictionary exists
    exists <- append.data %in% sav.files

    if(exists)
    {
        # Put new words into vector
        if(!textcleaner)
        {new.words <- tolower(unlist(word.list, use.names = FALSE))}

        if(save.location != "envir")
        {
            # Load appendix dictionary
            append.words <- readRDS(paste(sav.loc, append.data, sep = "/"))

            # With textcleaner, only the words that are not in the dictionary argument are added,
            # so the words of the dictionary argument that the file does not have are not saved
            if(textcleaner && length(dict.words) != 0L)
            {
                not.saved <- unique(tolower(dict.words))
                not.saved <- not.saved[!not.saved %in% append.words]

                if(length(not.saved) != 0L)
                {
                    warning(structure(
                        class = c("semnet_dictionary_words_not_saved", "warning", "condition"),
                        list(
                            message = paste0(
                                "'", append.data, "' already exists, so the ", length(not.saved),
                                " word(s) in the dictionary argument that are not in the file were not ",
                                "added (with textcleaner = TRUE, only words that are not in the dictionary ",
                                "argument are added to an existing file). To add them, use ",
                                "append.dictionary() without textcleaner = TRUE."
                            ),
                            call = NULL,
                            words = not.saved
                        )
                    ))
                }
            }

        }else{
            # Load appendix dictionary
            if(textcleaner)
            {append.words <- dict.words
            }else{append.words <- get(paste(dictionary.name, "dictionary", sep = "."), envir = globalenv())}
        }

        # Combine appendix dictionary with new words
        append.words <- .combine_words(append.words, new.words,
                                       new.only = textcleaner && save.location == "envir")

    }else{
        # Put new words into vector
        if(textcleaner)
        {append.words <- .combine_words(dict.words, new.words, new.only = TRUE)
        }else{append.words <- unique(sort(tolower(unlist(word.list, use.names = FALSE))))}
    }

    # Envir
    if(save.location == "envir")
    {
        # Let user know
        if(!textcleaner)
        {message(if(exists) "Dictionary has been updated" else "Dictionary has been created")
        }else if(length(new.words) != 0L)
        {message(paste("\nResponse was ADDED TO DICTIONARY:", paste("'", new.words, "'", sep = "")))}

        # Give back updated words
        return(append.words)
    }

    # Ask if user would like to save dictionary
    if(save.location == "choose" && !textcleaner)
    {ans <- yes.no.menu(title = if(exists) "Update your dictionary" else "Save your dictionary")
    }else{ans <- 1}

    # Save dictionary
    if(ans == 1)
    {
        saveRDS(append.words, file = file.path(paste(sav.loc, append.data, sep = "/")), version = 2)

        # Let user know that the dictionary has been saved
        if(exists)
        {message(paste("\n", append.data, " has been updated.", sep = ""))
        }else{
            message(paste("\nA new dictionary file was created in: '",
                          paste(sav.loc, append.data, sep = "/"), "'", sep = ""))
        }

    }else{
        # Let user know that the dictionary was not saved
        message(paste("\n", append.data, if(exists) " was not updated." else " was not saved.", sep = ""))
    }

    invisible(NULL)
}
#----

#' Which arguments are the existing dictionary?
#'
#' @description For \code{append.dictionary(textcleaner = TRUE)}. String
#' literals are always new words (even the word "dictionary"). Otherwise, an
#' argument whose expression contains "dictionary" is a dictionary. If there is
#' none, all arguments after the first one are dictionaries.
#'
#' @param call The call \code{substitute(list(...))}
#' @param n Number of arguments
#'
#' @return Indices of the dictionary arguments
#'
#' @noRd
# Dictionary arguments----
# Updated 14.10.2026
.dictionary_arguments <- function(call, n)
{
    args <- as.list(call)[-1L]

    if(length(args) != n) # Arguments could not be matched (e.g., ... forwarded)
    {return(if(n > 1L) seq_len(n)[-1L] else integer(0))}

    named <- vapply(args, function(a)
    {
        !is.character(a) && grepl("dictionary", paste(deparse(a, nlines = 1L), collapse = ""),
                                  ignore.case = TRUE)
    }, logical(1))

    if(any(named))
    {return(which(named))}

    if(n > 1L) seq_len(n)[-1L] else integer(0)
}
