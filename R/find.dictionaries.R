#' Finds Names and Locations of Appendix Dictionaries
#' @description A wrapper function to identify the save location
#' of appendix dictionaries from \code{\link[SemNetDictionaries]{append.dictionary}}
#' 
#' @param ... Vector.
#' Dictionary names (if they are known).
#' If left empty, all dictionaries that can be found are returned.
#' 
#' @param add.path Character.
#' Path to a directory with additional dictionaries to be found.
#' Set to \code{"choose"} to open an interactive directory explorer
#' 
#' @param recursive Boolean.
#' Should directories be searched recursively (through all folders in them)?
#' Defaults to \code{FALSE} because searching large folders is time intensive
#' 
#' @details Dictionaries are looked for in the \code{\link{SemNetDictionaries}}
#' package, \code{R}'s global environment (objects called \code{"*.dictionary"}),
#' and \code{"*.dictionary.rds"} files in \code{add.path}, the working directory,
#' and \code{tempdir()}
#' 
#' @return 
#' 
#' \item{names}{Returns the names of the dictionaries found}
#' 
#' \item{files}{Returns where each dictionary is stored: \code{"package"},
#' \code{"envir"}, or the path to the file. If there is no output
#' (e.g., \code{character(0)}), then no dictionary was found
#' (one can be created using the \code{\link[SemNetDictionaries]{append.dictionary}} function)}
#' 
#' @examples 
#' # Make a dictionary
#' example.dictionary <- append.dictionary(c("words","are","fun"), save.location = "envir")
#'  
#' # Dictionary can now be found
#' find.dictionaries("example")
#' # All dictionaries that can be found
#' find.dictionaries()
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{append.dictionary}} to create a new dictionary,
#' \code{\link{dictionaries}} to identify dictionaries in
#' \code{\link{SemNetDictionaries}}, and
#'  \code{\link{load.dictionaries}} to load multiple dictionaries
#'   
#' @export
# Find Dictionaries
# Updated 14.10.2026
find.dictionaries <- function(..., add.path = NULL, recursive = FALSE)
{
    # Grab requested dictionary names
    requested <- unlist(list(...), use.names = FALSE)
    requested <- requested[!is.na(requested)]

    # Resolve additional path
    add.path <- .resolve_add_path(add.path)

    # Dictionaries in 'SemNetDictionaries'
    pkg.names <- dictionaries(TRUE)
    names <- pkg.names
    files <- rep("package", length(pkg.names))

    # Dictionaries in the global environment
    env.objects <- ls(envir = globalenv(), pattern = "\\.dictionary$")
    env.objects <- env.objects[vapply(env.objects, function(x)
        is.character(get(x, envir = globalenv(), inherits = FALSE)), logical(1))]
    names <- c(names, sub("\\.dictionary$", "", env.objects))
    files <- c(files, rep("envir", length(env.objects)))

    # Dictionary files in additional path, working directory, and tempdir()
    for(dir in unique(c(add.path, getwd(), tempdir())))
    {
        found <- list.files(dir, pattern = "\\.dictionary\\.rds$", full.names = TRUE,
                            recursive = recursive)
        names <- c(names, sub("\\.dictionary\\.rds$", "", basename(found)))
        files <- c(files, gsub("\\\\", "/", found))
    }

    # Target requested dictionaries
    if(length(requested) != 0L)
    {
        clean <- .clean_name(requested, "dictionary")
        keep <- names %in% clean | (files == "package" & names %in% tolower(clean))
        names <- names[keep]
        files <- files[keep]
    }

    # Results list
    res <- list()
    res$names <- names
    res$files <- files

    return(res)
}
#----
