#' Finds Names and Locations of Appendix Dictionaries
#' @description A wrapper function to identify the save location
#' of appendix dictionaries from \code{\link[SemNetDictionaries]{append.dictionary}}
#' 
#' @return 
#' 
#' \item{names}{Returns the names of the appendix dictionary file(s)}
#' 
#' \item{files}{Returns the dictionary file(s) that are stored in each given path. If there is no output
#' (e.g., \code{character(0)}), then no appendix dictionary file exists
#' (one can be created using the \code{\link[SemNetDictionaries]{append.dictionary}} function)}
#' 
#' @details This function identifies where the
#' \code{\link[SemNetDictionaries]{append.dictionary}} function
#' has stored appendix dictionaries. The default for the
#' \code{\link[SemNetDictionaries]{append.dictionary}} function
#' is to store dictionaries in your temporary \code{R} data folder.
#' If you do not know where this is, a path is provided to you
#' in the output of this function (see \code{files} below).
#' 
#' @examples 
#' \dontrun{
#' 
#' #No appendix dictionaries found
#' find.dictionaries()
#' 
#' #Make a dictionary
#' append.dictionary(c("words","are","fun"))
#' 
#' #Dictionary can now be found
#' find.dictionaries()
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{append.dictionary}} to create a new dictionary,
#' \code{\link{dictionaries}} to identify dictionaries in
#' \code{\link{SemNetDictionaries}}
#' 
#' @export
#Find Dictionaries
find.dictionaries <- function()
{
    #let user know that search is happening
    message("Searching for dictionaries...")
    
    #search computer for dictionaries
    files <- list.files("/Users",pattern=".dictionary.rds",full.names=TRUE,recursive = TRUE)
    
    #grab names of dictionaries
    name <- vector("character", length = length(files))
    for(i in 1:length(files))
    {
        #grab only dictonary name
        dict.name <- gsub(".*/", "", files[i])
        
        #remove dictionary.rds
        name[i] <- gsub(".dictionary.rds.*","",dict.name)
    }
    
    #results list
    res <- list()
    res$names <- name
    res$files <- files
    
    return(res)
}
#----
