#' List Names of Dictionaries in 'SemNetDictionaries'
#' @description A wrapper function to identify all dictionaries included in 
#' \code{\link{SemNetDictionaries}}
#' 
#' @param quiet Boolean.
#' Determines whether the return should be quiet (does not print dictionaries).
#' Defaults to \code{FALSE}
#' 
#' @return Returns the names of dictionaries in \code{\link{SemNetDictionaries}}
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
#Find Dictionaries
dictionaries <- function (quiet)
{
    #missing argument
    if(missing(quiet))
    {quiet <- FALSE
    }else{quiet <- TRUE}
    
    #grab dictionary names
    data.files <- readRDS(system.file("data","Rdata.rds",package="SemNetDictionaries"))
    
    #remove .dictionary
    dicts <- gsub(".dictionary.*","",data.files)
    
    #target dictionarys
    dict.names <- dicts[which(is.na(match(data.files,dicts)))]
    
    if(!quiet)
    {
        #print dictionary names to user
        print(dict.names)
        
        #message for single letter dictionaries
        message('Single letter dictionaries can also be used (e.g., "f")')
        
        #suppress return
        invisible(dict.names)
    }else{return(dict.names)}
}
#----