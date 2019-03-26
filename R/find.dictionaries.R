#' Finds Names and Locations of Appendix Dictionaries
#' @description A wrapper function to identify the save location
#' of appendix dictionaries from \code{\link[SemNetDictionaries]{append.dictionary}}
#' 
#' @param ... Vector.
#' Appendix dictionary files names (if they are known).
#' If left empty, the function will search across
#' all files for files that end in \code{*.dictionary.rds}.
#' This search takes about 30 seconds to complete
#' (see examples for your computer's exact timing)
#' 
#' 
#' @return 
#' 
#' \item{names}{Returns the names of the appendix dictionary file(s) found on your computer}
#' 
#' \item{files}{Returns the dictionary file(s) that are stored in each given path. If there is no output
#' (e.g., \code{character(0)}), then no appendix dictionary file exists
#' (one can be created using the \code{\link[SemNetDictionaries]{append.dictionary}} function)}
#' 
#' @examples 
#' \dontrun{
#' 
#' #No appendix dictionaries found
#' find.dictionaries()
#' 
#' #For your computer's timing to complete search
#' t0 <- Sys.time()
#' find.dictionaries()
#' Sys.time() - t0
#' }
#' 
#' #Make a dictionary
#' append.dictionary(c("words","are","fun"),
#' dictionary.name = "example",
#' save.location = "path",
#' path = tempdir())
#' 
#' #Dictionary can now be found
#' find.dictionaries("example")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{append.dictionary}} to create a new dictionary,
#' \code{\link{dictionaries}} to identify dictionaries in
#' \code{\link{SemNetDictionaries}}
#' 
#' @export
#Find Dictionaries
find.dictionaries <- function(...)
{
    #grab dictionary list
    dictionary <- unlist(list(...))
    
    #check length of list
    if(length(dictionary)!=0)
    {
        #path to temporary
        temp.path <- tempdir()
        
        #remove any .dictionary if added by user
        for(i in 1:length(dictionary))
        {dict <- gsub(".dictionary.*","",dictionary[i])}
        
        #add dictionary to end of dict.name for data loading
        if(!"dictionary" %in% unlist(strsplit(dict,split="[.]")))
        {dict.long <- paste(dict,"dictionary","rds",sep=".")}
        
        #initiate temp boolean
        temp <- vector(length=length(dictionary))
        
        #search computer for dictionaries
        files <- list.files(temp.path,pattern=".dictionary.rds",full.names=TRUE,recursive = TRUE)
        
        #grab names of dictionaries
        name <- vector("character", length = length(files))
        for(i in 1:length(files))
        {
            #grab only dictonary name
            dict.name <- gsub(".*/", "", files[i])
            
            if(dict.long %in% dict.name)
            {
                #remove dictionary.rds
                name[i] <- gsub(".dictionary.rds.*","",dict.name)
                
                #file found in temp
                temp[i] <- TRUE
            }
        }
    }else if(length(dictionary)==0||all(!temp))
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
    }
    
    #results list
    res <- list()
    res$names <- name
    res$files <- files
    
    return(res)
}
#----
