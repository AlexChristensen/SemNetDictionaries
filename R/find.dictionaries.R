#' Finds Locations of Dictionaries
#' @description A wrapper function to identify the save location
#' of appendix dictionaries from \code{\link[SemNetDictionaries]{append.dictionary}}
#' 
#' @return Returns a list containing:
#' 
#' \item{location}{The location where \code{R} saves the
#' dictionaries}
#' 
#' \item{files}{The dictionary files that are stored in the location. If there is no output
#' (e.g., \code{character(0)}), then no appendix dictionary file exists
#' (one can be created using the \code{\link[SemNetDictionaries]{append.dictionary}} function)}
#' 
#' @details This function identifies where the
#' \code{\link[SemNetDictionaries]{append.dictionary}} function
#' has stored appendix dictionaries. The default for the
#' \code{\link[SemNetDictionaries]{append.dictionary}} function
#' is to store dictionaries in your \code{R} packages folder.
#' This folder is the default for where \code{R} stores your downloaded and installed packages.
#' 
#' @examples 
#' 
#' find.dictionaries()
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{append.dictionary}} to create a new dictionary,
#' \code{\link{dictionaries}} to identify available dictionaries,
#' \code{\link{extra.dictionaries}} to see appendix dictionaries not in
#' \code{\link{SemNetDictionaries}}
#' 
#' @export
#Find Dictionaries
find.dictionaries <- function()
{
    #locate path to R libraries
    paths <- .libPaths()
    
    #look through libraries
    for(i in 1:length(paths))
    {
        #identify packages in libraries
        files <- list.files(paths[i])
        
        #search for SemNetDictionaries package in libraries
        if("SemNetDictionaries" %in% files)
        {
            #if directory for dictionaries does not exist,
            #then create one
            if(!"SemNet_Dictionaries" %in% files)
            {
                #create directory for appendix dictionaries
                dir.create(paste(paths[i],"SemNet_Dictionaries",sep="/"))
                
                #identify packages in libraries
                files <- list.files(paths[i])
            }
            
            #find SemNetDictionaries package
            target <- which(files=="SemNet_Dictionaries")
            
            #end loop
            break
        }else{target <- NULL}
    }
    
    #save the location of SemNetDictionaries package
    if(!is.null(target))
    {
        #location for data files
        sav.loc <- paste(paths[i],files[target],sep="/")
        
        #identify data files in SemNetDictionaries package
        sav.files <- list.files(sav.loc)
    
        #initialize result list
        res <- list()
        res$location <- sav.loc #location of dictionaries
        res$files <- sav.files #dictionaries
        
        return(res)
    }else{stop("Could not find 'SemNetDictionaries' package in your library")}
}
#----