#' List Names of Dictionaries in 'SemNetDictionaries'
#' @description A wrapper function to identify all dictionaries included in 
#' \code{\link{SemNetDictionaries}}
#' 
#' @return Returns the names of dictionaries in \code{\link{SemNetDictionaries}}
#' 
#' @examples
#' dictionaries()
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{find.dictionaries}} to find where dictionaries are stored,
#' \code{\link{append.dictionary}} to create a new dictionary
#' 
#' @export
#Find Dictionaries
dictionaries <- function ()
{
    #find paths to R packages
    paths <- sort(.libPaths(),decreasing=TRUE)
    
    #find 'SemNetDictionaries' package in files
    for(i in 1:length(paths))
    {
        if("SemNetDictionaries" %in% list.files(paths[i]))
        {
            #target directory
            target.dir <- paste(paths[i],"SemNetDictionaries","data",sep="/")
            
            #files in 'SemNetDictionaries'
            files <- list.files(target.dir)
            
            #target file
            target.file <- files[which(files == "Rdata.rds")]
            
            #grab dictionary names
            dict.names <- unlist(readRDS(paste(target.dir,target.file,sep="/")))
        }
        
        #exit loop
        break
    }
    
    #remove .dictionary
    dict.names <- gsub(".dictionary.*","",dict.names)
    
    return(dict.names)
}
#----