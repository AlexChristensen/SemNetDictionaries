#' List Names of Dictionaries in 'SemNetDictionaries'
#' @description A wrapper function to identify all dictionaries included in 
#' \code{\link{SemNetDictionaries}}
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
dictionaries <- function ()
{
    #grab dictionary names
    data.files <- readRDS(system.file("data","Rdata.rds",package="SemNetDictionaries"))
    
    #remove .dictionary
    dicts <- gsub(".dictionary.*","",data.files)
    
    #target dictionarys
    dict.names <- dicts[which(is.na(match(data.files,dicts)))]
    
    return(dict.names)
}
#----