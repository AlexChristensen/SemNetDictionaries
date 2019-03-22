#' User-defined Dictionary List
#' @description A wrapper function designed to identify the list of
#' user-defined dictionaries
#' (e.g., \code{\link[SemNetDictionaries]{append.dictionary}}).
#' 
#' @return Returns a vector of dictionaries that are not hard coded into
#' the \code{\link{SemNetDictionaries}} package
#' 
#' @examples
#' 
#' extra.dictionaries()
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{find.dictionaries}} to find where dictionaries are stored,
#' \code{\link{dictionaries}} to identify available dictionaries,
#' \code{\link{append.dictionary}} to create a new dictionary
#' 
#' @export
#Extra Dictionaries Function
extra.dictionaries <- function()
{
    #extra dictionaries
    extra <- SemNetDictionaries::find.dictionaries()$files
    
    #initialize dictionary list
    dict.list <- list()
    
    #if there are new dictionaries
    if(length(extra)!=0)
    {
        #identify dictionary names
        for(i in 1:length(extra))
        {dict.list[[i]] <- unlist(strsplit(extra[i],split=".Rdata"))}
        
        #transform to vector
        dicts <- unlist(dict.list)
        
        #remove matching dictionary names
        uniq.all <- unique(c(SemNetDictionaries::dictionaries,dicts))
        dicts <- intersect(uniq.all,dicts)
        
        return(dicts)
    }else{return(NULL)}
}
#----