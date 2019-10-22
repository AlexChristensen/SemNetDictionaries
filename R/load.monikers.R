#' Load Monikers
#' @description A wrapper function to load monikers into
#' the 'SemNetCleaner' package. Searches for monikers in \code{R}'s
#'  \code{\link{SemNetDictionaries}} package. Outputs a unique word list
#' that is combined from all dictionaries entered in the \code{moniker} argument
#' 
#' @param moniker Character vector.
#' monikers to load (must be a dictionary in
#' \code{\link[SemNetDictionaries]{dictionaries}})
#' 
#' @return Returns a vector of unique words that have been combined
#' and alphabetized from the specified monikers
#' 
#' @examples 
#' #find dictionaries to load
#' dictionaries()
#' 
#' #load "animals" monikers
#' load.monikers("animals")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Load moniker Function
load.monikers <- function (moniker)
{
    #set in case of corpus being used
    if(length(moniker)>10)
    {misn.list <- list(moniker)
    }else{
        
        #initialize moniker list
        misn.list <- list()
        
        #look in 'SemNetDictionaries'
        sndict <- SemNetDictionaries::dictionaries(TRUE)
        
        if(all(moniker %in% sndict))
        {
            for(i in 1:length(moniker))
            {
                #remove any .moniker if added by user
                misn <- gsub(".moniker.*","",moniker[i])
                
                #add dictionary to end of misn.name for data loading
                if(!"moniker" %in% misn)
                {misn.long <- paste(misn,"moniker",sep=".")}
                
                #check if dictionary is in 'SemNetDictionaries' or on computer
                if(misn %in% sndict)
                {
                    check <- tryCatch(data(list = misn.long,envir = environment()), warning = function(w) w)
                    
                    if(!is.list(check))
                    {
                        #load dictionary data
                        misn.list[[i]] <- get(data(list = misn.long,envir = environment()))
                    }else{misn.list[[i]] <- NA}
                }
            }
        }
    }
    
    #combine into vector and alphabetize
    full.misn <- sort(unlist(misn.list))
    
    return(full.misn)
}
#----