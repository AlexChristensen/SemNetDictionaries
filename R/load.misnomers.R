#' Load Misnomers
#' @description A wrapper function to load misnomers into
#' the 'SemNetCleaner' package. Searches for misnomers in \code{R}'s
#'  \code{\link{SemNetDictionaries}} package. Outputs a unique word list
#' that is combined from all dictionaries entered in the \code{misnomer} argument
#' 
#' @param misnomer Character vector.
#' Misnomers to load (must be a dictionary in
#' \code{\link[SemNetDictionaries]{dictionaries}})
#' 
#' @return Returns a vector of unique words that have been combined
#' and alphabetized from the specified misnomers
#' 
#' @examples 
#' #find dictionaries to load
#' dictionaries()
#' 
#' #load "animals" misnomers
#' load.misnomers("animals")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @export
#Load Misnomer Function
load.misnomers <- function (misnomer)
{
    #set in case of corpus being used
    if(length(misnomer)>10)
    {misn.list <- list(misnomer)
    }else{
        
        #initialize misnomer list
        misn.list <- list()
        
        #look in 'SemNetDictionaries'
        sndict <- SemNetDictionaries::dictionaries()
        
        if(all(misnomer %in% sndict))
        {
            for(i in 1:length(misnomer))
            {
                #remove any .misnomer if added by user
                misn <- gsub(".misnomer.*","",misnomer[i])
                
                #add dictionary to end of misn.name for data loading
                if(!"misnomer" %in% misn)
                {misn.long <- paste(misn,"misnomer",sep=".")}
                
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