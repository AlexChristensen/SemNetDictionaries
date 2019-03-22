#' Load Dictionaries
#' @description A wrapper function for to load text dictionaries into
#' \code{\link{SemNetCleaner}}. Searches for dictionaries in 
#' \code{\link{SemNetDictionaries}} and on your computer. Outputs 
#' a word list that is combined from all dictionaries entered 
#' in the \code{dictionary} argument
#' 
#' @param dictionary Character vector.
#' Dictionaries to load.
#' 
#' \code{\link[SemNetDictionaries]{dictionaries}} will identify dictionaries in \code{\link{SemNetDictionaries}}
#' 
#' \code{\link[SemNetDictionaries]{find.dictionaries}} will identify dictionaries on your computer
#' 
#' @return Returns a vector of words that has been combined
#' and alphabetized from specified dictionaries
#' 
#' @examples 
#' #find dictionaries to load
#' dictionaries()
#' 
#' \dontrun{
#' load.dictionaries("animals")
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @importFrom utils data
#' 
#' @export
#Load Dictionary Function
load.dictionaries <- function (dictionary)
{
    #set in case of corpus being used
    if(length(dictionary)>10)
    {dict.list <- list(dictionary)
    }else{
        
        #initialize dictionary list
        dict.list <- list()
        
        #update dictionaries with user-defined dictionaries
        find.dict <- SemNetDictionaries::find.dictionaries()
        name.dict <- find.dict$names
        path.dict <- find.dict$files
        sndict <- SemNetDictionaries::dictionaries()
        
        #identify number of dictionaries
        for(i in 1:length(dictionary))
        {
            #remove any .dictionary if added by user
            dict <- gsub(".dictionary.*","",dictionary[i])
            
            #add dictionary to end of dict.name for data loading
            if(!"dictionary" %in% dict)
            {dict.long <- paste(dict,"dictionary",sep=".")}
            
            #check if dictionary is in 'SemNetDictionaries' or on computer
            if(dict %in% sndict)
            {
                #load dictionary data
                dict.list[[i]] <- get(data(list = dict.long,envir = environment()))
                
            }else if(dict %in% name.dict)
            {
                #target dictionary in path
                target <- which(dict==name.dict)
                
                #load dictionary data
                dict.list[[i]] <- readRDS(path.dict[target])
                
            }else{message(paste(dict,"was not found in existing dictionaries.",sep=" "))}
        }
        
    }
    
    #combine into vector and alphabetize
    full.dict <- sort(unlist(dict.list))
    
    return(full.dict)
}
#----