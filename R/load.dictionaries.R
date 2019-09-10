#' Load Dictionaries
#' @description A wrapper function to load dictionaries into
#' the 'SemNetCleaner' package. Searches for dictionaries in \code{R}'s global
#' environment, the \code{\link{SemNetDictionaries}} package, and on your computer.
#' Outputs a unique word list that is combined from all dictionaries entered 
#' in the \code{dictionary} argument
#' 
#' @param dictionary Character vector.
#' Dictionaries to load
#' 
#' Dictionaries in your global environment
#' MUST be objects called \code{"*.dictionary"} (see examples).
#' 
#' \code{\link[SemNetDictionaries]{dictionaries}} will identify dictionaries in the \code{\link{SemNetDictionaries}} package
#' 
#' \code{\link[SemNetDictionaries]{find.dictionaries}} will identify dictionaries on your computer
#' 
#' @return Returns a vector of unique words that have been combined
#' and alphabetized from the specified dictionaries
#' 
#' @examples 
#' # Find dictionaries to load
#' dictionaries()
#' 
#' # Load "animals" dictionary
#' load.dictionaries("animals")
#' 
#' # Create a dictionary
#' new.dictionary <- append.dictionary(c("words", "are", "fun"))
#' 
#' # Load created dictionary
#' load.dictionaries("new")
#' 
#' # Load animals and new dictionary
#' load.dictionaries(c("animals", "new"))
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
        
        #first look in 'SemNetDictionaries' and environment dictionaries
        sndict <- SemNetDictionaries::dictionaries()
        #grab dictonaries from environment and remove ".dictionary"
        envdict <- gsub(".dictionary","",ls(envir=.GlobalEnv)[grep(".dictionary",ls(envir=.GlobalEnv))])
        #also look in package folder
        pkgdict <- gsub(".dictionary.rds","",list.files(system.file("Data", package = "SemNetDictionaries")))[grep(".dictionary",list.files(system.file("Data", package = "SemNetDictionaries")))]
        
        snenvdict <- c(sndict,envdict,pkgdict)
        
        if(all(dictionary %in% snenvdict))
        {
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
                    
                }else if(dict %in% envdict)
                {
                    #get dictionary from environment
                    dict.list[[i]] <- get(dict.long)
                }else if(dict %in% pkgdict)
                {
                    #get dictionary from package
                    dict.list[[i]] <- readRDS(paste(system.file("Data", package = "SemNetDictionaries"),"/",dict.long,".rds",sep=""))
                }
            }
        }else{
            
            #update dictionaries with user-defined dictionaries
            find.dict <- SemNetDictionaries::find.dictionaries()
            name.dict <- find.dict$names
            path.dict <- find.dict$files
            envdict <- gsub(".dictionary","",ls(envir=.GlobalEnv)[grep(".dictionary",ls(envir=.GlobalEnv))])
            
            #identify number of dictionaries
            for(i in 1:length(dictionary))
            {
                #remove any .dictionary if added by user
                dict <- gsub(".dictionary.*","",dictionary[i])
                
                #add dictionary to end of dict.name for data loading
                if(!"dictionary" %in% unlist(strsplit(dict,split="[.]")))
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
                    
                }else if(dict %in% envdict)
                {
                    #get dictionary from environment
                    dict.list[[i]] <- get(dict.long)
                
                }else{message(paste(dict,"was not found in existing dictionaries.",sep=" "))}
            }
        }
        
    }
    
    #combine into vector and alphabetize
    full.dict <- sort(unlist(dict.list))
    
    return(full.dict)
}
#----