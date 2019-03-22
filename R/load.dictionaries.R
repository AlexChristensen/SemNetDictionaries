#' Load Dictionaries
#' @description A wrapper function for to load text dictionaries in
#' \code{\link{SemNetCleaner}}
#' 
#' @param dictionary Character vector.
#' Dictionaries to load (see examples)
#' 
#' @return Returns a vector of words in specified dictionaries
#' 
#' @examples 
#'
#' load.dictionaries("animals")
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
        
        ###############################
        ####START LOAD DICTIONARIES####
        ###############################
        
        #initialize dictionary list
        dict.list <- list()
        
        #update dictionaries with user-defined dictionaries
        up.dict <- SemNetDictionaries::extra.dictionaries()
        loc <- SemNetDictionaries::find.dictionaries()$location
        fil <- SemNetDictionaries::find.dictionaries()$files
        
        #identify number of dictionaries
        for(i in 1:length(dictionary))
        {
            #grab name of dictionary or dictionaries
            str.vec <- unlist(strsplit(dictionary[i],split="[.]"))
            
            #add dictionary to end of dict.name if it's not there
            if(!"dictionary" %in% str.vec)
            {dict <- paste(str.vec,"dictionary",sep=".")}
            
            if(dict %in% SemNetDictionaries::dictionaries)
            {
                dict.list[[i]] <- get(data(list = dict,envir = environment()))
            }else if(dict %in% up.dict)
            {
                #target dictionary name
                target.dict <- which(up.dict==dict)
                
                #load into dictionary list
                dict.list[[i]] <- get(load(paste(loc,fil[target.dict],sep="/")))
                
            }else{message(paste(dict,"was not found in existing dictionaries.",sep=" "))}
        }
        
    }
    
    #combine into vector and alphabetize
    full.dict <- sort(unlist(dict.list))
    
    #############################
    ####END LOAD DICTIONARIES####
    #############################
    
    return(full.dict)
}
#----