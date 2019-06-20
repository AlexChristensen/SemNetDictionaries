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
#' example.dictionary <- append.dictionary(c("words","are","fun"), save.location = "envir")
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
        #dictionary vector
        dict <- vector("character",length = length(dictionary))
        
        #remove any .dictionary if added by user
        for(i in 1:length(dictionary))
        {dict[i] <- gsub(".dictionary.*","",dictionary[i])}
        
        #add dictionary to end of dict.name for data loading
        if(!"dictionary" %in% unlist(strsplit(dict,split="[.]")))
        {dict.temp <- paste(dict,"dictionary","rds",sep=".")}
        
        #add dictionary to end of dict.name for data loading
        if(!"dictionary" %in% unlist(strsplit(dict,split="[.]")))
        {dict.env <- paste(dict,"dictionary",sep=".")}
        
        #initiate temp boolean
        temp <- vector(length=length(dictionary))
        
        #search temporary files for dictionaries
        files <- list.files(tempdir(),pattern=".dictionary.rds",full.names=TRUE,recursive = TRUE)
        
        #search global environment for dictionaries
        env.files <- ls(envir=.GlobalEnv)
        
        #grab names of dictionaries
        name <- list()
        
        #initialize count
        count <- 0
        
        #search through temp.files
        for(i in 1:length(files))
        {
            #grab only dictonary name
            dict.name <- gsub(".*/", "", files[i])
            
            if(dict.temp %in% dict.name)
            {
                count <- count + 1
                
                #remove dictionary.rds
                name[[count]] <- gsub(".dictionary.rds.*","",dict.name)
                
                #file found in temp
                temp[count] <- TRUE
            }
        }
        
        #search through env.files
        for(i in 1:length(env.files))
        {
            #grab only dictonary name
            dict.name <- gsub(".*/", "", env.files[i])
            
            if(dict.env %in% dict.name)
            {
                count <- count + 1
                
                #remove dictionary
                name[[count]] <- gsub(".dictionary*","",dict.name)
                
                #file found in temp
                temp[count] <- TRUE
                
                #add listing of where file is located
                files <- c(files,"envir")
            }
        }
    }else if(length(dictionary)==0||all(!temp))
    {
        #let user know that search is happening
        message("Searching for dictionaries...")
        
        #search computer for dictionaries
        files <- list.files(unlist(strsplit(Sys.getenv(),split=" ")$HOMEPATH),pattern=".dictionary.rds",full.names=TRUE,recursive = TRUE)
        
        #search global environment for dictionaries
        env.files <- ls(envir=.GlobalEnv)
        
        #grab names of dictionaries
        name <- list()
        
        #initialize count
        count <- length(files)
        
        #search through files
        for(i in 1:length(files))
        {
            #grab only dictonary name
            dict.name <- gsub(".*/", "", files[i])
            
            #remove dictionary.rds
            name[[i]] <- gsub(".dictionary.rds.*","",dict.name)
        }
        
        #identify dictionaries in the global environment
        env.targets <- grep(".dictionary",env.files)
        
        #search through env.files
        if(length(env.targets)!=0)
        {
            for(i in 1:length(env.targets))
            {
                count <- count + 1
                
                #remove dictionary
                name[[count]] <- gsub(".dictionary*","",env.files[env.targets[i]])
                
                #add listing of where file is located
                files <- c(files,"envir")
            }
        }
    }
    
    #results list
    res <- list()
    res$names <- unlist(name)
    res$files <- files
    
    return(res)
}
#----
