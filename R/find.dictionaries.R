#' Finds Names and Locations of Appendix Dictionaries
#' @description A wrapper function to identify the save location
#' of appendix dictionaries from \code{\link[SemNetDictionaries]{append.dictionary}}
#' 
#' @param ... Vector.
#' Appendix dictionary files names (if they are known).
#' If left empty, the function will search across
#' all files for files in folders on your desktop
#' that end in \code{*.dictionary.rds}.
#' This search takes a few seconds to complete
#' (see examples for your computer's exact timing)
#' 
#' @param add.path Character.
#' Path to additional dictionaries to be found.
#' DOES NOT search recursively (through all folders in path)
#' to avoid time intensive search
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
#' # Make a dictionary
#' example.dictionary <- append.dictionary(c("words","are","fun"), save.location = "envir")
#'  
#' # Dictionary can now be found
#' find.dictionaries("example")
#' \donttest{
#' # No appendix dictionaries found
#' find.dictionaries()
#' 
#' # For your computer's timing to complete search
#' t0 <- Sys.time()
#' find.dictionaries()
#' Sys.time() - t0
#' }
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{append.dictionary}} to create a new dictionary,
#' \code{\link{dictionaries}} to identify dictionaries in
#' \code{\link{SemNetDictionaries}}
#' 
#' @export
#Find Dictionaries
# Updated 03.04.2020
find.dictionaries <- function(..., add.path = NULL)
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
        
        #grab dictionary names in 'SemNetDictionaries'
        data.files <- unname(unlist(readRDS(system.file("data","Rdata.rds",package="SemNetDictionaries"))))
        
        #search temporary files for dictionaries
        temp.files <- list.files(tempdir(),pattern=".dictionary.rds",full.names=TRUE,recursive = TRUE)
        temp.files <- gsub("/", "\\\\", temp.files)
        
        #search global environment for dictionaries
        env.files <- ls(envir=environment())
        
        #grab names of dictionaries
        name <- list()
        
        #initialize count
        count <- 0
        
        if(length(temp.files) > 0)
        {
            #search through temp.files
            for(i in 1:length(temp.files))
            {
                #grab only dictonary name
                dict.name <- gsub(".*\\\\", "", temp.files[i])
                
                if(dict.temp %in% dict.name)
                {
                    count <- count + 1
                    
                    #remove dictionary.rds
                    name[[count]] <- gsub(".dictionary.rds.*","",dict.name)
                    
                    #file found in temp
                    temp[count] <- TRUE
                }
            }
        }
        
        #search through dictionaries in package
        if(any(dict.env %in% data.files))
        {
            for(i in 1:length(dict.env))
            {
                if(dict.env[i] %in% data.files)
                {
                    count <- count + 1
                    
                    #remove dictionary
                    name[[count]] <- gsub(".dictionary*","",dict.env[i])
                    
                    #file found in package
                    temp[count] <- TRUE
                    
                    #add listing of where file is located
                    files <- c(temp.files,"package")
                }
            }
        }
        
        #search through env.files
        for(i in 1:length(env.files))
        {
            #grab only dictonary name
            dict.name <- gsub(".*\\\\", "", env.files[i])
            
            if(dict.name %in% dict.env)
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
        
    }
    
    if(length(dictionary) == 0 || all(!temp))
    {
        #let user know that search is happening
        message("Searching for dictionaries...")
        
        #grab dictionary names in 'SemNetDictionaries'
        files <- unname(unlist(readRDS(system.file("data","Rdata.rds",package="SemNetDictionaries"))))
        
        #dictionaries in 'SemNetDictionaries'
        files <- files[grep(".dictionary",files)]
        
        #add 'SemNetDictionaries' names
        name <- as.vector(gsub(".dictionary", "", files), mode = "list")
        
        #add 'SemNetDictionaries' files
        dicts <- rep("package", length(name))
        
        #search computer for dictionaries
        desk.files <- list.files(file.path(Sys.getenv("USERPROFILE"),"Desktop"), pattern=".dictionary.rds", full.names=TRUE, recursive = TRUE)
        desk.files <- gsub("\\\\", "/", desk.files)
        
        #search through downloads
        down.files <- list.files(file.path(Sys.getenv("USERPROFILE"),"Downloads"), pattern=".dictionary.rds", full.names=TRUE, recursive = TRUE)
        down.files <- gsub("\\\\", "/", down.files)
        
        #search through working directory
        wd.files <- list.files(getwd(), pattern=".dictionary.rds", full.names=TRUE, recursive = TRUE)
        wd.files <- gsub("\\\\", "/", wd.files)
        
        #search global environment for dictionaries
        env.files <- ls(envir=environment())
        
        #initialize count
        count <- length(dicts)
        
        #search through desktop files
        if(length(desk.files)!=0)
        {
            for(i in 1:length(desk.files))
            {
                #initialize count
                count <- count + 1
                
                #grab only dictonary name
                dict.name <- gsub(".*/", "", desk.files[i])
                
                #remove dictionary.rds
                name[[count]] <- gsub(".dictionary.rds.*","",dict.name)
                
                #add listing of where file is located
                files <- c(files, desk.files[i])
            }
        }
        
        #search through downloads files
        if(length(down.files)!=0)
        {
            for(i in 1:length(down.files))
            {
                #initialize count
                count <- count + 1
                
                #grab only dictonary name
                dict.name <- gsub(".*/", "", down.files[i])
                
                #remove dictionary.rds
                name[[count]] <- gsub(".dictionary.rds.*","",dict.name)
                
                #add listing of where file is located
                files <- c(files, down.files[i])
            }
        }
        
        #search through downloads files
        if(length(wd.files)!=0)
        {
            for(i in 1:length(wd.files))
            {
                #initialize count
                count <- count + 1
                
                #grab only dictonary name
                dict.name <- gsub(".*/", "", wd.files[i])
                
                #remove dictionary.rds
                name[[count]] <- gsub(".dictionary.rds.*","",dict.name)
                
                #add listing of where file is located
                files <- c(files, wd.files[i])
            }
        }
        
        #identify dictionaries in the global environment
        env.targets <- grep(".dictionary",env.files)
        
        #search through environment files
        if(length(env.targets)!=0)
        {
            for(i in 1:length(env.targets))
            {
                #initialize count
                count <- count + 1
                
                #remove dictionary
                name[[count]] <- gsub(".dictionary*","",env.files[env.targets[i]])
                
                #add listing of where file is located
                files <- c(files,"envir")
            }
        }
        
        #search additional path
        if(!is.null(add.path))
        {
            if(dir.exists(add.path))
            {
                #search through path
                path.files <- list.files(add.path,pattern=".dictionary.rds",full.names=TRUE,recursive = FALSE)
                path.files <- gsub("\\\\", "/", path.files)
                
                #search through additional path files
                if(length(path.files)!=0)
                {
                    for(i in 1:length(path.files))
                    {
                        #initialize count
                        count <- count + 1
                        
                        #grab only dictonary name
                        dict.name <- gsub(".*/", "", path.files[i])
                        
                        #remove dictionary.rds
                        name[[count]] <- gsub(".dictionary.rds.*","",dict.name)
                        
                        #add listing of where file is located
                        files <- c(files, path.files[i])
                    }
                }
                
            }else{stop("find.dictionaries(): Additional path does not exist")}
        }
    }
    
    #results list
    res <- list()
    res$names <- unlist(name)
    res$files <- files
    
    return(res)
}
#----
