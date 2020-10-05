#' Appendix Dictionary
#' @description A function designed to create post-hoc dictionaries in the
#' \code{\link{SemNetDictionaries}} package. This allows for new semantic categories or word lists
#' to be saved for future use (i.e., your own personal dictionary).
#' Dictionaries created using this function can either be saved as an R object to your global
#' environment or as a .rds file on your current computer. Open-source community-derived
#' dictionaries can be uploaded to and downloaded from
#' \href{https://github.com/AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}
#' 
#' @param ... Character vector.
#' A vector of words to create or add to a dictionary
#' 
#' @param dictionary.name Character.
#' Name of dictionary to create or add words to.
#' Defaults to \code{"appendix"}.
#' Input a name to create or add to an existing dictionary.
#' This function with automatically name files with the \code{"*.dictionary.rds"} suffix
#' 
#' @param save.location Character.
#' A choice for where to store appendix dictionary.
#' Defaults to \code{"envir"}.
#' 
#' \itemize{
#' 
#' \item{\code{"envir"}:}
#' {Returns dictionary as a vector object to \code{R}'s global environment}
#' 
#' \item{\code{"wd"}:}
#' {Saves dictionary to working directory. Useful for storing dictionaries
#' alongside projects}
#' 
#' \item{\code{"choose"}:}
#' {User chooses a directory for more permanent storage. This will
#' allow you to use this dictionary in the future}
#' 
#' \item{\code{"path"}:}
#' {User specifies a path to a directory if it is already known. This
#' will allow direct updates to the directory and bypass the prompts in
#' the save/update menus. This will also allow you to use
#' this dictionary in the future}
#' 
#' }
#' 
#' @param path Character.
#' A path to an existing directory.
#' Only necessary for \code{save.location = "path"}
#' 
#' @param textcleaner Boolean.
#' Argument for skipping asking to save the dictionary twice.
#' Defaults to \code{FALSE}.
#' If \code{TRUE}, then asking to save the dictionary will be skipped.
#' 
#' @param package Boolean.
#' Argument not meant for user use.
#' Allows me to update the package's dictionaries efficiently
#' 
#' @details Appendix dictionaries are useful for storing spelling
#' definitions that are not available in the \code{\link{SemNetDictionaries}}
#' package. This function enables the storage of personalized dictionaries,
#' which can be used in combination with other dictionaries to facilitate
#' the cleaning of text data.
#' 
#' Dictionaries are either stored in \code{R}'s global environment,
#' where they will be deleted once \code{R} is closed (unless you save them),
#' or in a directory you choose. A menu will pop-up asking whether you would like to
#' save or update your dictionary.
#' You have two options:
#' 
#' \itemize{
#' 
#' \item{\code{Yes} (or \code{1}):}
#' {Gives this function permission to
#' save (or update) your dictionary to a chosen directory.
#' If \code{save.location = "envir"}, your file will
#' be deleted after closing \code{R}}
#' 
#' \item{\code{No} (or \code{2}):}
#' {Does NOT give this function permission to save
#' your dictionary to your computer. \code{save.location = "envir"} will
#' always return your dictionary as a vector object to \code{R}'s
#' global environment}
#' 
#' }
#' 
#' To save your dictionary file, you can either:
#' 
#' \itemize{
#' 
#' \item{Manually save:}
#' {Use \link{saveRDS} and save using the \code{"*.dictionary"} suffix}
#' 
#' \item{\code{save.location = "choose"}:}
#' {A file explorer menu will pop-up and a directory can be manually selected}
#' 
#' \item{\code{save.location = "path"}:}
#' {The file will automatically be saved to the directory you provide}
#' 
#' }
#' 
#' Note that \code{save.location = "choose"} and \code{save.location = "path"} will
#' automatically update your dictionary if there is a file with the same name enter
#' into the \code{dictionary.name} argument.
#'
#' To find where your dictionaries are stored, use the
#' \code{\link[SemNetDictionaries]{find.dictionaries}} function.
#' These dictionaries are only stored on
#' your private computer and must either be publicly shared or
#' transferred to other computers in order to use them elsewhere.
#' If you would like to share a dictionary for others to use, then please submit
#' a pull request or post an issue with your dictionary on my GitHub:
#' \href{https://github.com/AlexChristensen/SemNetDictionaries}{AlexChristensen/SemNetDictionaries}.
#' 
#' @examples
#' # Create a dictionary
#' new.dictionary <- append.dictionary(c("words","are","fun"), save.location = "envir")
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{find.dictionaries}} to find where dictionaries are stored,
#' \code{\link{dictionaries}} to identify dictionaries in
#' \code{\link{SemNetDictionaries}}
#' 
#' @importFrom utils menu
#' 
#' @export
# Appendix Dictionary
# Updated 23.09.2020
append.dictionary <- function(...,
                              dictionary.name = "appendix",
                              save.location = c("envir","wd","choose","path"),
                              path = NULL,
                              textcleaner = FALSE,
                              package = FALSE)
{
    #get words
    word.list <- list(...)
    words <- unlist(list(...))
    
    #get names
    name <- as.character(substitute(list(...)))
    name <- name[-which(name=="list")]
    
    #check for dictionaries
    dicts <- grep(".dictionary", name)
    
    #get dictionary words
    if(length(dicts) != 0)
    {dict.words <- sort(unique(unlist(word.list[dicts])))}
    
    #check if package
    if(package)
    {
        #shortens essential argument input
        if(missing(save.location))
        {save.location <- "path"}
        
        #path to package on local pc
        path <- "D:/R Packages/SemNetDictionaries/data"
        
        #updated dictionary
        updated <- sort(unique(c(unlist(words), load.dictionaries(dictionary.name))))
        
        #set dictionary name
        dictionary <- paste(dictionary.name, ".dictionary", sep = "")
        
        #assign dictionary object
        assign(dictionary, updated, envir = environment())
        
        #path to data
        data.path <- paste(path, "/", dictionary, ".Rdata", sep = "")
        
        #save data
        save(list = dictionary, file = data.path, envir = environment(), version = 2)
        
        #escape function
        return(message(paste(dictionary, ".Rdata was updated.", sep = "")))
    }
    
    #save location for appendix dictionary
    if(missing(save.location))
    {save.location <- "envir"
    }else{save.location <- match.arg(save.location)}
    
    if(save.location == "envir")
    {
        #files in environment
        sav.files <- ls(envir = globalenv())
        
        #create appendix dictionary alias
        append.data <- paste(dictionary.name,"dictionary",sep=".")
        
    }else if(save.location != "envir")
    {
        if(save.location == "wd")
        {
            #path to working directory
            path <- getwd()
            
        }else if(save.location == "choose")
        {
            #let user select path
            path <- paste(tcltk::tkchooseDirectory(), collapse = " ")
            
        }else if(save.location == "path") #check if path exists
        {
            #stop function if path is not input
            if(is.null(path))
            {stop("A 'path' must be specified.")}
            
            if(!dir.exists(path))
            {stop("'path' does not exist.")} 
        }
        
        #save location with path
        sav.loc <- path
        
        #identify saved files in save location
        sav.files <- list.files(sav.loc)
        
        #create appendix dictionary alias
        append.data <- paste(dictionary.name,"dictionary","rds",sep=".")
    }
    
    #check if appendix dictionary exists
    if(append.data %in% sav.files)
    {
        #put new words into vector
        new.words <- unlist(words)
        
        #make them lower case
        new.words <- tolower(new.words)
        
        if(save.location != "envir")
        {
            #load appendix dictionary
            append.words <- readRDS(paste(sav.loc,append.data,sep="/"))
            
        }else if(save.location == "envir")
        {
            #load appendix dictionary
            if(textcleaner)
            {append.words <- dict.words
            }else{append.words <- get(paste(dictionary.name,"dictionary",sep="."), envir = globalenv())}
        }
        
        #combine appendix dictionary with new words
        comb.words <- c(append.words,new.words)
        
        #alphabetize and unique words combined into dictionary
        append.words <- unique(sort(comb.words))
        
        if(save.location == "envir")
        {
            #let user know
            if(!textcleaner)
            {message("Dictionary has been updated")   
            }else{message(paste("\nResponse was ADDED TO DICTIONARY:", paste("'", new.words, "'", sep = "")))}
            
            #give back updated words
            return(append.words)
            
        }else if(save.location == "choose")
        {
            #ask if user would like to save dictionary
            if(!textcleaner)
            {ans <- menu(c("Yes","No"),title="Would you like to update your saved dictionary?")
            }else{ans <- 1}
            
            if(ans == 1)
            {
                #save as updated appendix dictionary
                saveRDS(append.words, file = file.path(paste(sav.loc,append.data,sep="/")), version = 2)
                
                #let user know that the dictionary has been updated
                message(paste("\n", append.data," has been updated.",sep=""))
                
            }else if(ans == 2)
            {
                #let user know that the dictionary was not updated
                message(paste("\n", append.data," was not updated.",sep=""))
            }
            
        }else{
            #then save as updated appendix dictionary
            saveRDS(append.words, file = file.path(paste(sav.loc,append.data,sep="/")), version = 2)
            
            #let user know that the dictionary has been updated
            message(paste("\n", append.data," has been updated.",sep=""))
        }
        
    }else{
        #put new words into vector
        append.words <- unlist(words)
        
        #make them lower case
        append.words <- tolower(append.words)
        
        #alphabetize and unique words combined into dictionary
        if(textcleaner)
        {append.words <- unique(sort(c(append.words, dict.words)))
        }else{append.words <- unique(sort(append.words))}
        
        if(save.location == "envir")
        {
            #let user know
            if(!textcleaner)
            {message("Dictionary has been created")   
            }else{message(paste("\nResponse was ADDED TO DICTIONARY:", paste("'", new.words, "'", sep = "")))}
            
            #give back updated words
            return(append.words)
            
        }else if(save.location == "choose")
        {
            #ask if user would like to save dictionary
            if(!textcleaner)
            {ans <- menu(c("Yes","No"),title="Would you like to save your appendix dictionary?")
            }else{ans <- 1}
            
            if(ans == 1)
            {
                #save as new appendix dictionary
                saveRDS(append.words, file = file.path(paste(sav.loc,append.data,sep="/")), version = 2)
                
                #let user know that a new file has been saved
                message(paste("\nA new dictionary file was created in: '",
                              paste(sav.loc, append.data, sep="/"), "'", sep = ""))
                
            }else if(ans == 2)
            {
                #let user know that the dictionary was not updated
                message(paste("\n",append.data," was not saved.",sep=""))
            }
            
        }else{
            #save as new appendix dictionary
            saveRDS(append.words, file = file.path(paste(sav.loc,append.data,sep="/")), version = 2)
            
            #let user know that a new file has been saved
            message(paste("\nA new dictionary file was created in: '",
                          paste(sav.loc, append.data, sep="/"), "'", sep = ""))
        }
    }
}
#----