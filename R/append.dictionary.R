#' Appendix Dictionary
#' @description A function designed to create post-hoc dictionaries in the
#' \code{\link{SemNetDictionaries}} package. This allows for new semantic categories or word lists
#' to be saved for future use. THIS IS YOUR OWN PERSONAL DICTIONARY.
#' Dictionaries created using this function can either be saved as an R object to your global
#' environment or as a .rds file on your current computer. Open-source community-derived
#' dictionaries can be uploaded to and downloaded from
#' \href{AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}
#' 
#' @param ... Character vector.
#' A vector of words to create or add to a dictionary
#' 
#' @param dictionary.name Character.
#' Name of dictionary to create or add words to.
#' Defaults to \code{"appendix"}.
#' Input a name to create or add to an existing dictionary.
#' This function with automaticaly name files with the \code{"*.dictionary.rds"} suffix.
#' 
#' @param save.location Character.
#' A choice for where to store appendix dictionary.
#' Defaults to \code{"envir"}.
#' 
#' \code{"envir"} will return dictionary as a vector object
#' to R's global environment
#' 
#' \code{"choose"} allows you to choose a directory for
#' more permenant storage. This will allow you to use this dictionary
#' in the future
#' 
#' \code{"path"} allows you to specify a path to a directory
#' if it is already known. This will allow direct updates to the directory
#' and bypass the prompts in the save/update menus. This will also
#' allow you to use this dictionary in the future
#' 
#' @param path Character.
#' A path to an existing directory
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
#' \code{No} (or \code{2}) will not give this function permission to save
#' your dictionary to your computer. \code{save.location = "envir"} will
#' always return your dictionary as a vector object to \code{R}'s
#' global environment
#' 
#' \code{Yes} (or \code{1}) will give this function permission to
#' save (or update) your dictionary to a chosen directory.
#' If \code{save.location = "envir"}, your file will
#' be deleted after closing \code{R} (unless manually saved).
#' 
#' To save your dictionary file, you can either:
#' 
#' Manually save: use \link{saveRDS} and save using the \code{"*.dictionary"} suffix
#' 
#' \code{save.location = "choose"}: a file explorer menu will pop-up and a directory
#' can be selected
#' 
#' \code{save.location = "path"}: the file will automatically be saved to the directory
#' you provide
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
#' \href{AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}.
#' 
#' @examples
#' #create a dictionary
#' new.dictionary <- append.dictionary(c("words","are","fun"))
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
#Appendix Dictionary
append.dictionary <- function(...,
                              dictionary.name = "appendix",
                              save.location = c("envir","choose","path"),
                              path = NULL)
{
    #grab words
    words <- list(...)
    
    #save location for appendix dictionary
    if(missing(save.location))
    {save.location <- "envir"
    }else{save.location <- match.arg(save.location)}
    
    if(save.location == "envir")
    {
        #files in environment
        sav.files <- ls(envir=.GlobalEnv)
        
        #create appendix dictionary alias
        append.data <- paste(dictionary.name,"dictionary",sep=".")
        
    }else if(save.location != "envir")
    {
        
        if(save.location == "choose")
        {
            #let user select path
            path <- tcltk::tk_choose.dir()
            
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
            append.words <- readRDS(paste(sav.loc,append.data,sep="\\"))
            
        }else if(save.location == "envir")
        {
            #load appendix dictionary
            append.words <- get(paste(dictionary.name,"dictionary",sep="."))
        }
        
        #combine appendix dictionary with new words
        comb.words <- c(append.words,new.words)
        
        #alphabetize and unique words combined into dictionary
        append.words <- unique(sort(comb.words))
        
        if(save.location == "envir")
        {
            #let user know
            message("Dictionary has been updated")
            
            #give back updated words
            return(append.words)
            
        }else if(save.location == "choose")
        {
            #ask if user would like to save dictionary
            ans <- menu(c("Yes","No"),title="Would you like to update your saved dictionary?")
            
            if(ans == 1)
            {
                #save as updated appendix dictionary
                saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
                
                #let user know that the dictionary has been updated
                message(paste(append.data," has been updated.",sep=""))
                
            }else if(ans == 2)
            {
                #let user know that the dictionary was not updated
                message(paste(append.data," was not updated.",sep=""))
            }
            
        }else if(save.location == "path")
        {
            #then save as updated appendix dictionary
            saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
            
            #let user know that the dictionary has been updated
            message(paste(append.data," has been updated.",sep=""))
        }
        
    }else{
        #put new words into vector
        append.words <- unlist(words)
        
        #make them lower case
        append.words <- tolower(append.words)
        
        #alphabetize and unique words combined into dictionary
        append.words <- unique(sort(append.words))
        
        if(save.location == "envir")
        {
            #give back words
            return(append.words)
            
        }else if(save.location == "choose")
        {
            #ask if user would like to save dictionary
            ans <- menu(c("Yes","No"),title="Would you like to save dictionary?")
            
            if(ans == 1)
            {
                #save as new appendix dictionary
                saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
                
                #let user know that a new file has been saved
                message(paste("A new dictionary file was created in:\n",
                              paste(sav.loc,append.data,sep="\\"),"\n"))
                
            }else if(ans == 2)
            {
                #let user know that the dictionary was not updated
                message(paste(append.data," was not saved.",sep=""))
            }
            
        }else if(save.location == "path")
        {
            #save as new appendix dictionary
            saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
            
            #let user know that a new file has been saved
            message(paste("A new dictionary file was created in:\n",
                          paste(sav.loc,append.data,sep="\\"),"\n"))
        }
    }
}
#----