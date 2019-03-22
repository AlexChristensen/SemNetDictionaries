#' Appendix Dictionary
#' @description A function designed to post-hoc update the dictionaries in the
#' \code{\link{SemNetDictionaries}}. This function is used to add
#' words that are not included in \code{\link{SemNetDictionaries}}'
#' dictionaries. This allows for new semantic categories or word lists to be included
#' for future use. THIS IS YOUR OWN PERSONAL DICTIONARY. Dictionaries created using this function
#' will ONLY be available on your current computer. Open-source community-derived
#' dictionaries can be uploaded to and downloaded from
#' \href{AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}
#' 
#' @param ... Character vector.
#' A vector of words to create or add to a dictionary
#' 
#' @param dictionary.name Character.
#' Name of dictionary to create or add words to.
#' Defaults to \code{"appendix.dictionary"}.
#' Input a name to create or add to an existing dictionary
#' 
#' @param save.location Character.
#' A choice for where to store appendix dictionary.
#' Defaults to \code{"temp"}.
#' 
#' \code{"temp"} creates a temporary
#' dictionary that is deleted once \code{R} is closed
#' 
#' \code{"choose"} allows you to choose a directory for
#' more permenant storage. This will allow you to use this dictionary
#' in the future
#' 
#' @details Appendix dictionaries are useful for storing spelling
#' definitions that are not available in the \code{\link{SemNetDictionaries}}
#' package. This function enables the storage of personalized dictionaries,
#' which can be used in combination with other dictionaries to facilitate
#' the cleaning of text data.
#' 
#' Dictionaries are either stored in \code{R}'s temporary files,
#' where they will be deleted once \code{R} is closed, or in a location
#' that you choose. A menu will pop-up asking whether you would like to
#' save or update your dictionary (\code{"Save or update dictionary?"}).
#' You have two options:
#' 
#' \code{Yes} (or \code{1}) will give this function permission to
#' save (or update) your dictionary to either your temporary folder
#' or chosen directory. If \code{save.location = "temp"}, your file will
#' still be deleted after closing \code{R}. The only way to store your
#' dictionary on your computer is by choosing \code{save.location = "choose"}.
#' If \code{save.location = "choose"} is input, then dictionaries in that directory
#' with the same name as \code{dictionary.name} (e.g., "appedix.dictionary") will be updated.
#' 
#' \code{No} (or \code{2}) will not give this function permission to save
#' your dictionary to your computer. The dictionary will automatically be
#' saved to your temporary folder where it will be deleted upon exiting \code{R}.
#'
#' To find where your dictionaries are stored, use the
#' \code{\link[SemNetDictionaries]{find.dictionaries}} function.
#' 
#' These dictionaries (temporary and otherwise) are only stored on
#' your private computer and must either be publicly shared or
#' transferred to other computers in order to use them elsewhere.
#' Please see the documentation for \code{\link{SemNetDictionaries}} for more details.
#' If you would like to share a dictionary for others to use, then please submit
#' a pull request or post an issue with your dictionary:
#' \href{AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}.
#' 
#' @examples
#' \dontrun{
#' 
#' append.dictionary(c("words","are","fun"))
#' }
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
                              dictionary.name,
                              save.location = c("temp","choose"))
{
    #grab words
    words <- list(...)
    
    #save location for appendix dictionary
    if(missing(save.location))
    {save.location <- "temp"
    }else{save.location <- match.arg(save.location)}
    
    if(save.location == "temp")
    {
        #create temporary save location
        sav.loc <- tempdir()
        
    }else if(save.location == "choose")
    {
        #let user select path
        sav.loc <- tcltk::tk_choose.dir()
    }
    
    #identify saved files in save location
    sav.files <- list.files(sav.loc)
    
    #create appendix dictionary alias
    if(missing(dictionary.name))
    {
        append.data <- "appendix.dictionary.rds"
        
        #let user know what dictionary was named
        message("No 'dictionary.name' entered. Dictionary was named 'appendix.dictionary'")
        
    }else{append.data <- paste(dictionary.name,"dictionary","rds",sep=".")}
    
    #check if appendix dictionary exists
    if(append.data %in% sav.files)
    {
        #put new words into vector
        new.words <- unlist(words)
        
        #make them lower case
        new.words <- tolower(new.words)
        
        #load appendix dictionary
        append.words <- readRDS(paste(sav.loc,append.data,sep="\\"))
        
        #combine appendix dictionary with new words
        comb.words <- c(append.words,new.words)
        
        #alphabetize and unique words combined into dictionary
        append.words <- unique(sort(comb.words))
    }else{
        #put new words into vector
        append.words <- unlist(words)
        
        #make them lower case
        append.words <- tolower(append.words)
        
        #alphabetize and unique words combined into dictionary
        append.words <- unique(sort(append.words))
    }
    
    
    #ask if user would like to save dictionary
    ans <- menu(c("Yes","No"),title="Save or update dictionary?")
    
    #if yes, then save or update
    if(ans == 1)
    {
        #check if appendix dictionary exists
        if(append.data %in% sav.files)
        {
            #save as updated appendix dictionary
            saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
            
            if(save.location == "temp")
            {
                #let user know that the dictionary has been updated
                message(paste(append.data," has been updated.\n",sep=""))
                
                #let user know that data will be deleted upon exit from R
                warning("Dictionary data will be deleted when you exit R. Use 'save.location = choose' to choose a location to permenantly save dictionary.")
            
            }else if(save.location == "choose")
            {
                #let user know that the dictionary has been updated
                message(paste(append.data," has been updated.",sep=""))
            }
        }else{
            #let user know that a new file has been saved
            message(paste("A new dictionary file was created in:\n",
                          paste(sav.loc,append.data,sep="\\"),"\n"))
            
            if(save.location == "temp")
            {
                #save as new appendix dictionary
                saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
                
                #let user know that data will be deleted upon exit from R
                warning("Dictionary data will be deleted when you exit R. Use 'save.location = choose' to choose a location to permenantly save dictionary.")
                
            }else if(save.location == "choose")
            {
                    #save as new appendix dictionary
                    saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
            }
        }
    }
    
    #save a temporary dictionary
    if(ans == 2)
    {
        #save as new appendix dictionary
        saveRDS(append.words, file = paste(sav.loc,append.data,sep="\\"))
        
        #let user know that a temporary file has been saved
        message(paste("A temporary dictionary file was created in:\n",append.data,"\n"))
        
        #let user know that data will be deleted upon exit from R
        warning("Dictionary data will be deleted when you exit R. Use 'save.location = choose' to choose a location to permenantly save dictionary.")
        
    }
}
#----