#' Appendix Dictionary
#' @description A function designed to post-hoc update the dictionaries in the
#' \code{\link{SemNetDictionaries}}. This function is used to add
#' words that are not included in \code{\link{SemNetDictionaries}}'
#' dictionaries. This allows for new semantic categories or words to be included
#' for future use. THIS IS YOUR OWN PERSONAL DICTIONARY. Dictionaries created using this function
#' will ONLY be available on your current computer. An open-source community-derived
#' dictionaries can be uploaded to and downloaded from
#' \href{AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}
#' 
#' @param ... Character vector.
#' A vector of words to create or add to dictionary
#' 
#' @param dict.name Characters.
#' Name of dictionary to create or add words to.
#' Defaults to \code{"appendix.dictionary"}.
#' Input a name to create or add to an existing dictionary
#' 
#' @details Dictionaries are stored in your \code{R} packages library.
#' To find where your dictionaries are stored, use the
#' \code{\link[SemNetDictionaries]{find.dictionaries}} function. A new
#' folder is created, so that your appendix dictionary files can be stored
#' without deletion when updating the \code{\link{SemNetDictionaries}}
#' package. These dictionaries are only stored on your private computer
#' and must either be publicly shared or transferred to other computers in
#' order to use them elsewhere. Please see the documentation for
#' \code{\link{SemNetDictionaries}} for more details.
#' 
#' Appendix dictionaries are useful for storing spelling
#' definitions that are not available in the \code{\link{SemNetDictionaries}}
#' package. This function enables the storage of personalized dictionaries,
#' which can be used in combination with other dictionaries to facilitate
#' the cleaning of text data. Although these dictionaries are personal,
#' they can also be shared for other users. Please submit a pull request
#' with your dictionary to be added to the package at: \href{AlexChristensen/SemNetDictionaries}{https://github.com/AlexChristensen/SemNetDictionaries}
#' 
#' @examples
#' 
#' append.dictionary(c("words","are","fun"))
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @seealso \code{\link{find.dictionaries}} to find where dictionaries are stored,
#' \code{\link{dictionaries}} to identify available dictionaries,
#' \code{\link{extra.dictionaries}} to see appendix dictionaries not in
#' \code{\link{SemNetDictionaries}}
#' 
#' @export
#Appendix Dictionary
append.dictionary <- function(..., dict.name)
{
    #grab words
    words <- list(...)
    
    #find dictionaries
    dicts <- SemNetDictionaries::find.dictionaries()
    
    sav.loc <- dicts$location
    sav.files <- dicts$files
    
    #create appendix dictionary alias
    if(missing(dict.name))
    {append.data <- "appendix.dictionary.Rdata"
    }else{append.data <- paste(dict.name,"Rdata",sep=".")}
    
    #check if appendix dictionary exists
    if(append.data %in% sav.files)
    {
        #put new words into vector
        new.words <- unlist(words)
        
        #make them lower case
        new.words <- tolower(new.words)
        
        #load appendix dictionary
        load(file = paste(sav.loc,append.data,sep="/"))
        
        #combine appendix dictionary with new words
        comb.words <- c(append.words,new.words)
        
        #alphabetize and unique words combined into dictionary
        append.words <- unique(sort(comb.words))
        
        #save as new appendix dictionary
        save(append.words, file = paste(sav.loc,append.data,sep="/"))
        
        #let them know the dictionary has been updated
        message(paste(append.data," has been updated.",sep=""))
    }else{
        
        #put new words into vector
        append.words <- unlist(words)
        
        #make them lower case
        append.words <- tolower(append.words)
        
        #alphabetize and unique words combined into dictionary
        append.words <- unique(sort(append.words))
        
        #warn them that a new file has been saved
        message(paste("A new file was created in your R library:\n",
                      paste(sav.loc,append.data,sep="/"),
                      "\n\nSee documentation: ?append.dictionary",sep="")
                )
        
        #save as new appendix dictionary
        save(append.words, file = paste(sav.loc,append.data,sep="/"))
    }
}
#----