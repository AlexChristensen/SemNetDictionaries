#Stop Words Dictionary---
#' Stop Words Dictionary
#'
#' A selection of stop words that can be removed from semantic responses (\emph{n} = \Sexpr[results=text,stage=render]{length(SemNetDictionaries::stop_words.dictionary)})
#' 
#' To add additional animals to the dictionary, please make an
#' appendix dictionary (\code{\link[SemNetDictionaries]{append.dictionary}})
#' 
#' @name stop_words.dictionary
#' 
#' @docType data
#' 
#' @usage data(stop_words.dictionary)
#' 
#' @format stop_words.dictionary (vector, length = \Sexpr[results=text,stage=render]{length(SemNetDictionaries::stop_words.dictionary)})
#' 
#' @keywords datasets
#' 
#' @examples 
#' data("stop_words.dictionary")
NULL
#----