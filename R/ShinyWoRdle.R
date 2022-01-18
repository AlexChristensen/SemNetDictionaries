#' @title Shiny App to Play \href{WoRdle}{https://www.powerlanguage.co.uk/wordle/}
#' 
#' @description An interactive Shiny application for playing \href{WoRdle}{https://www.powerlanguage.co.uk/wordle/}
#' 
#' @examples
#' 
#' if(interactive())
#' {ShinyWoRdle()}
#' 
#' 
#' @export
# Shiny app for Wordle----
# Updated 18.01.2022
ShinyWoRdle <- function()
{
  shiny::runApp(appDir = system.file("ShinyWoRdle", package="SemNetDictionaries"))
}