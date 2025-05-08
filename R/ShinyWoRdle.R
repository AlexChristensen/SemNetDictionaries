#' @title Shiny App to Play WoRdle
#'
#' @description An interactive Shiny application for playing WoRdle
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