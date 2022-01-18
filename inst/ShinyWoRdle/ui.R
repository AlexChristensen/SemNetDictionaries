suppressMessages(library(shiny))
suppressMessages(library(shinyalert))
suppressMessages(library(shinyjs))
suppressMessages(library(SemNetDictionaries))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(patchwork))

# Get screen width
jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  return jsWidth;
});
'

# Interface for WoRdle----
ui <- fluidPage(

  # Get screen size
  # tags$script(jscode),
  
  # Switch view
  HTML(
    paste0(
      '<meta name="viewport" content="width=',
      tags$script(jscode),
      sep = ""
    )
  ),
  
  # Use shinyjs
  shinyjs::useShinyjs(),
  
  # Title
  titlePanel("WoRdle"),
  
  tabPanel(
    "WoRdle",
    
    # Breaks
    br(), br(),
    
      # Side bar
      sidebarPanel(
        
        width = 5,
        
        # Set number of letters in words
        fluidRow(
          column(12, align="center",
                 checkboxGroupInput("num_letters", label = "Number of Letters",
                                    choiceNames = seq(4, 9, 1),
                                    choiceValues = seq(4, 9, 1), inline = TRUE,
                                    selected = 5
                 )
          )
        ),
        
        # Start game
        fluidRow(
          column(12, align="center",
                 actionButton(inputId = "go_button", label = "Let's go!")
          )
        ),
        
        # Guess input
        textInput(inputId = "guess_input", label = "Type Guess", value = ""),
        
        # Guess and new word button
        fluidRow(
          column(12, align="center",
                 actionButton(inputId = "guess_button", label = "Guess!"),
                 actionButton(inputId = "new_button", label = "New Word")
          )
        ),
        
        # Keyboard
        fluidRow(
          column(12, align="center",
                 plotOutput("keyboard", height = "200px")
          )
        ),
        
        # Reset button
        fluidRow(
          column(12, align="center",
                 actionButton(inputId = "reset_button", label = "Reset Number of Letters")
          )
        ),
        
        br(), br(),
        
        # Need for citation
        tagList(  
          HTML(
            '<center><p style="font-size:20px">Wordle<sup>&copy;</sup> is originally created and copyrighted (2021-2022) by Josh Wardle (aka powerlanguage).<br/>
          All credit for the creation and development of Wordle<sup>&copy;</sup> must be provided to them.</p></center>'
          )
        
        ),
      
      ),
    
    # Main panel
    mainPanel(
      
      width = 7,
      
      fluidRow(
        column(12, align="center",
               plotOutput("frames", width = "75%")
        )
      ),
      
      tags$footer(
        HTML(
          '<center><p style="font-size:20px">
          This implementation is adapted from the original
          <a href="https://www.powerlanguage.co.uk/wordle/">Wordle<sup>&copy;</sup></a></p></center>'
        )
      )
      
    )
    
  )

)
