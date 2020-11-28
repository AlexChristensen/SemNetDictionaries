#' Yes/no menu
#' 
#' @description Provides Linux style yes/no menu
#' 
#' @param title Character.
#' Custom question
#'
#' @return \code{1} for \code{y} and \code{2} for \code{n}
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
#' 
#' @importFrom utils menu
#' 
# Yes/no menu----
# Updated 22.11.2020
yes.no.menu <- function (title = NULL) 
{
  # function for appropriate response
  yes.no <- function (ans)
  {
    # check for numeric
    if(is.numeric(ans)){
      
      return(NA)
      
    }else if(is.character(ans)){
      
      #change to lower case
      ans <- tolower(ans)
      
      if(ans != "y" && ans != "yes" && ans != "n" && ans != "no"){
        return(NA)
      }else{
        
        return(
          switch(ans,
                 "y" = 1,
                 "yes" = 1,
                 "n" = 2,
                 "no" = 2
                 
          )
        )
        
      }
      
    }else{return(NA)}
  }
  
  # append title with Linux style yes/no
  title <- paste(title, c("[Y/n]? "))
  
  # get response
  ans <- readline(prompt = title)
  
  # make sure there is an appropriate response
  while(is.na(yes.no(ans))){
    ans <- readline(prompt = "Inappropriate response. Try again [Y/n]. ")
  }
  
  return(yes.no(ans))
}

#' Stylizes Text
#' 
#' Makes text bold, italics, underlined, and strikethrough
#' 
#' @param text Character.
#' Text to stylized
#' 
#' @return Sytlized text
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# Style text----
# Updated 08.09.2020
styletext <- function(text, defaults = c("bold", "italics", "highlight",
                                         "underline", "strikethrough"))
{
  # Check system
  sys.check <- system.check()
  
  if(sys.check$TEXT)
  {
    if(missing(defaults))
    {number <- 0
    }else{
      
      # Get number code
      number <- switch(defaults,
                       bold = 1,
                       italics = 3,
                       underline = 4,
                       highlight = 7,
                       strikethrough = 9
      )
      
    }
    
    return(paste("\033[", number, ";m", text, "\033[0m", sep = ""))
  }else{return(text)}
}

#' System check for OS and RSTUDIO
#' 
#' @description Checks for whether text options are available
#' 
#' @param ... Additional arguments
#' 
#' @return \code{TRUE} if text options are available and \code{FALSE} if not
#' 
#' @author Alexander Christensen <alexpaulchristensen@gmail.com>
#' 
#' @noRd
# System Check----
# Updated 08.09.2020
system.check <- function (...)
{
  OS <- unname(tolower(Sys.info()["sysname"]))
  
  RSTUDIO <- ifelse(Sys.getenv("RSTUDIO") == "1", TRUE, FALSE)
  
  TEXT <- TRUE
  
  if(!RSTUDIO){if(OS != "linux"){TEXT <- FALSE}}
  
  res <- list()
  
  res$OS <- OS
  res$RSTUDIO <- RSTUDIO
  res$TEXT <- TEXT
  
  return(res)
}