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
