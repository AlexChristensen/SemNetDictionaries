# Code for WoRdle----
server <- function(input, output, session)
{
  
  # Get screen width
  # output$screen_width <- renderUI({
  #   
  #   HTML(
  #     input$GetScreenWidth
  #   )
  #   
  # })
  
  # Set up QWERTY keyboard
  # (from https://stackoverflow.com/questions/42089812/plot-keyboard-layout-in-r#42090533)
  # Thanks to Haboryme
  
  # Initialize bindings ----
  
  ## First row
  df1 <<- data.frame(xmin=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5),
                 xmax=c(2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),
                 ymin=rep(2,10),
                 ymax=rep(2.5,10),
                 value=c("Q","W","E","R","T","Y","U","I","O","P"))
  ## Second row
  df2 <<- data.frame(xmin=c(1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75),
                 xmax=c(2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75),
                 ymin=rep(1.5,9),
                 ymax=rep(2,9),
                 value=c("A","S","D","F","G","H","J","K","L"))
  ## Third row
  df3 <<- data.frame(xmin=c(2.5,3.5,4.5,5.5,6.5,7.5,8.5),
                 xmax=c(3.5,4.5,5.5,6.5,7.5,8.5,9.5),
                 ymin=rep(1,7),
                 ymax=rep(1.5,7),
                 value=c("Z","X","C","V","B","N","M"))
  
  #putting it together
  keyboard <<- rbind(df1,df2,df3)
  
  # Colors
  green_color <<- "#528c4e"
  yellow_color <<- "#b49f3a"
  grey_color <<- "#3a3b3d"
  default <<- "#131313"
  border <<- "#4f4f50"
  
  # Set up keyboard colors
  key_fill <<- rep(default, nrow(keyboard))
  names(key_fill) <- keyboard$value
  key_color <<- rep(border, nrow(keyboard))
  names(key_color) <- keyboard$value
  
  # Show/hide
  shinyjs::hide("guess_input")
  shinyjs::hide("guess_button")
  shinyjs::hide("new_button")
  shinyjs::hide("reset_button")
  
  # Instructions ----
  
  # Send instructions
  instruction_frame <<- data.frame(
    xmin=seq(2.5, (1.5 + 5), 1),
    xmax=seq(3.25, (2.5 + 5), 1),
    ymin=rep(1, 5),
    ymax=rep(2, 5),
    value=rep("", 5)
  )
  instruction_frame$value <- c("W", "O", "R", "D", "S")
  
  ## Correct letter
  correct_fill <- c(green_color, rep(default, 4))
  correct_color <- c(NA, rep(border, 4))
  
  correct_plot <- ggplot2::ggplot(instruction_frame, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    ggplot2::geom_rect(color=correct_color, fill=correct_fill, lwd=1.25) + 
    ggplot2::geom_text(ggplot2::aes(
      x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
    ),size=10, color = "white")+
    ggplot2::theme_void()
  
  ## Letter in word
  in_word_fill <- c(default, yellow_color, rep(default, 3))
  in_word_color <- c(border, NA, rep(border, 3))
  
  in_word_plot <- ggplot2::ggplot(instruction_frame, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    ggplot2::geom_rect(color=in_word_color, fill=in_word_fill, lwd=1.25) + 
    ggplot2::geom_text(ggplot2::aes(
      x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
    ),size=10, color = "white")+
    ggplot2::theme_void()
  
  ## Bad letter
  bad_fill <- c(rep(default, 2), grey_color, rep(default, 2))
  bad_color <- c(rep(border, 2), NA, rep(border, 2))
  
  bad_plot <- ggplot2::ggplot(instruction_frame, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
    ggplot2::geom_rect(color=bad_color, fill=bad_fill, lwd=1.25) + 
    ggplot2::geom_text(ggplot2::aes(
      x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
    ),size=10, color = "white")+
    ggplot2::theme_void()
  
  # Make grid
  shinyalert::shinyalert(
    title = "How to Play",
    html = TRUE,
    text = tagList(
      renderText({"Guess the WORDLE in length of word + 1 tries."}),
      br(),
      renderText({"Each guess must be a valid word"}),
      br(),
      renderPlot({correct_plot}, height = 50, width = 300),
      br(),
      renderText({"The letter 'W' is in the correct spot."}),
      br(),
      renderPlot({in_word_plot}, height = 50, width = 300),
      br(),
      renderText({"The letter 'O' is in the word but the wrong spot."}),
      br(),
      renderPlot({bad_plot}, height = 50, width = 300),
      br(),
      renderText({"The letter 'R' is not in the word in any spot."})
    ),
    size = "s",
    closeOnClickOutside = TRUE
  )
  
  # Initialize word and UI ----
  
  # Get words in dictionary
  observeEvent(input$go_button, {
    
    # Keyboard plot
    key_plot <<- ggplot2::ggplot(keyboard, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      ggplot2::geom_rect(color=key_color, fill=key_fill, lwd=1.25) + 
      ggplot2::geom_text(ggplot2::aes(
        x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
      ),size=10, color = "white") +
      ggplot2::theme_void() + 
      ggplot2::theme(
        panel.background = element_rect(
          color = "#f4f5f4", fill = "#f4f5f4"
        )
      )
    
    # Obtain dictionary of words
    dictionary <<- SemNetDictionaries::cocaspell.dictionary
    
    # Number of letters for all words
    characters <<- unlist(lapply(dictionary, nchar))
    
    # Number of letters
    num_letters <<- input$num_letters
    
    # Match selected number of letters
    word_index <<- !is.na(match(characters, num_letters))
    
    # Obtain words
    words <<- dictionary[word_index]
    
    # Select a starting word
    word <<- words[sample(1:length(words), 1, replace = FALSE)]
    
    # Loop through to find word with no punctuations
    while(nchar(gsub("[[:punct:]]", "", word)) != nchar(word)){
      
      # Re-select a starting word
      word <<- words[sample(1:length(words), 1, replace = FALSE)]
      
    }

    # Get word length
    word_letters <<- toupper(unlist(strsplit(word, split = "")))
    word_length <<- length(word_letters)
    guess_length <<- word_length + 1
    
    # Create data frames for guessing frames
    guess_frame <<- data.frame(
      xmin=seq(2.5, (1.5 + word_length), 1),
      xmax=seq(3.25, (2.5 + word_length), 1),
      ymin=rep(1, word_length),
      ymax=rep(2, word_length),
      value=rep("", word_length)
    )
    
    # Set up levels for guess frame
    guess_frame$value <- factor(
      guess_frame$value, levels = c(
        "", LETTERS
      )
    )
    
    # Set up frame colors
    frame_fill <<- rep(default, word_length)
    frame_color <<- rep(border, word_length)
    
    # Frame plots
    frame_plot <<- ggplot2::ggplot(guess_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      ggplot2::geom_rect(color=frame_color, fill=frame_fill, lwd=1.25) + 
      ggplot2::geom_text(aes(
        x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
      ),size=10, color = "white")+
      ggplot2::theme_void()
    
    # Frame list
    frame_list <<- lapply(1:guess_length, function(i){
        frame_plot
    })

    # Frame grid
    frame_grid <<- patchwork::wrap_plots(
      frame_list,
      ncol = 1
    )
    
    # Show/hide
    shinyjs::hide("num_letters")
    shinyjs::hide("go_button")
    shinyjs::show("guess_input")
    shinyjs::show("guess_button")
    shinyjs::show("reset_button")
    
    # Send frames
    output$frames <- renderPlot({frame_grid})
  
    # Send keyboard
    output$keyboard <- renderPlot({key_plot})
    
  })
  
  # Guess ----
  
  # Get guess
  observeEvent(input$guess_button, {
    
    # Obtain guess
    guess <<- tolower(trimws(input$guess_input))
 
    # Check if word in dictionary
    if(!guess %in% SemNetDictionaries::cocaspell.dictionary){
      
      # Let them know word is not in dictionary
      shinyalert::shinyalert(
        title = "Guessed word not found in dictionary. Try again.",
        type = "error"
      )
      
      # Update text input
      updateTextInput(
        session, inputId = "guess_input", label = "Type Guess", value = ""
      )
      
    }else if(nchar(guess) != word_length){ # Check for guess length
      
      # Let them know word is not of the same length
      shinyalert::shinyalert(
        title = "Number of letters does not match word length!",
        type = "error"
      )
      
      # Update text input
      updateTextInput(
        session, inputId = "guess_input", label = "Type Guess", value = ""
      )
      
    }else{ # Word is in dictionary and length of word
      
      # Increase guess count
      if(exists("guess_count", envir = globalenv())){
        guess_count <<- guess_count + 1
      }else{# Set guess counter
        guess_count <<- 1
      }
      
      # Check letters that match word
      guess_letters <<- toupper(unlist(strsplit(guess, split = "")))
      
      # Guess index
      guess_index <<- 1:length(word_letters)
      
      # Get frequency of word letters
      word_letter_freq <<- table(word_letters)
      
      # Indices for guess letters in word letters
      if(any(guess_letters %in% word_letters)){
        
        # Indices for guess letters that match word letters
        if(any(guess_letters == word_letters)){
          
          # Indices for guess letters
          matched_index <<- which(guess_letters == word_letters)

          # Update guess index
          guess_index <<- setdiff(guess_index, matched_index)
          
        }else{
          matched_index <<- NULL
        }
        
        # Indices for guess letters
        in_index <<- which(guess_letters %in% word_letters)
        
        # Determine how many guess letters are in word
        guess_letter_freq <<- table(guess_letters[in_index])
        
        # Remove extraneous indices
        if(any(word_letter_freq[names(guess_letter_freq)] < guess_letter_freq)){
          
          # Match letters
          match_extra <<- intersect(names(guess_letter_freq), names(word_letter_freq))
          
          # Target extraneous
          target_extra <<- which(guess_letter_freq[match_extra] > word_letter_freq[match_extra])
          target_extra <<- guess_letter_freq[match_extra][target_extra] - word_letter_freq[match_extra][target_extra]
          
          # Check for matched index
          if(!is.null(matched_index)){
            
            # Obtain table match frequencies
            matched_letter_freq <<- table(guess_letters[matched_index])
            
            # Match letters
            match_extra <<- intersect(names(matched_letter_freq), names(target_extra))
            
            # Only if matched extra is non-zero
            if(length(match_extra) != 0){
              
              # Target extraneous
              target_extra <<- target_extra + matched_letter_freq[match_extra]
              
            }
            
          }
          
          # Remove from guess letters
          for(i in seq_along(target_extra)){
            
            # Extraneous letter
            extra_letter <<- rev(in_index[guess_letters[in_index] == names(target_extra)[i]])[1:target_extra]
            
            # Update guess index in word
            in_index <<- setdiff(in_index, extra_letter)
            
          }
          
        }
        
        # Update indices for letters in word
        in_index <<- setdiff(in_index, matched_index)
        
        # Update guess index
        guess_index <<- setdiff(guess_index, in_index)
        
      }else{
        
        in_index <<- NULL
        matched_index <<- NULL
        
      }
      
      # Guess frame
      guess_frame$value <- factor(
        guess_letters, levels = c(
          "", LETTERS
        )
      )
      
      # Default update to guess letters
      if(length(guess_index) != 0){

        # Update colors
        ## Keyboard
        names(key_fill) <- keyboard$value
        key_fill[guess_letters[guess_index]] <- grey_color
        key_fill <<- key_fill
        
        # Keyboard plot
        key_plot <<- ggplot2::ggplot(keyboard, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
          ggplot2::geom_rect(color=key_color, fill=key_fill, lwd=1.25) + 
          ggplot2::geom_text(aes(
            x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
          ),size=10, color = "white") +
          ggplot2::theme_void() + 
          ggplot2::theme(
            panel.background = element_rect(
              color = "#f4f5f4", fill = "#f4f5f4"
            )
          )
        
        ## Frame
        frame_fill[guess_index] <- grey_color
        frame_fill <<- frame_fill
        frame_color[guess_index] <- NA
        frame_color <<- frame_color
        
        frame_list[[guess_count]] <- ggplot2::ggplot(guess_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
          ggplot2::geom_rect(color=frame_color, fill=frame_fill, lwd=1.25) + 
          ggplot2::geom_text(aes(
            x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
          ),size=10, color = "white")+
          ggplot2::theme_void()
      }
      
      # Set up current frame
      if(length(in_index) != 0){
      
        # Update colors
        ## Keyboard
        names(key_fill) <- keyboard$value
        key_fill[guess_letters[in_index]] <- yellow_color
        key_fill <<- key_fill
        
        # Keyboard plot
        key_plot <<- ggplot2::ggplot(keyboard, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
          ggplot2::geom_rect(color=key_color, fill=key_fill, lwd=1.25) + 
          ggplot2::geom_text(aes(
            x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
          ),size=10, color = "white") +
          ggplot2::theme_void() + 
          ggplot2::theme(
            panel.background = element_rect(
              color = "#f4f5f4", fill = "#f4f5f4"
            )
          )
        
        ## Frame
        frame_fill[in_index] <- yellow_color
        frame_fill <<- frame_fill
        frame_color[in_index] <- NA
        frame_color <<- frame_color
        
        frame_list[[guess_count]] <- ggplot2::ggplot(guess_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
          ggplot2::geom_rect(color=frame_color, fill=frame_fill, lwd=1.25) + 
          ggplot2::geom_text(aes(
            x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
          ),size=10, color = "white")+
          ggplot2::theme_void()
        
      }
      
      # Set up current frame
      if(length(matched_index) != 0){
        
        # Update colors
        ## Keyboard
        names(key_fill) <- keyboard$value
        key_fill[guess_letters[matched_index]] <- green_color
        key_fill <<- key_fill
        
        # Keyboard plot
        key_plot <<- ggplot2::ggplot(keyboard, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
          ggplot2::geom_rect(color=key_color, fill=key_fill, lwd=1.25) + 
          ggplot2::geom_text(aes(
            x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
          ),size=10, color = "white") +
          ggplot2::theme_void() + 
          ggplot2::theme(
            panel.background = element_rect(
              color = "#f4f5f4", fill = "#f4f5f4"
            )
          )
        
        ## Frame
        frame_fill[matched_index] <- green_color
        frame_fill <<- frame_fill
        frame_color[matched_index] <- NA
        frame_color <<- frame_color
        
        frame_list[[guess_count]] <- ggplot2::ggplot(guess_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
          ggplot2::geom_rect(color=frame_color, fill=frame_fill, lwd=1.25) + 
          ggplot2::geom_text(aes(
            x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
          ),size=10, color = "white")+
          ggplot2::theme_void() 
        
      }
      
      # Update frame list
      frame_list <<- lapply(1:guess_length, function(i){
        
        frame_list[[i]]
        
      })
      
      # Frame grid
      frame_grid <<- patchwork::wrap_plots(
        frame_list,
        ncol = 1
      )
      
      # Send frames
      output$frames <- renderPlot({frame_grid})
      
      
      # Send keyboard
      output$keyboard <- renderPlot({key_plot})
      
      # Update text input
      updateTextInput(
        session, inputId = "guess_input", label = "Type Guess", value = ""
      )
      
      # Check if solved
      if(all(guess_letters == word_letters)){
        
        shinyalert::shinyalert(
          title = "You guessed the word!",
          text = "Press 'New Word' to play again.",
          type = "success"
        )
        
        # Show/hide
        shinyjs::show("new_button")
        
      }else if(guess_length == guess_count){# Check if maximum number of guesses reached
        
        shinyalert::shinyalert(
          title = "You ran out of guesses!",
          text = paste("The word was:", word, "\n\nPress 'New Word' to play again."),
          type = "warning"
        )
        
        # Show/hide
        shinyjs::show("new_button")
        
      }
      
    }
      
  })
  
  # New word ----
  
  # New word button
  observeEvent(input$new_button, {
    
    # Reset plots
    output$frames <- renderPlot({})
    
    # Send keyboard
    output$keyboard <- renderPlot({})
    
    ## First row
    df1 <<- data.frame(xmin=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5),
                       xmax=c(2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),
                       ymin=rep(2,10),
                       ymax=rep(2.5,10),
                       value=c("Q","W","E","R","T","Y","U","I","O","P"))
    ## Second row
    df2 <<- data.frame(xmin=c(1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75),
                       xmax=c(2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75),
                       ymin=rep(1.5,9),
                       ymax=rep(2,9),
                       value=c("A","S","D","F","G","H","J","K","L"))
    ## Third row
    df3 <<- data.frame(xmin=c(2.5,3.5,4.5,5.5,6.5,7.5,8.5),
                       xmax=c(3.5,4.5,5.5,6.5,7.5,8.5,9.5),
                       ymin=rep(1,7),
                       ymax=rep(1.5,7),
                       value=c("Z","X","C","V","B","N","M"))
    
    #putting it together
    keyboard <<- rbind(df1,df2,df3)
    
    # Colors
    green_color <<- "#528c4e"
    yellow_color <<- "#b49f3a"
    grey_color <<- "#3a3b3d"
    default <<- "#131313"
    border <<- "#4f4f50"
    
    # Set up keyboard colors
    key_fill <<- rep(default, nrow(keyboard))
    names(key_fill) <- keyboard$value
    key_color <<- rep(border, nrow(keyboard))
    names(key_color) <- keyboard$value
    
    # Reset values
    key_plot <<- ggplot2::ggplot(keyboard, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      ggplot2::geom_rect(color=key_color, fill=key_fill, lwd=1.25) + 
      ggplot2::geom_text(aes(
        x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
      ),size=10, color = "white") +
      ggplot2::theme_void() + 
      ggplot2::theme(
        panel.background = element_rect(
          color = "#f4f5f4", fill = "#f4f5f4"
        )
      )
    
    # Remove guess counter
    if(exists("guess_count", envir = globalenv())){
      rm("guess_count", envir = globalenv()) 
    }
    
    # Select a starting word
    word <<- words[sample(1:length(words), 1, replace = FALSE)]
    
    # Loop through to find word with no punctuations
    while(nchar(gsub("[[:punct:]]", "", word)) != nchar(word)){
      
      # Re-select a starting word
      word <<- words[sample(1:length(words), 1, replace = FALSE)]
      
    }
    
    # Get word length
    word_letters <<- toupper(unlist(strsplit(word, split = "")))
    word_length <<- length(word_letters)
    guess_length <<- word_length + 1
    
    # Create data frames for guessing frames
    guess_frame <<- data.frame(
      xmin=seq(2.5, (1.5 + word_length), 1),
      xmax=seq(3.25, (2.5 + word_length), 1),
      ymin=rep(1, word_length),
      ymax=rep(2, word_length),
      value=rep("", word_length)
    )
    
    # Set up frame colors
    frame_fill <<- rep(default, word_length)
    frame_color <<- rep(border, word_length)
    
    # Frame plots
    frame_plot <<- ggplot2::ggplot(guess_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      ggplot2::geom_rect(color=frame_color, fill=frame_fill, lwd=1.25) + 
      ggplot2::geom_text(aes(
        x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
      ),size=10, color = "white")+
      ggplot2::theme_void()
    
    # Frame list
    frame_list <<- lapply(1:guess_length, function(i){
      
      frame_plot
      
    })
    
    # Frame grid
    frame_grid <<- patchwork::wrap_plots(
      frame_list,
      ncol = 1
    )
    
    # Show/hide
    shinyjs::hide("num_letters")
    shinyjs::hide("go_button")
    shinyjs::hide("new_button")
    shinyjs::show("guess_input")
    shinyjs::show("guess_button")
    shinyjs::show("reset_button")
    
    # Send frames
    output$frames <- renderPlot({frame_grid})
    
    # Send keyboard
    output$keyboard <- renderPlot({key_plot})
    
  })

  # New letters ----
  
  # Reset button
  observeEvent(input$reset_button, {
    
    # Remove all variables in the global environment
    # rm(list = ls(envir = globalenv()), envir = globalenv())
    
    # Reset plots
    output$frames <- renderPlot({})
    
    # Send keyboard
    output$keyboard <- renderPlot({})
    
    ## First row
    df1 <<- data.frame(xmin=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5),
                       xmax=c(2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5,11.5),
                       ymin=rep(2,10),
                       ymax=rep(2.5,10),
                       value=c("Q","W","E","R","T","Y","U","I","O","P"))
    ## Second row
    df2 <<- data.frame(xmin=c(1.75,2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75),
                       xmax=c(2.75,3.75,4.75,5.75,6.75,7.75,8.75,9.75,10.75),
                       ymin=rep(1.5,9),
                       ymax=rep(2,9),
                       value=c("A","S","D","F","G","H","J","K","L"))
    ## Third row
    df3 <<- data.frame(xmin=c(2.5,3.5,4.5,5.5,6.5,7.5,8.5),
                       xmax=c(3.5,4.5,5.5,6.5,7.5,8.5,9.5),
                       ymin=rep(1,7),
                       ymax=rep(1.5,7),
                       value=c("Z","X","C","V","B","N","M"))
    
    #putting it together
    keyboard <<- rbind(df1,df2,df3)
    
    # Colors
    green_color <<- "#528c4e"
    yellow_color <<- "#b49f3a"
    grey_color <<- "#3a3b3d"
    default <<- "#131313"
    border <<- "#4f4f50"
    
    # Set up keyboard colors
    key_fill <<- rep(default, nrow(keyboard))
    names(key_fill) <- keyboard$value
    key_color <<- rep(border, nrow(keyboard))
    names(key_color) <- keyboard$value
    
    # Reset values
    key_plot <<- ggplot2::ggplot(keyboard, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
      ggplot2::geom_rect(color=key_color, fill=key_fill, lwd=1.25) + 
      ggplot2::geom_text(aes(
        x=(xmin+xmax)/2,y=(ymin+ymax)/2,label=value
      ),size=10, color = "white") +
      ggplot2::theme_void() + 
      ggplot2::theme(
        panel.background = element_rect(
          color = "#f4f5f4", fill = "#f4f5f4"
        )
      )
    
    # Remove guess counter
    if(exists("guess_count", envir = globalenv())){
      rm("guess_count", envir = globalenv()) 
    }
    
    # Show/hide
    shinyjs::hide("guess_input")
    shinyjs::hide("guess_button")
    shinyjs::hide("reset_button")
    shinyjs::show("num_letters")
    shinyjs::show("go_button")
    
  })
  
  # Remove all variables in the global environment
  onStop(function(x){
    rm(list = ls(envir = globalenv()), envir = globalenv())
  })
  
}