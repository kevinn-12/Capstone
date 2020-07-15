library(shiny)

dat <- read.csv("dat_all.csv")

shinyServer(function(input, output) {
    
    NextWordPrediction <- function(input) {
        dat <- dat %>%
            filter(., N_gram == str_count(input, "\\S+") + 1) %>%
            filter(grepl(paste("^", tolower(str_squish(input)), sep = ""), Word)) %>%
            arrange(., desc(Prop))
        
        if (nrow(dat) != 0) {
            assign("dat",
                   dat %>%
                       mutate(Frequency = ifelse(Word == input &
                                                     N_gram == str_count(input, "\\S+"),
                                                 Frequency + 1,
                                                 Frequency)) %>%
                       group_by(., N_gram) %>%
                       mutate(., Prop = Frequency/ sum(Frequency)) %>%
                       data.frame(.),
                   envir = .GlobalEnv)
            
            val <- dat$Word_to_Predict[1]
            ans <- paste(str_squish(input), val)
            
            return(list(ans, head(dat,5)))
            
        } else if (nrow(dat) == 0 & word(input, 1) != "NA") {
            assign("dat",
                   dat %>%
                       add_row(., Word = tolower(input), Frequency = + 1, N_gram = str_count(input, "\\S+"), 
                               Word_to_Predict = word(input, -1)) %>%
                       group_by(., N_gram) %>%
                       mutate(., Prop = Frequency/ sum(Frequency)) %>%
                       data.frame(.),
                   envir = .GlobalEnv)
            
            input_1 <-  Reduce(paste, word(input, 2:str_count(input,"\\S+")))
            
            return(NextWordPrediction(input_1))
            
        } else if (word(input, 1) == "NA") {
            ans <- paste("Word not in dictionary. We added this to our database!")
            
            return(ans)
        }
    }
    
    # output$predictiontext = NextWordPrediction(input$text)[1]
    # 
    # output$predictiontable = NextWordPrediction(input$text)[2]
    
    output$predictiontext =  reactive({

        NextWordPrediction(input$text)[1]
        
    })

    output$predictiontable =  renderTable({

        NextWordPrediction(input$text)[2]
        
    })
    
})

