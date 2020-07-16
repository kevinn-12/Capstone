library(shiny); library(dplyr); library(stringr)

dat <- read.csv("dat_all.csv")

shinyServer(function(input, output) {
    dat_reactive <<- reactive({
        dat %>%
            filter(., N_gram == str_count(input$text, "\\S+") + 1) %>%
            filter(grepl(paste("^", tolower(str_squish(input$text)), sep = ""), Word)) %>%
            arrange(., desc(Prop))
    })
    
    output$predictiontext = reactive({
        if (nrow(dat_reactive()) != 0) {
            
            dat <<- dat %>%
                mutate(Frequency = ifelse(Word == input$text &
                                              N_gram == str_count(input$text, "\\S+"),
                                          Frequency + 1,
                                          Frequency)) %>%
                group_by(., N_gram) %>%
                mutate(., Prop = Frequency/ sum(Frequency)) %>%
                data.frame(.)
            
            val <- dat_reactive()$Word_to_Predict[1]
            
            return(val)
            
        } else {
            for (i in 1:str_count(input$text, "\\S+")) {
                input_1 <-  Reduce(paste, word(input$text, i:str_count(input$text,"\\S+")))
                
                dat_reactive <- reactive({
                    dat %>%
                        filter(., N_gram == str_count(input_1, "\\S+") + 1) %>%
                        filter(grepl(paste("^", tolower(str_squish(input_1)), sep = ""), Word)) %>%
                        arrange(., desc(Prop))
                })
                
                if (nrow(dat_reactive()) != 0) {
                    val <- dat_reactive()$Word_to_Predict[1]
                    
                    return(val)
                    
                } else if (nrow(dat_reactive()) == 0 & i == str_count(input$text, "\\S+")) {
                    dat <<- dat %>%
                        add_row(., Word = tolower(input$text), Frequency = + 1, N_gram = str_count(input$text, "\\S+"), 
                                Word_to_Predict = word(input$text, -1)) %>%
                        group_by(., N_gram) %>%
                        mutate(., Prop = Frequency/ sum(Frequency)) %>%
                        data.frame(.)
                    
                    ans <- paste("Word not in dictionary. We added this to our database!")
                    
                    return(ans)
                }
            }
        }
    })
    
    output$predictiontable =  renderTable({

        head(dat_reactive(),5)

    })
    
})

