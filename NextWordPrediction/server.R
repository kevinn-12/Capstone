library(shiny); library(dplyr); library(stringr); library(data.table)

shinyServer(function(input, output, session) {
    dat <- fread("dat_all.csv")
    
    dat_reactive <<- reactive({
        dat %>%
            filter(., N_gram == str_count(input$text, "\\S+") + 1 & nzchar(input$text) & 
                       grepl(paste("^", tolower(str_squish(input$text)), sep = ""), Word)) %>%
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
            for (i in 2:str_count(input$text, "\\S+")) {
                input_1 <-  word(input$text, start = i, end =  str_count(input$text,"\\S+"))
                
                dat_reactive <- reactive({
                    dat %>%
                        filter(., N_gram == str_count(input_1, "\\S+") + 1 & nzchar(input_1) & 
                                   grepl(paste("^", tolower(str_squish(input_1)), sep = ""), Word)) %>%
                        arrange(., desc(Prop))
                })
               
                if (nrow(dat_reactive()) != 0) {
                    val <- dat_reactive()$Word_to_Predict[1]

                    return(val)
                    
                } else if (nrow(dat_reactive()) == 0 & i == str_count(input$text, "\\S+") & nzchar(input$text) & !(input$text %in% dat$Word)) {
                    
                    for (i in str_count(input$text, "\\S+"):1) {
                        input_1 <-  word(input$text, start = 1, end =  i)
                        
                        dat <<- dat %>%
                            add_row(., Word = tolower(input_1), Frequency = + 1, N_gram = str_count(input_1, "\\S+"), 
                                    Word_to_Predict = word(input_1, -1)) %>%
                            group_by(., N_gram) %>%
                            mutate(., Prop = Frequency/ sum(Frequency)) %>%
                            data.frame(.)
                    }
                    
                    ans <- paste("Word not in dictionary. We added this to our database!")
                    
                    return(ans)
                    
                } else if (!nzchar(input$text)) {
                    ans <- paste("Type something...")
                    
                    return(ans)
                }
            }
        }
    })
    
    session$onSessionEnded(function() {
        fwrite(dat, "dat_all.csv")
    })
    
    # output$predictiontable =  renderTable({
    # 
    #     head(dat_reactive(),5)
    # 
    # })
    
})

