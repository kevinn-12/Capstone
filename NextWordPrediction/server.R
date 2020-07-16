library(shiny); library(dplyr); library(stringr)

dat <- read.csv("dat_all.csv")

shinyServer(function(input, output) {
  
    output$predictiontext =  reactive({
        dat_reactive <- reactive({
            dat %>%
                filter(., N_gram == str_count(input$text, "\\S+") + 1) %>%
                filter(grepl(paste("^", tolower(str_squish(input$text)), sep = ""), Word)) %>%
                arrange(., desc(Prop))
        })
        
        if (nrow(dat_reactive()) != 0) {
            
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
                    }
                }
            

            
        }


    })

    # output$predictiontable =  renderTable({
    # 
    #     dat_reactive()
    # 
    # })
    
})

