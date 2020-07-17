library(shiny); library(shinythemes)


# Define UI for application
shinyUI(fluidPage(theme = shinytheme("cyborg"),

    # Application title
    titlePanel(h1("NextWordPrediction", align = "center")),
    
    navbarPage("Home",
               tabPanel(title=HTML("</a></li><li><a href='https://github.com/kevinn-12/Capstone' target='_blank'>Info"))
    ),
    
    mainPanel(
        fluidRow(align = "center",
                 
            textInput("text",
                      "Type something...",
                      ""),
            
            textOutput("predictiontext")
            ), 
        
        width = 12
    ),
    # fluidRow(align = "center",
    #     actionButton("twitter_share",
    #                              label = "Share",
    #                              icon = icon("twitter"),
    #                              onclick = sprintf("window.open('%s')", "https://twitter.com/intent/tweet?text=Hello%20world&url=https://shiny.rstudio.com/gallery/widget-gallery.html/")),
    #     actionButton("linkedin_share",
    #                  label = "Share",
    #                  icon = icon("linkedin"),
    #                  onclick = sprintf("window.open('%s')", "https://www.linkedin.com/shareArticle?mini=true&url=http://developer.linkedin.com&title=LinkedIn%20Developer%20Network&summary=My%20favorite%20developer%20program&source=LinkedIn"))
    #     ),
    

))
