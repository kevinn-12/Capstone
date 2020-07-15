library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("NextWordPrediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("text",
                        "Type something...",
                        "")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
            wellPanel(
                
                # Link to report
                helpText(a('More information on the app',
                           href='http://rpubs.com/akselix/word_prediction', 
                           target = '_blank')
                ),
                
                # Link to repo
                helpText(a('Code repository',
                           href='https://github.com/akselix/capstone_swiftkey/tree/master/shiny',
                           target = '_blank')
                ),
                
            textOutput("predictiontext"),
            
            tableOutput('predictiontable')
        )
    )
))
)
