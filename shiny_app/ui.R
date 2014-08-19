require(rCharts)
options(RCHART_LIB = 'polycharts')

inputTextarea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
      tags$textarea(id = inputId,
                    class = "inputtextarea",
                    rows = nrows,
                    cols = ncols,
                    as.character(value))
    )
}

dialogue <- "Goooooooood morning Vietnam! It's 0600 hours. What does the O stand for? 
O my God, it's early! Speaking of early, let's hear it for that Marty Lee Drywitz. 
Silky smooth sounds, making me sound like Peggy Lee...Gooooooooood-byyyyyyye Vietnaaaaam! 
That's right, I'm history... I'm outta here. I got the lucky ticket home, baby. 
Rollin, rollin, rollin'... keep them wagons rollin', rawhide! 
Yeah, that's right... the final Adrian Cronauer broadcast... and this one is brought to you by our friends at the Pentagon. 
Remember the people who brought you Korea? 
That's right, the U.S. Army. If it's being done correctly, here or abroad, it's probably not being done by the Army."

shinyUI(fluidPage(
  # Application title
  titlePanel("MillionDollar$tory"), 
  theme = "flatly.css",
  
  # Sidebar with controls to select the random distribution type
  fluidRow(
    column(3,
      wellPanel(
        h4("Data importation"),
 
    # Button to import data
    #fileInput('file1', 'Choose CSV/TXT File',
              #accept=c('text/csv', 'text/comma-separated-values,text/plain'))
        textInput("budget", "Enter your movie budget (if known)", "1000000"),
        #checkboxGroupInput("genre", "movie genre:",
             #choices = list("Comedy" = Comedy, "Crime" = Crime, "Drama"= Drama Horror Romance" = 1, "Choice 2" = 2,
              #         "Choice 3" = 3), selected = 1),
        # create input text area to paste script
        inputTextarea('TextArea', dialogue, 16, 25)
        # create command to upload file
        #fileInput("InputFile", 
        #          label = h4("Upload file"))
      )
    ),
    
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
    column(9,
      tabsetPanel(type = "tabs", position = "above", 
        tabPanel("App", 
        fluidRow(
            column(5, 
              h4(textOutput("text_budget")),
              br(),
              h4(textOutput("flop_prob")),
              br(),
              h4(textOutput("profit_ratio"))
            ),
            column(7,
              plotOutput("predict_revenue")
            )
          ),
          p(HTML("This application is kindly provided by
           <a href='http://www.rstudio.com/'><font color='#DF01A5'>
           <b>Natty</b></font></a> with the generous help of 
           <font color='#3E2D8E'><b>Shiny</b></font> and 
           <font color='#159332'><b>Rstudio</b></font>. It is distributed under
           the licence <a href='http://www.wtfpl.net/'>WTFPL</a>."))
        ), 
        tabPanel("Movie Analytics",
          h3("Emotional rollercoaster"),
          htmlOutput("rollercoaster"),
          htmlOutput("TextEmotion")
          #includeMarkdown("html/MillionDollarStory_Blockbusters.Rmd")
          #includeHTML("html/MillionDollarStory_Blockbusters.html")
          ),
        #tabPanel("Expected Blockbusters"
          #includeMarkdown("html/MillionDollarStory_Blockbusters.Rmd")
          #includeHTML("html/MillionDollarStory_Blockbusters.html")
        #  ),
        #tabPanel("Expected Flops"
          #includeHTML("html/MillionDollarStory_Blockbusters.html")
        #  ),
        tabPanel("Behind the Scenes",
          #helpText("Enter information about yourself below to make the estimate more accurate.")
          includeHTML("html/MillionDollarStory_BehindTheScenes.html")
          )
      )
    )
  )
))