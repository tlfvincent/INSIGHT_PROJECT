require(rCharts)
library(shinyBS)
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
  #theme = "bootstrap.css",
  tags$head(
        # Include our custom CSS
        includeCSS("www/styles.css")
  ),
  tags$head(tags$style(HTML("
    .shiny-body-output {
      background-color:#D3D3D3;
    }
  "))),

  tags$head(tags$style(HTML("
    .shiny-text-output {
      background-color:#D3D3D3;
    }
  "))),
  h1("", span("", style = "font-weight: 300"), 
      style = "font-family: 'Minion Pro';
         text-align: center;
         background-image: url('final.png');
        padding: 37px"),
  br(),
  # Application title
  #titlePanel("MillionDollar$tory"), 
  
  # Sidebar with controls to select the random distribution type
  fluidRow(
    column(3,
      absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
        top = 140, left = "auto", right = 1080, bottom = "auto", 
        width = 270, height = "auto", cursor="default",
      #wellPanel(
        h4("Import data here"),
 
    # Button to import data
      h5(helpText("Enter an estimated movie budget")),
        textInput('budget', "", "0"),
        # create input text area to paste script
        h5(helpText("Enter your movie screenplay")),
        inputTextarea('TextArea', '', 16, 25),
        #textInput('TextArea', '',dialogue),
        submitButton('Update View', icon("refresh"))
      )
    ),
    
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
    column(9,
      absolutePanel(id = "controls", class = "modal", fixed = FALSE, draggable = TRUE,
        top = 140, left = "auto", right = 20, bottom = "auto", 
        width = 1000, height = "auto", cursor="default",
      tabsetPanel(type = "tabs", position = "above", 
        tabPanel(h4("App"), 
        fluidRow(
              h4(htmlOutput("profit_ratio")),
              plotOutput("predict_revenue")
          )
        ), 
        tabPanel(h4("Movie Analytics"),
          h3("Emotional rollercoaster"),
          htmlOutput("rollercoaster"),
          htmlOutput("TextEmotion")
          ),
        tabPanel(h4("Behind the Scenes"),
          #helpText("Enter information about yourself below to make the estimate more accurate.")
          includeHTML("html/MillionDollarStory_BehindTheScenes.html")
          )
      )
    )
    )
  )
))