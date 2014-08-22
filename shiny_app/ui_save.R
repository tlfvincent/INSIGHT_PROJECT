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
  #theme = "styles.css",
  tags$head(
        # Include our custom CSS
        includeCSS("www/styles.css")
  ),
  tags$head(tags$style(HTML("
    .shiny-text-output {
      background-color:#fff;
    }
  "))),
  
  h1("MillionDollar$tory", span("", style = "font-weight: 300"), 
      style = "font-family: 'Minion Pro';
        color: #fff; text-align: center;
        background-image: url('best-posters-of-all-time.jpg');
        padding: 22px"),
  br(),
  # Application title
  #titlePanel("MillionDollar$tory"), 
  
  
  # Sidebar with controls to select the random distribution type
  fluidRow(
    column(3,
      absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
        top = 60, left = "auto", right = 20, bottom = "auto",
        width = 270, height = "auto",
      #wellPanel(
        h4("Data importation"),
 
    # Button to import data
        textInput('budget', "Enter an estimated movie budget", "0"),
        # create input text area to paste script
        inputTextarea('TextArea', '', 16, 25),
        #textInput('TextArea', '',dialogue),
        submitButton("Update View", icon("refresh"))
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
            #column(4, 
              #h4(textOutput("text_budget")),
              #br(),
              #h4(textOutput("flop_prob")),
              #br(),
              h5(htmlOutput("profit_ratio")),
              #uiOutput("myList")
            #),
            #column(8,
              plotOutput("predict_revenue")
            #)
          )
        ), 
        tabPanel("Movie Analytics",
          h3("Emotional rollercoaster"),
          htmlOutput("rollercoaster"),
          htmlOutput("TextEmotion")
          ),
        tabPanel("Behind the Scenes",
          #helpText("Enter information about yourself below to make the estimate more accurate.")
          includeHTML("html/MillionDollarStory_BehindTheScenes.html")
          )
      )
    )
  )
))