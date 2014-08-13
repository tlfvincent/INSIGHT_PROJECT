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


shinyUI(fluidPage(
  # Application title
  titlePanel("MillionDollar$tory"), theme = "bootstrap.css",
  
  # Sidebar with controls to select the random distribution type
  fluidRow(
    column(3,
      wellPanel(
        textInput("budget", "Enter your movie budget (if known)", "1000000"),
        # create input text area to paste script
        inputTextarea('TextArea', "This will probably not work\n Let's try and mess this up \n #YOLO \n #hashtag\n This will probably not work
\n woop it did work!",20,35 ),
        # create command to upload file
        fileInput("file", label = h3("File input"))
      )
    ),
    
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
    column(9,
      tabsetPanel(type = "tabs", 
        tabPanel("App", 
          plotOutput("predict_revenue"),
          plotOutput("rollercoaster")
        ), 
        tabPanel("Behind the Scenes",
          #helpText("Enter information about yourself below to make the estimate more accurate.")
          includeHTML("www/MillionDollarStory_BehindTheScenes.html")

          ) 
        #tabPanel("About",
        #helpText('YO!')
        #)
      )
    )
  )
))