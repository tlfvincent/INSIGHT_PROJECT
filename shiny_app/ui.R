require(rCharts)
library(shinyBS)
options(RCHART_LIB = 'polycharts')

# load movie screenplays
load(file='data/all_movie_screenplay.Rdata')
available.movies <- names(movie.screenplay)


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
        top = 120, left = "auto", right = 1080, bottom = "auto", 
        width = 270, height = "auto", cursor="default",
      #wellPanel(
        h4("Import data here"),
 
    # Button to import data
      h5(helpText("Enter an estimated movie budget")),
      h5(helpText("(in millions $)")),
        textInput('budget', "", "10"),
        h5(helpText("Select a movie:")),
        selectizeInput(
       'MovieSelect', '', choices = available.movies,
        options = list(
          placeholder = 'Select a movie',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
        h5(helpText("Or enter your own screenplay!")),
        # create input text area to paste script
        inputTextarea('TextArea', '', 12, 25),
        #textInput('TextArea', '',dialogue),
        submitButton('Update View', icon("refresh"))
      )
    ),
    
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
    column(9,
      absolutePanel(id = "controls", class = "modal", fixed = FALSE, draggable = TRUE,
        top = 120, left = "auto", right = 20, bottom = "auto", 
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