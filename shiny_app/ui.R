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

shinyUI(

  navbarPage(span(strong("Welcome to MillionDollar$tory"), width="100px"), 
          theme = "flatly.css",

  #fluidPage(
  #titlePanel("MillionDollar$tory"), theme = "bootstrap.css",
#headerPanel(
#list(tags$head(tags$style("body {background-color: white; }")),
#"Graphs", HTML('<img src="meme.png", height="500px"    
#style="float:right"/>','<p style="color:black"> test test </p>' ))
#),
  tabPanel("App",
    #sidebarLayout(
      #sidebarPanel(
      fluidRow(
        column(3,
          wellPanel(
          # HTML info
          div(style = "margin-top: 30px; width: 50px;"),
          #helpText("Enter information about your movie."),
          textInput("budget", "Enter your movie budget (if known)", "1000000"),
          # create input text area to paste script
          inputTextarea('TextArea', "This will probably not work\n Let's try and mess this up \n #YOLO \n #hashtag\n This will probably not work
\n woop it did work!",20,35 ),
          # create command to upload file
          fileInput("file", label = h3("File input"))
        )
      ),
      column(9,
      #mainPanel(
        plotOutput("predict_revenue"),
        plotOutput("rollercoaster")
        #showOutput("rollercoaster", "polycharts")
        #plotOutput("rollercoaster"),
        #tabsetPanel(type = "tabs", 
        #tabPanel("With budget", plotOutput("plot")), 
        #tabPanel("Without budget", tableOutput("table")),
        #code("code displays your text similar to computer code")
      )
    )
  ),

  # model validation page
  tabPanel("Model Validation",
      fluidRow(
        column(2,
          wellPanel(
            radioButtons("yvar", 
                  h4("Predict:"), 
                    c("revenue" = "revenue", "vote_average" = "vote_average")),
          br(),
          radioButtons("xvar", 
                  h4("With feature:"), 
                    c("budget" = "budget", "genre" = "genre"))
          )
        ),
    column(10, 
          plotOutput("plot_cor_static")
          #showOutput("plot_cor_interactive", "polycharts")
      )
    )
  ),

  # panel for data summary page
  tabPanel("Data Summary",
    sidebarLayout(
      sidebarPanel(
        helpText("Enter information about yourself below to make the estimate more accurate."),
        selectInput('data_type', 'Select a movie feature:', 
            choices = c('revenue', 'vote_average', 'genre', 'words', 'sentiment'))
      ),
      mainPanel(
        showOutput("genre_cor", "polycharts")
      )
    )
  ),


  # page 
  tabPanel("About",
    p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph", style = "font-family: 'times'; font-si16pt"),
    code("code displays your text similar to computer code")
  )
)

)