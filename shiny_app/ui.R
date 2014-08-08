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
  navbarPage("MillionDollar$tory",
#headerPanel(
#list(tags$head(tags$style("body {background-color: white; }")),
#"Graphs", HTML('<img src="meme.png", height="500px"    
#style="float:right"/>','<p style="color:black"> test test </p>' ))
#),
  tabPanel("App",
    sidebarLayout(
      sidebarPanel(
        textInput("budget", "Enter your movie budget (if known)", "1000000"),
        # create input text area to paste script
        inputTextarea('exampleTextarea', "This will probably not work\n Let's try and mess this up \n #YOLO \n #hashtag",20,35 ),
        # create command to upload file
        fileInput("file", label = h3("File input"))
      ),
      mainPanel(
        tabsetPanel(type = "tabs", 
        tabPanel("With budget", plotOutput("plot")), 
        tabPanel("Without budget", tableOutput("table")),
        p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph", style = "font-family: 'times'; font-si16pt"),
        code("code displays your text similar to computer code")
      )
      )
    )
  ),

  # model validation page
  tabPanel("Model Validation",
    sidebarLayout(
      sidebarPanel(
        radioButtons("yvar", 
                  h4("Predict:"), 
                    c("revenue" = "revenue", "vote_average" = "vote_average")),
        br(),
        radioButtons("xvar", 
                  h4("With feature:"), 
                    c("budget" = "budget", "genre" = "genre"))
        #checkboxGroupInput("xvar",
        #                  label = "With features:",
        #                  choices = c("budget" = "budget", 
        #                    "genre" = "genre", 
        #                    "sentiment" = "sentiment"))
                            #selected = 'budget')
    #selectInput('y.var', 'Predict:', choices = c('revenue', 'vote_average'))
    ),
    mainPanel(
          plotOutput("plot_cor_static")
          #showOutput("plot_cor_interactive", "polycharts")
      )
    )
  ),

  # panel for data summary page
  tabPanel("Data Summary",
    sidebarLayout(
      sidebarPanel(
        selectInput('y.var', 'Y variable:', choices = c('revenue', 'vote_average'))
      ),
      mainPanel(
        showOutput("genre_cor", "polycharts")
      )
    )
  ),


  # page 
  tabPanel("About",
    verbatimTextOutput("summary")
  )
)

)