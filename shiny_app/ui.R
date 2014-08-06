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

shinyUI(navbarPage("MillionDollar$tory",

  tabPanel("App",
    sidebarLayout(
      sidebarPanel(
        textInput("budget", "Enter your movie budget (if known)", "1000000"),
         # create input text area to paste script
         inputTextarea('exampleTextarea', '',20,35 ),
         # create command to upload file
         fileInput("file", label = h3("File input"))
        ),
            mainPanel(
        #plotOutput("plot")
              tabsetPanel(type = "tabs", 
        tabPanel("With budget", plotOutput("plot")), 
        tabPanel("Without budget", tableOutput("table"))
      )
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
        showOutput("genre_cor", "polycharts"),
        showOutput("plot_cor", "polycharts")
      )
    )
  ),

  # model validation page
  tabPanel("Model Validation"
  ),
  # page 
  tabPanel("About",
    verbatimTextOutput("summary")
  )
))