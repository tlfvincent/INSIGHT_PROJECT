library(shiny)
library(rCharts)
library(reshape2)

# load TMDB movie metadata
load(file='TMDB_movie_metadata.Rdata')

# create matr of correlation between co-occuring movie genres
genre.index <- 10:ncol(tmdb.movie.metadata)
unique.genre <- colnames(tmdb.movie.metadata)[genre.index]
genre.matrix <- tmdb.movie.metadata[, genre.index]
corrmatrix <- cor(genre.matrix, method='spearman') #store corr matrix


'plotCor' <- function(df)
{
  df$revenue <- log(df$revenue, 10)
  df$budget <- log(df$budget, 10)
  fit <- lm(revenue ~ budget, data=df)
  df$fitted_values <- fit$fitted.values

  r1 <- rPlot(fitted_values ~ budget, data = df, type = 'line', size = list(const = 5))
  r1$layer(x = "budget", y = "revenue", data = df, type = 'point', size = list(const = 2))
  #r1 <- rPlot(revenue ~ budget, data = df, type = 'point', size = list(const = 3))
  r1$addParams(width = 800, height = 500, dom = 'plot_cor', title = "Box office returns vs. movie budget")
  r1$guides(y = list(title = "Box office", max = 14, size = list(const = 15)))
  r1$guides(x = list(title = "Budget", min=0, max = 10, size=15))
  return(r1)
}

#options(RCHART_WIDTH = 800)
shinyServer(function(input, output) {
  # You can access the value of the widget with input$file, e.g.
  #output$plot <- renderPlot({ hist(input$file[ , 1]) })

  output$genre_cor <- renderChart({
    corrdata=as.data.frame(corrmatrix)
    corrdata$Variable1=names(corrdata)
    corrdatamelt=melt(corrdata, id="Variable1")
    names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
    corrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile')
    corrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red', middle: 'white', upper: 'blue', midpoint: 0}}}")
    corrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
    corrmatplot$guides(x = list(numticks = 3))
    corrmatplot$addParams(height =500, width=800, dom = 'genre_cor')
    return(corrmatplot)
  })

  output$plot_cor <- renderChart({
    r1 <- plotCor(tmdb.movie.metadata)
    return(r1)
  })
})



# The following steps are generic and can all be placed in a function with some tweaks to customize output 
