library(shiny)
library(rCharts)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
cols = brewer.pal(9, 'Set1')
source('misc.R')

# load TMDB movie metadata
load(file='TMDB_movie_metadata.Rdata')

# create matr of correlation between co-occuring movie genres
genres <- c("Animation", "Family", "Comedy", "Romance", "Drama", "Indie", 
      "Crime", "Mystery", "Suspense", "Thriller", "Adventure" , "Action", 
      "War", "Horror", "Fantasy", "Science_Fiction")
#genre.index <- 10:ncol(movie.metadata)
genre.index <- match(genres, colnames(tmdb.movie.metadata))
genre.matrix <- tmdb.movie.metadata[, genre.index]
#genre.index <- 10:ncol(tmdb.movie.metadata)
#unique.genre <- colnames(tmdb.movie.metadata)[genre.index]
#genre.matrix <- tmdb.movie.metadata[, genre.index]
corrmatrix <- cor(genre.matrix, method='spearman') #store corr matrix

#options(RCHART_WIDTH = 800)
shinyServer(function(input, output) {

  output$predict_revenue <- renderPlot({
    h1 <- PredictRevenue(tmdb.movie.metadata, input$budget)
    return(h1)
    })

  output$rollercoaster <- renderPlot({
    r1 <- ComputeEmotionalRollerCoaster(input$TextArea)
    return(r1)
    #return(p1)
    })



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

  #output$plot_cor_interactive <- renderChart({
  #  r1 <- plotCorInteractive(tmdb.movie.metadata, input$yvar, input$xvar)
  #  return(r1)
  #})
  
  output$plot_cor_static <- renderPlot({
      #plot(tmdb.movie.metadata$revenue, tmdb.movie.metadata$budget)
      p2 <- plotCorStatic(tmdb.movie.metadata, input$yvar, input$xvar, cols)
      return(p2)
    })
})



# The following steps are generic and can all be placed in a function with some tweaks to customize output 
