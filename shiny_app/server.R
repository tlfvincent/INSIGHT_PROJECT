library(shiny)
library(rCharts)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(glmnet)
library(googleVis)
library(RCurl)
library(rjson)
library(plyr)
platform <- sessionInfo()$R.version$platform
linux <- stringr::str_detect(platform, "linux")
cols = brewer.pal(9, 'Set1')
source('data/misc.R')

# load TMDB movie metadata
load(file='data/TMDB_movie_metadata.Rdata')

# NRC emotion lexicon
load(file='data/NRC_emotion_lexicon.Rdata')

# load word2vec word clusters
load(file='data/word2vec_clusters.Rdata')

# create matr of correlation between co-occuring movie genres
#genres <- c("Animation", "Family", "Comedy", "Romance", "Drama", "Indie", 
#      "Crime", "Mystery", "Suspense", "Thriller", "Adventure" , "Action", 
#      "War", "Horror", "Fantasy", "Science_Fiction")
#genre.index <- 10:ncol(movie.metadata)
#genre.index <- match(genres, colnames(tmdb.movie.metadata))
#genre.matrix <- tmdb.movie.metadata[, genre.index]
#corrmatrix <- cor(genre.matrix, method='spearman') #store corr matrix

#options(RCHART_WIDTH = 800)
shinyServer(function(input, output) {

  output$text_budget <- renderText({ 
      #rating.score <- PredictRating(input$TextArea, input$budget)
      sprintf('Your movie budget: $%s', input$budget)
    })

    output$flop_prob <- renderText({ 
      flop.prob <- PredictFlopProb(input$TextArea, input$budget)
      sprintf('Flop probability: %s', flop.prob)
    })

    output$profit_ratio <- renderText({ 
      profit.ratio <- PredictProfit(input$TextArea, 
                                input$budget, 
                                fit, 
                                word2vec.clusters)
      sprintf('Profit ratio: %s', profit.ratio)
    })

  output$predict_revenue <- renderPlot({
    h1 <- PredictRevenue(tmdb.movie.metadata, input$budget)
    return(h1)
    })

  output$rollercoaster <- renderGvis({
    r1 <- ComputeEmotionalRollerCoaster(input$TextArea, linux)
    #r1 <- ComputeEmotionalRollerCoaster(input$InputFile)
    return(r1)
    #return(p1)
  })

    output$TextEmotion <- renderGvis({
    r1 <- FindTextEmotion(nrc.lexicon, input$TextArea)
    return(r1)
  })

    #dInput = reactive({
    #infile = input$InputFile
    #if (is.null(infile))
    #  p(HTML("<b><div style='background-color:#FADDF2;border:1px solid
    #                   black;'>Warning! 'wnaetw' is a free program provided
    #                   without any guarantee: please note that it does not
    #                   replace your brain. In particular, the dev team is not
    #                   responsible if a lazy student is not able to interpret
    #                   the function's outputs properly!!! (and if he thinks
    #                   that an average zip code is somehow informative...)</div>
    #                   </b>"))
    #else {
    #  #dat <- scan(in.file, sep='\n', what='raw()')
    #  paste(readLines(infile), collapse=" ")
    #}
  #})

  #output$rollercoaster1 = renderPlot({
  #  text <- dInput()
  #  if (is.null(text))
  #    return(NULL)
  #  else {
  #    r1 <- ComputeEmotionalRollerCoaster(text)
  #    return(r1)
  #  }
  #})

  #output$genre_cor <- renderChart({
  #  corrdata=as.data.frame(corrmatrix)
  #  corrdata$Variable1=names(corrdata)
  #  corrdatamelt=melt(corrdata, id="Variable1")
  #  names(corrdatamelt)=c("Variable1","Variable2","CorrelationCoefficient")
  #  corrmatplot = rPlot(Variable2 ~ Variable1, color = 'CorrelationCoefficient', data = corrdatamelt, type = 'tile')
  #  corrmatplot$guides("{color: {scale: {type: gradient2, lower: 'red', middle: 'white', upper: 'blue', midpoint: 0}}}")
  #  corrmatplot$guides(y = list(numticks = length(unique(corrdatamelt$Variable1))))
  #  corrmatplot$guides(x = list(numticks = 3))
  #  corrmatplot$addParams(height =500, width=500, dom = 'genre_cor')
  #  return(corrmatplot)
  #  })

  #output$plot_cor_interactive <- renderChart({
  #  r1 <- plotCorInteractive(tmdb.movie.metadata, input$yvar, input$xvar)
  #  return(r1)
  #})
  
  #output$plot_cor_static <- renderPlot({
      #plot(tmdb.movie.metadata$revenue, tmdb.movie.metadata$budget)
  #    p2 <- plotCorStatic(tmdb.movie.metadata, input$yvar, input$xvar, cols)
  #    return(p2)
  #  })
})

