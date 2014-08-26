library(shiny)
library(rCharts)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(glmnet)
library(randomForest)
library(RCurl)
library(rjson)
library(plyr)
library(scales)
library(markdown)
library(shinyBS)
library(memoise)
suppressPackageStartupMessages(library(googleVis))
cols = brewer.pal(9, 'Set1')
source('data/misc.R')

# load TMDB movie metadata
load(file='data/TMDB_movie_metadata.Rdata')

# load movie screenplays
load(file='data/all_movie_screenplay.Rdata')
available.movies <- names(movie.screenplay)

# NRC emotion lexicon
load(file='data/NRC_emotion_lexicon.Rdata')

# load word2vec word clusters
load(file='data/word2vec_clusters.Rdata')

# load cached word2vec word clusters from screenplay
load(file='data/cached_word2vec_vectors.Rdata')

# load random forest model for profitability prediction
load(file='data/random_forest_profit_model.Rdata')

shinyServer(function(input, output, session) {

  # compute and print expected profit ratio and box  office revenue for
  # user-defined budget and screenplay text
  output$profit_ratio <- renderUI({
    if(input$budget != 0 & input$TextArea != '')
    {
      screenplay.profitability <- PredictProfit(input$TextArea, 
                                input$budget, 
                                rf.fit, 
                                word2vec.clusters,
                                word2vec.test,
                                original=TRUE, title='')
      profit.ratio <- round(screenplay.profitability[[1]], 1)
      input.budget <- as.numeric(input$budget)
      revenue.income <- screenplay.profitability[[2]]
      str1 <- sprintf('The expected profit ratio of this screenplay is %s.', profit.ratio)
      str2 <- sprintf('For a budget of $%s millions, you will earn an estimated box office income of $%s millions.', input.budget, revenue.income)
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    else if(input$budget != 0 & input$MovieSelect != '')
    {
      if (!(input$MovieSelect %in% available.movies)) { stop("This movie is not in the database!") }
      screenplay.profitability <- PredictProfit(movie.screenplay[input$MovieSelect], 
                                input$budget, 
                                rf.fit, 
                                word2vec.clusters,
                                word2vec.test,
                                original=FALSE, title=input$MovieSelect)
      profit.ratio <- round(screenplay.profitability[[1]], 1)
      input.budget <- as.numeric(input$budget)
      revenue.income <- screenplay.profitability[[2]]
      str1 <- sprintf('The expected profit ratio of this screenplay is %s.', profit.ratio)
      str2 <- sprintf('For a budget of $%s millions, you will earn an estimated box office income of $%s millions.', input.budget, revenue.income)
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    else
    {
      str1 <- '<br/><br/>
                <center> 
                Please input an estimated budget and the screenplay you wish to evaluate in the panel on the left
                </center>'
      HTML(str1)
    }
  })

  # compute and plot expected profit ratio and box  office revenue for
  # a range of budgertbudget and screenplay text
  output$predict_revenue <- renderPlot({
    if(input$budget != 0 & input$TextArea != '')
    {
      screenplay.profitability <- PredictProfit(input$TextArea, 
                                  input$budget, 
                                  rf.fit, 
                                  word2vec.clusters,
                                  word2vec.test,
                                  original=TRUE, title='')
      profit.ratio <- screenplay.profitability[[1]]
      ytest <- screenplay.profitability[[3]]
      h1 <- PredictRevenue(tmdb.movie.metadata, input$budget, profit.ratio, ytest, rf.fit)
      return(h1)
    }
    else if(input$budget != 0 & input$MovieSelect != '')
    {
      #if (!(input$MovieSelect %in% available.movies)) { stop("This movie is not in the database!") }
      screenplay.profitability <- PredictProfit(movie.screenplay[input$MovieSelect], 
                                  input$budget, 
                                  rf.fit, 
                                  word2vec.clusters,
                                  word2vec.test,
                                  original=FALSE, title=input$MovieSelect)
      profit.ratio <- screenplay.profitability[[1]]
      ytest <- screenplay.profitability[[3]]
      h1 <- PredictRevenue(tmdb.movie.metadata, input$budget, profit.ratio, ytest, rf.fit)
      return(h1)
    }
  })

  # Compute positive and negative sentiment in user-defined screenplay text
  # Sentiment analysis is performed using the sentiment140 API 
  output$rollercoaster <- renderGvis({
    if(input$MovieSelect != '')
    {
      if (!(input$MovieSelect %in% available.movies)) { stop("This movie is not in the database!") }
      r1 <- ComputeEmotionalRollerCoaster(movie.screenplay[input$MovieSelect])
      return(r1)
    }
    if(input$TextArea != '')
    {
      r1 <- ComputeEmotionalRollerCoaster(input$TextArea)
      #r1 <- ComputeEmotionalRollerCoaster(input$TextArea)
      return(r1)
    }
  })

  # FInd the most frequent human emotions in user-defined screenplay text
  # Words associated to human emotions are extracted from the NRC human emotion lexicon
  output$TextEmotion <- renderGvis({
    if(input$TextArea != '')
    {
      r1 <- FindTextEmotion(nrc.lexicon, input$TextArea)
      return(r1)
    }
    if(input$MovieSelect != '')
    {
      if (!(input$MovieSelect %in% available.movies)) { stop("This movie is not in the database!") }
      r1 <- FindTextEmotion(nrc.lexicon, movie.screenplay[input$MovieSelect])
      return(r1)
    }
  })
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


