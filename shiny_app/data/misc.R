'PrintScreenplay' <- function(text)
{
  ############################################################
  # This function prints out the input screenplay text
  # to a file named input.txt to be scanned by other functions.
  # This is necessary to speed up computation time.
  ############################################################
  
  input.file <- 'input.txt'
  write.table(text, file=input.file, quote=FALSE, row.names=FALSE, col.names=FALSE)
}

'PredictFlopProb' <- function(text, budget)
{
  PrintScreenplay(text)
  #y.pred <- predict(fit, X.test, lambda=fit$lambda.min)
  #flop.prob <- predict(fit, X)
  flop.prob <- 0.95
  return(flop.prob)
}

'PredictProfit' <- function(text, budget, rf.fit, word2vec.clusters)
{
  ############################################################
  # This function predicts the expected profit ration from 
  # user-defined budget and screenplay text. The underlying
  # algorithm relies on random forest regression
  ############################################################

  # count number of word2vec clusters
  nb.clusters <- unique(word.clusters[,2])
  screenplay <- scan('input.txt', sep=' ', what='raw()')
  screenplay.words <- table(as.vector(screenplay))
  words.freq <- table(screenplay.words)
  words.match <- match(names(words.freq), word2vec.clusters[, 1])
  #remove NAs
  index.na <- which(is.na(words.match=='TRUE'))
  if(length(index.na) > 0)
  {
    words.match <- words.match[-index.na]
    words.freq <- words.freq[-index.na]
  }
  # populate test vector
  screenplay.test <- mat.or.vec()
  for(w in 1:length(words.match))
  {
    index.col <- as.numeric(word.clusters[words.match[w], 2]) + 1
    word2vec.mat[f, index.col] <- word2vec.mat[f, index.col] + as.vector(words.freq[w])
  }
  

  profit.ratio <- 100
  return(profit.ratio)
}

'FindTextEmotion' <- function(lexicon, text)
{
  PrintScreenplay(text)
  emotions <- colnames(lexicon)
  words <- row.names(lexicon)
  temp <- mat.or.vec(length(emotions), 1)

  screenplay <- scan('input.txt', sep=' ', what='raw()')
  screenplay.words <- table(as.vector(screenplay))
  words.match <- match(names(screenplay.words), words)
  #remove NAs
  words.match <- words.match[!is.na(words.match)]
  temp <- apply(lexicon[words.match, ], 2, sum)

  print('plotting space')
  freq <- round(temp[-c(6,7)]/sum(temp[-c(6,7)]), 2)
  df <- data.frame(emotions=emotions[-c(6,7)], frequency=freq)
  Bar <- gvisBarChart(df, xvar="emotions", yvar="frequency", 
                  options=list(isStacked=TRUE, 
                  title="Frequency of feelings in movie", 
                  vAxis="{title:''}",
                  hAxis="{title:'Frequency in text'}",
                  width=900)
                  )
  return(Bar)
}

'ComputeEmotionalRollerCoaster' <- function(text, linux)
{
  ## This code is (heavily) adapated from the one provided by Chris Okugami
  ## Github project can be found here: https://github.com/okugami79/sentiment140
  text <- sapply(text, function(x) strsplit(x, split='\\n', perl=TRUE))
  text <- text[[1]]
  #x <- sentiment(text)
  r <- dynCurlReader()  
  # get rid of single quote 
  text <- gsub("'", ' ' ,text)
  text <- gsub('"', ' ' ,text)
  x  <- paste( sprintf("{'text': '%s'}", text),
         collapse = "," ) 
  curlPerform(postfields =   
                sprintf('{"language": "auto", "data": [%s]}', x),
              url = "http://www.sentiment140.com/api/bulkClassifyJson?query=movie", 
              verbose = FALSE,
              post = 1L, 
              writefunction = r$update)
  print('finished sentiment 140')
  sentiment.out <- lapply(r$value(), fromJSON)
  polarity <- unlist(lapply(sentiment.out[[1]]$data, function(x) x$polarity))
  polarity <- polarity - 2
  window <- c()
  for(i in 1:(length(polarity)-20))
  {
   window <- c(window, sum(polarity[i:(i+20)]))
  }
  #input.file <- 'input.txt'
  #write.table(text, file=input.file, quote=FALSE, row.names=FALSE, col.names=FALSE)
  #stmt <- 'curl --data-binary @input.txt "http://www.sentiment140.com/api/bulkClassify?query=movie" >sentiment.csv' 
  #system(stmt)
  #print('running sentiment 140')
  #info <- system(stmt, intern=TRUE)
  #print('finished sentiment 140')
  #sentiment <- read.csv(file='sentiment.csv', header=FALSE)
  #sentiment <- sentiment[, 1]-2
  #window <- c()
  #for(i in 1:(length(sentiment)-20))
  #{
  # window <- c(window, sum(sentiment[i:(i+20)]))
  #}
  neg <- window
  neg[which(neg>0)] <- 0
  pos <- window
  pos[which(pos<0)] <- 0
  df <- data.frame(time = 1:length(window), 
                  pos = pos, 
                  neg=neg)
  SteppedArea <- gvisSteppedAreaChart(df, xvar="time", 
                                    yvar=c("pos", "neg"),
                                    options=list(isStacked=TRUE, 
                                      title="Sentiment timescale of movie", 
                                      vAxis="{title:'Sentiment'}",
                                      hAxis="{title:'Line number'}",
                                      width=950))
  return(SteppedArea)
  
}

'PredictRevenue' <- function(df, budget)
{
  target.budget <- log(as.numeric(budget), 10)
  df$revenue <- log(df$revenue, 10)
  df$budget <- log(df$budget, 10)
  profit <- df$revenue / df$budget
  status <- rep('loss', nrow(df))
  status[which(profit>1)] <- 'profit'
  df$status <- as.factor(status)

  fit <- lm(revenue ~ budget, data=df)
  revenue.pred <- coef(fit)[1] + target.budget*coef(fit)[2]
  df.pred <- data.frame(revenue_pred=revenue.pred, 
                  target_budget=target.budget)
  a <- signif(coef(fit)[1], digits = 2)
  b <- signif(coef(fit)[2], digits = 2)
  textlab <- paste("y = ",b,"x + ",a, sep="")

  p1 <- ggplot(df, aes(y=revenue, x=budget, color=status)) +
          geom_point(alpha = 0.5) +
          theme_classic() +
          geom_abline(intercept = 0, slope=1, 
            colour = "indianred3", size = 1.5, linetype='dashed') +
          geom_abline(intercept = a, slope=b, 
            colour = "royalblue3", size = 2, linetype='solid') +
          ylim(5, 9) + xlim(5, 9) +
          ylab('revenue') +
          annotate("text", x = 7, y = 9, 
            label = textlab, 
            color="black", size = 5, 
            parse=FALSE) +
          theme(axis.text.x=element_text(size=14),
            axis.title.x=element_text(size=16),
            axis.text.y=element_text(size=14),
            axis.title.y=element_text(size=16)) +
          geom_point(data=df.pred, aes(x = target_budget, y = revenue_pred), colour = "red", size=4)
  return(p1)
}

'plotCorInteractive' <- function(df, yvar, xvar)
{
  df <- df[, match(c(yvar, xvar), colnames(df))]
  colnames(df) <- c('response', xvar)
  df$response <- log(df$response, 10)
  if('budget' %in% xvar) { 
    df$budget <- log(df$budget, 10)
    yxline <- data.frame(x=df$response, y=df$budget)
  }
  fit <- lm(response ~ ., data=df)
  df$fitted_values <- fit$fitted.values

  r1 <- rPlot(fitted_values ~ budget, data = df, type = 'line', size = list(const = 5))
  r1$layer(x = "budget", y = "revenue", data = df, type = 'point', size = list(const = 2))
  r1$layer(x = "x", y = "y", data = yxline, type = 'line', size = list(const = 2))
  #r1 <- rPlot(revenue ~ budget, data = df, type = 'point', size = list(const = 3))
  r1$addParams(width = 800, height = 500, dom = 'plot_cor', title = "Box office returns vs. movie budget")
  r1$guides(y = list(title = "Box office", max = 14, size = list(const = 15)))
  r1$guides(x = list(title = "Budget", min=5, max = 10, size=15))
  return(r1)
}

'plotCorStatic' <- function(df, yvar, xvar, cols)
{
  ratio <- log(df$revenue, 10) / log(df$budget, 10)
  labels <- mat.or.vec(length(ratio), 1)
  labels[intersect(which(ratio<=1.1), which(ratio>=0.9))] <- 'uncertain'
  labels[which(ratio>1.1)] <- 'profit'
  labels[which(ratio<0.9)] <- 'loss'
  df <- df[, match(c(yvar, xvar), colnames(df))]
  df$labels <- labels
  colnames(df) <- c('response', xvar, 'labels')
  if(yvar=='revenue'){
    df$response <- log(df$response, 10)
  }
  if('budget' %in% xvar) { 
    df$budget <- log(df$budget, 10)
    #yxline <- data.frame(x=df$response, y=df$budget)
  }
  fit <- coef(lm(response ~ ., data=df))
  a <- signif(fit[1], digits = 2)
  b <- signif(fit[2], digits = 2)
  textlab <- paste("y = ",b,"x + ",a, sep="")
  #df$fitted_values <- fit$fitted.values
  if(yvar == 'revenue'){
    method.smooth <- 'lm'
  }
  else
  {
    method.smooth <- 'loess'
  }
  p1 <- ggplot(df, aes(y=response, x=budget)) +
          geom_point(aes(color = factor(labels)), alpha = 1/2) +
          geom_smooth(size=2, method=method.smooth, 
            fill="blue", colour="darkblue", alpha=0.2) +
          theme_classic() +
          ylim(0, 10) + xlim(0, 10) +
          ylab(yvar) +
          scale_color_manual(values=cols) +
          annotate("text", x = 7, y = 10, 
            label = textlab, 
            color="black", size = 5, 
            parse=FALSE) +
          theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=18))

  p2 <- ggplot(df, aes(y=response, x=budget)) +
          geom_point(aes(color = factor(labels)), alpha = 1/2) +
          geom_smooth(size=2, method=method.smooth, 
            fill="blue", colour="darkblue", alpha=0.2) +
          theme_classic() +
          ylab(yvar) +
          xlim(5, 9) +
          theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=18))
  #if(yvar == 'revenue'){
    #p1 <- p1 + geom_abline(intercept = 0, slope = 1, size=1, color='indianred2')
    #p2 <- p2 + geom_abline(intercept = 0, slope = 1, size=1, color='indianred2')
  #}

  p3 <- grid.arrange(p1, p2, ncol=2)
  return(p3)
}



