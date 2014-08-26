'PrintScreenplay' <- function(text)
{
  ############################################################
  # This function prints out the input screenplay text
  # to a file named input.txt to be scanned by other functions.
  # This is necessary to speed up computation time.
  ############################################################
  input.file <- 'input.txt'
  unlink(input.file)
  write.table(text, file=input.file, quote=FALSE, row.names=FALSE, col.names=FALSE)
}

#'PredictFlopProb' <- function(text, budget)
#{
#  PrintScreenplay(text)
  #y.pred <- predict(fit, X.test, lambda=fit$lambda.min)
  #flop.prob <- predict(fit, X)
#  flop.prob <- 0.95
#  return(flop.prob)
#}

'PredictProfit' <- function(text, budget, rf.fit, word2vec.clusters)
{
  ############################################################
  # This function predicts the expected profit ration from 
  # user-defined budget and screenplay text. The underlying
  # algorithm relies on random forest regression
  ############################################################

  # count number of word2vec clusters
    PrintScreenplay(text)
    #target.budget <- as.numeric(budget) / 1000000
    target.budget <- as.numeric(budget)
    nb.clusters <- unique(word2vec.clusters[,2])
    screenplay <- scan('input.txt', sep=' ', what='raw()')
    screenplay.words <- table(as.vector(screenplay))
    #screenplay <- scan('input.txt', sep='\n', what='raw()')
    #screenplay.words <- unlist(sapply(screenplay, function(x) strsplit(x, '\\s+', perl=TRUE)[[1]]))
    words.freq <- table(screenplay.words)
    words.match <- match(names(words.freq), word2vec.clusters[, 1])
    #remove NAs
    index.na <- which(is.na(words.match=='TRUE'))
    if(length(index.na) > 0)
    {
      words.match <- words.match[-index.na]
      words.freq <- words.freq[-index.na]
    }
    # populate word2vec test vector
    word2vec.screenplay <- mat.or.vec(length(nb.clusters), 1)
    for(w in 1:length(words.match))
    {
      index.col <- as.numeric(word2vec.clusters[words.match[w], 2]) + 1
      word2vec.screenplay[index.col] <- word2vec.screenplay[index.col] + as.vector(words.freq[w])
    }

  # add budget information
  ytest <- c(target.budget, word2vec.screenplay)
  names(ytest) <- c('budget', 1:500)
  profit.ratio <- 2^(predict(rf.fit, ytest))
  revenue.income <- round(profit.ratio * target.budget , 1)
  print(profit.ratio)
  return(list(profit.ratio, revenue.income, ytest))
}

'PredictRevenue' <- function(df, budget, profit.ratio, ytest, rf.fit)
{
  #target.budget <- log(as.numeric(budget), 10)
  target.budget <- as.numeric(budget)*1e6
  #df$revenue <- log(df$revenue, 10)
  #df$budget <- log(df$budget, 10)
  #profit <- log((df$revenue / df$budget), 2)
  profit <- df$revenue / df$budget
  status <- rep('loss', nrow(df))
  status[which(profit > 1)] <- 'profit'
  df$status <- as.factor(status)
  df <- df[which(df$budget >= 1e4), ]

  revenue.pred <- profit.ratio * target.budget
  df.pred <- data.frame(revenue_pred=revenue.pred, 
                  target_budget=target.budget)

  cols <- c("profit" = "royalblue3","loss" = "firebrick3")
  p1 <- ggplot(df, aes(y=revenue, x=budget, color=status)) +
    geom_point(alpha = 0.5, size=1.5) +
    theme_classic() +
    geom_abline(intercept = 0, slope=1, 
      colour = "indianred3", size = 1.5, linetype='dashed') +
    ylab('Movie Revenue (in $)') + xlab('Movie budget (in $)') +
    scale_colour_manual(values = cols) + 
    scale_y_continuous(trans=log10_trans()) + 
    scale_x_continuous(trans=log10_trans()) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
    theme(axis.text.x=element_text(size=14),
      axis.title.x=element_text(size=16),
      axis.text.y=element_text(size=14),
      axis.title.y=element_text(size=16),
      legend.title=element_text(size=14),
      legend.text=element_text(size=12),
      legend.position='top') +
    geom_point(data=df.pred, aes(x = target_budget, y = revenue_pred), colour = "red", size=4)

  # add budget information
  budget.range <- (seq(1e6, 200e6, 1000000)) / 1000000
  ytest2 <- lapply(budget.range, function(x) as.vector(c(x, ytest[-1])))
  ytest2 <- as.data.frame(do.call(rbind, ytest2))
  colnames(ytest2) <- c('budget', 1:500)
  yhat <- predict(rf.fit, ytest2)
  yhat <- 2^yhat
  #index.loss <- which(yhat<1)
  #yhat[index.loss] <- -1/(yhat[index.loss])
  max.yhat <- max(yhat)
  max.budget <- budget.range[which.max(yhat)]
  df.segment <- data.frame(x=c(0, max.budget*1e6), 
                            y=c(max.yhat, 0), 
                            vx=c(max.budget*1e6, max.budget*1e6), 
                            vy=c(max.yhat, max.yhat))

  df <- data.frame(budget=budget.range, profitability=yhat)
  df.profit <- data.frame(budget=target.budget, profit=profit.ratio)
  #df.profit <- data.frame(budget=target.budget, profit=log(profit.ratio, 2))
  p2 <- ggplot(df, aes(x=budget*1e6, y=profitability)) +
       geom_line(size=2, color='royalblue3') +
       theme_classic() +
      ylab('Movie Profitability') + xlab('Movie budget (in $)') +
       annotate("text", label='Maximum profit', x=max.budget*1e6, y=max.yhat+0.25, size=5, 
      fontface="bold.italic") +
       geom_segment(df.segment, mapping=aes(x = x, y = y, xend = vx, yend = vy), 
                  size=1.3, linetype='dotted') +
       theme(axis.text.x=element_text(size=14),
          axis.title.x=element_text(size=16),
          axis.text.y=element_text(size=14),
          axis.title.y=element_text(size=16)) +
       geom_point(data=df.profit, aes(x=budget, y=profit), size=6, color='red', pch=18) +
       scale_x_continuous(trans=log10_trans()) +
       scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))


  #df.all <- data.frame(group=rep('profit', length(profit)), profit=profit)
  #df.profit <- data.frame(group='profit', profit=profit.ratio)
   # p2 <- ggplot(df.all, aes(x=group, y=profit)) + 
   #   geom_boxplot(alpha=0.2, outlier.size = 0) +
   #   geom_point(alpha=0.3, position='jitter', color='royalblue3') +
    #  ylim(-5, 5) +
    #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) +
   #   scale_y_log10(breaks = trans_breaks("log2", function(x) 2^x), labels = trans_format("log2", math_format(2^.x))) +
    #  theme_classic() + 
    #  xlab('') + ylab('Movie profitability') +
    #  geom_point(data=df.profit, aes(x=group, y=profit), size=4, color='red', pch=18) +
    #  theme(axis.text.x=element_text(size=14),
    #  axis.title.x=element_text(size=16),
    #  axis.text.y=element_text(size=14),
    #  axis.title.y=element_text(size=16),
    #  legend.title=element_text(size=14),
    #  legend.text=element_text(size=12))

  p3 <- grid.arrange(p2, p1, ncol=2, nrow=1, widths=c(4, 4))
  return(p3)
}

'FindTextEmotion' <- function(lexicon, text)
{
  ############################################################
  # This function finds the range of basic human emotions in
  # the user-defined screenplay. It relies on the NRC lexicon
  # of human emotions
  ############################################################

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

  freq <- round(temp[-c(6,7)]/sum(temp[-c(6,7)]), 2)
  df <- data.frame(emotions=emotions[-c(6,7)], frequency=freq)
  Bar <- gvisBarChart(df, xvar="emotions", yvar="frequency", 
                  options=list(isStacked=TRUE, 
                  title="Frequency of feelings in movie", 
                  vAxis="{title:''}",
                  hAxis="{title:'Frequency in text'}",
                  width=800)
                  )
  return(Bar)
}

'ComputeEmotionalRollerCoaster' <- function(text)
{
  ## This code is (heavily) adapated from the one provided by Chris Okugami
  ## Github project can be found here: https://github.com/okugami79/sentiment140
  #text <- sapply(text, function(x) strsplit(x, split='\\n', perl=TRUE))
  #text <- text[[1]]
  #x <- sentiment(text)
  text <- sapply(text, function(x) strsplit(x, split='\\. ', perl=TRUE))
  text <- text[[1]]
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

  sentiment.out <- lapply(r$value(), fromJSON)
  polarity <- unlist(lapply(sentiment.out[[1]]$data, function(x) x$polarity))
  polarity <- polarity - 2
  window <- c()
  for(i in 1:(length(polarity)-20))
  {
   window <- c(window, sum(polarity[i:(i+20)]))
  }
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



#################################################
############### DEFUNCT CODE ####################
#################################################

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

  p3 <- grid.arrange(p1, p2, ncol=2)
  return(p3)
}



