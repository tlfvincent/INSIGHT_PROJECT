'ComputeEmotionalRollerCoaster' <- function(text)
{
  input.file <- 'input.txt'
  write.table(text, file=input.file, quote=FALSE, row.names=FALSE, col.names=FALSE)
  stmt <- sprintf('curl --data-binary @%s "http://www.sentiment140.com/api/bulkClassify?query=movie" >sentiment.csv', input.file)  
  system(stmt)
  sentiment <- read.csv(file='sentiment.csv', header=FALSE)
  sentiment <- sentiment[, 1]-2
  window <- c()
  for(i in 1:(length(sentiment)-3))
  {
   window <- c(window, sum(sentiment[i:(i+5)]))
  }
  df <- data.frame(time = 1:length(window), sentiment = window)
  r1 <- ggplot(df, aes(x=time, y=sentiment)) +
    geom_smooth(size=2, col='royalblue3') +
    theme_classic()
  #unlink('sentiment.csv', recursive = FALSE)
  #r1 <- rPlot(sentiment ~ time, data = df, type = "point")
  #r1$layer(x = "time", y = "sentiment", data = df, type = 'line', size = list(const = 3))
  #r1$addParams(width = 600, height = 300, dom = 'rollercoaster')
  return(r1)
}

'PredictFlopProb' <- function(text, budget)
{
  target.budget <- log(as.numeric(budget), 10)
  df$revenue <- log(df$revenue, 10)
  df$budget <- log(df$budget, 10)
  fit <- lm(revenue ~ budget, data=df)
  revenue.pred <- coef(fit)[1] + target.budget*coef(fit)[2]
  df.pred <- data.frame(revenue_pred=revenue.pred, target_budget=target.budget)
  a <- signif(coef(fit)[1], digits = 2)
  b <- signif(coef(fit)[2], digits = 2)
  textlab <- paste("y = ",b,"x + ",a, sep="")
  p1 <- ggplot(df, aes(y=revenue, x=budget)) +
          geom_point(alpha = 1/2) +
          geom_smooth(size=2, method='lm', 
            fill="blue", colour="darkblue", alpha=0.2) +
          theme_classic() +
          ylim(0, 10) + xlim(0, 10) +
          ylab('revenue') +
          annotate("text", x = 7, y = 10, 
            label = textlab, 
            color="black", size = 5, 
            parse=FALSE) +
          theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=18)) +
          geom_point(data=df.pred, aes(x = target_budget, y = revenue_pred), colour = "red", size=4)
  return(p1)
}


'PredictRevenue' <- function(df, budget)
{
  target.budget <- log(as.numeric(budget), 10)
  df$revenue <- log(df$revenue, 10)
  df$budget <- log(df$budget, 10)
  fit <- lm(revenue ~ budget, data=df)
  revenue.pred <- coef(fit)[1] + target.budget*coef(fit)[2]
  df.pred <- data.frame(revenue_pred=revenue.pred, target_budget=target.budget)
  a <- signif(coef(fit)[1], digits = 2)
  b <- signif(coef(fit)[2], digits = 2)
  textlab <- paste("y = ",b,"x + ",a, sep="")
  p1 <- ggplot(df, aes(y=revenue, x=budget)) +
          geom_point(alpha = 1/2) +
          geom_smooth(size=2, method='lm', 
            fill="blue", colour="darkblue", alpha=0.2) +
          theme_classic() +
          ylim(0, 10) + xlim(0, 10) +
          ylab('revenue') +
          annotate("text", x = 7, y = 10, 
            label = textlab, 
            color="black", size = 5, 
            parse=FALSE) +
          theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=18)) +
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



