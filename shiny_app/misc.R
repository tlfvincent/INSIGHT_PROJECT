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

'plotCorStatic' <- function(df, yvar, xvar)
{
  df <- df[, match(c(yvar, xvar), colnames(df))]
  colnames(df) <- c('response', xvar)
  if(yvar=='revenue'){
    df$response <- log(df$response, 10)
  }
  if('budget' %in% xvar) { 
    df$budget <- log(df$budget, 10)
    yxline <- data.frame(x=df$response, y=df$budget)
  }
  #fit <- coef(lm(response ~ ., data=df))
  #df$fitted_values <- fit$fitted.values
  p1 <- ggplot(df, aes(y=response, x=budget)) +
          geom_point() +
          geom_smooth(size=2, method='lm') +
          geom_abline(intercept = 0, slope = 1, size=1, color='indianred2') +
          theme_bw() +
          ylab(yvar) +
          theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=18))

    p2 <- ggplot(df, aes(y=response, x=budget)) +
          geom_point() +
          geom_smooth(size=2, method='lm') +
          geom_abline(intercept = 0, slope = 1, size=1, color='indianred2') +
          theme_bw() +
          ylab(yvar) +
          xlim(5, 9) +
          theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
            axis.text.y=element_text(size=15),
            axis.title.y=element_text(size=18))

  p3 <- grid.arrange(p1, p2, ncol=2)
  #r1 <- plot(df$response, df$budget, 
  #            type='p',
  #            col='blue', 
  #            pch=20)
  return(p3)
}
