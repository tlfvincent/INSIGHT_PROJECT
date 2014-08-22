setwd('~/Desktop/MillionDollarStory/model_evaluation')
library(RSQLite)
library(superpc)
library(e1071)
library(LiblineaR)
library(nnet)
library(caTools)
library(caret)
library(earth)
library(lars)
library(elasticnet)
library(RRF)
library(randomForest)
library(neuralnet)
source('misc.R')

# read in word2vec clusters
setwd('~/Desktop/MillionDollarStory/exploratory_analysis/word2vec')
load('word2vec_mat.Rdata')


# extract TMDB movie metadata
setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/databases')
movie.metadata <- ExtractMovieInfo()

# create dataframe
#df <- movie.metadata[, c(3, 7)]
profit <- (movie.metadata$revenue / movie.metadata$budget) 
profit <- log(profit, 2)
df <- data.frame(profit=profit, budget= movie.metadata$budget/1000000 )
df <- as.data.frame(cbind(df, word2vec.mat))

# find movie titles with negative profit
movie.metadata$title[which(profit < 0)]


# compute RMSE using 10-fold cross-validation
K=10
folds <- GenerateCvFolds(K=10)
rmspe.perf <- mat.or.vec(10, 5)
mape.perf <- mat.or.vec(10, 5)
colnames(rmse) <- c('lm', 'lm-full', 'MARS', 'SVM', 'Random Forest')
for(k in 1:10)
{
  cat('Processing fold', k, '......\n')
  index.test <- folds[[k]]
  train <- df[-index.test, ]
  train <- train[complete.cases(train), ]
  test <- df[index.test, ]

  fit <- lm(profit ~ budget, data=train)
  y.pred <- predict(fit, test)
  #rmse[k, 1] <- sqrt(mean((y.pred - test$profit )^2, na.rm=TRUE))
  rmspe.perf[k, 1] <- rmspe(test$profit, y.pred)
  mape.perf[k, 1] <- mape(test$profit, y.pred)

  fit <- lm(profit  ~ ., data=train)
  y.pred <- predict(fit, test)
  #rmse[k, 2] <- sqrt(mean((y.pred - test$profit )^2, na.rm=TRUE))
  rmspe.perf[k, 2] <- rmspe(test$profit, y.pred)
  mape.perf[k, 2] <- mape(test$profit, y.pred)

  fit <- earth(as.numeric(profit ) ~ ., data=train)
  y.pred <- predict(fit, test)
  #rmse[k, 3] <- sqrt(mean((y.pred - test$profit )^2, na.rm=TRUE))
  rmspe.perf[k, 3] <- rmspe(test$profit, y.pred)
  mape.perf[k, 3] <- mape(test$profit, y.pred)

  fit <- svm(profit  ~ ., data=train)
  y.pred <- predict(fit, test)
  #rmse[k, 4] <- sqrt(mean((y.pred - test$profit )^2, na.rm=TRUE))
  rmspe.perf[k, 4] <- rmspe(test$profit, y.pred)
  mape.perf[k, 4] <- mape(test$profit, y.pred)

  y <- train$profit 
  X <- train[, -1]

  index.na <- which(is.na(test$profit)=='TRUE')
  if(length(index.na) > 0)
  {
    test <- test[-index.na, ]
  }

  rf <- randomForest(X, y, ntree=150)
  y.pred <- predict(rf, test)

  rmspe.perf[k, 5] <- rmspe(test$profit, y.pred)
  mape.perf[k, 5] <- mape(test$profit, y.pred)
  #rmse[k, 5] <- sqrt(mean((y.pred - test$profit)^2, na.rm=TRUE))
}

library(gbm)
rmspe.gbm <- mat.or.vec(10, 1)
for(k in 1:10)
{
  cat('Processing fold', k, '......\n')
  index.test <- folds[[k]]
  train <- df[-index.test, ]
  train <- train[complete.cases(train), ]
  test <- df[index.test, ]
  fit <- gbm(profit ~ ., distribution='gaussian', data=train)
  y.pred <- predict(fit, test[,-1], n.trees=50)
  rmspe.gbm[k] <- rmspe(test$profit, y.pred)
}


library(rpart)
rmspe.cart <- mat.or.vec(10, 1)
for(k in 1:10)
{
  cat('Processing fold', k, '......\n')
  index.test <- folds[[k]]
  train <- df[-index.test, ]
  train <- train[complete.cases(train), ]
  test <- df[index.test, ]
  fit <- rpart(profit ~ ., method='anova', data=train)
  y.pred <- predict(fit, test[,-1], n.trees=50)
  rmspe.cart[k] <- rmspe(test$profit, y.pred)
}

rmspe.final <- cbind(rmspe, rmspe.gbm, rmspe.cart)
colnames(rmspe.final) <- c('lm', 'lm-full', 'MARS', 'SVM', 'RF', 'GBM', 'CART')
setwd("/Users/ThomasVincent/Desktop/MillionDollarStory/model_evaluation")
save(rmspe.final, file='rmspe_profit_word2vec.Rdata')

# plot rmse boxplot
library(ggplot2)
library(reshape2)
setwd("/Users/ThomasVincent/Desktop/MillionDollarStory/model_evaluation")
load(file='rmspe_ratings_word2vec.Rdata')
df <- melt(rmspe.final)
colnames(df) <- c('fold', 'model', 'RMSPE')

png(file='rmspe_prediction_performance.png', height=400, width=800)
ggplot(df, aes(x=model, y=RMSPE)) + 
        geom_boxplot(lwd=1.2) +
        geom_hline(yintercept=1.975657, size=1.25, linetype='dotted', color='indianred3') +
        theme_bw() +
        xlab('') +
        theme(axis.text.x=element_text(size=22),
          axis.text.y=element_text(size=18),
          axis.title.y=element_text(size=22))
dev.off()
 






# fit randomforest on all data
y <- df$profit 
X <- df[, -1]
index.na <- which(is.na(df$profit)=='TRUE')
if(length(index.na) > 0)
{
  test <- df[-index.na, ]
}
else
{
  test <- df
}
rf.fit <- randomForest(X, y, ntree=150)
yhat <- predict(rf.fit, test)
plot(test$profit, yhat)
setwd("/Users/ThomasVincent/Desktop/MillionDollarStory/model_evaluation")
save(rf.fit, file='random_forest_profit_model.Rdata')

nrc.ytest <- train[100, 3:502]
budget.range <- (seq(1e6, 1e8, 1000000)) / 1000000
test.vec <- lapply(budget.range, function(x) as.vector(c(x, nrc.ytest)))
test.vec <- as.data.frame(do.call(rbind, test.vec))
colnames(test.vec) <- c('budget', 1:500)
yhat <- predict(rf, test.vec)
#plot(yhat, type='l', lwd=3)


#revenue = ((2^yhat) * budget.range)
#plot(budget.range, revenue, type='l', lwd=3)


df.chart <- data.frame(profit=log(movie.metadata$revenue, 10), 
                        budget=log(movie.metadata$budget, 10))
status <- mat.or.vec(nrow(df.chart), 1)
status[which(profit>0)] <- 'profit'
status[which(profit<0)] <- 'loss'
df.chart$status <- status

library(rCharts)
options(RCHART_HEIGHT = 500, RCHART_WIDTH = 500)
n1 <- rPlot(profit ~ budget, 
      data = df.chart, 
      color = "status", 
      type = "point")
n1$set(pointSize = 0.5)

n1


df <- data.frame(x=budget.range,y=yhat
ggplot(df, aes(x, y, colour=y)) + 
  geom_point() + 
  scale_colour_gradient(low="blue",high="red") +
  theme_bw() +
  geom_point(x=budget.range,y=revenue)

# plot prediction versus predicted for best performance RRF
pred.ratings <- true.ratings <- vector('list', 10)
for(k in 1:10)
{
  cat('Processing fold', k, '......\n')
  index.test <- folds[[k]]
  train <- df[-index.test, ]
  train <- train[complete.cases(train), ]
  test <- df[index.test, ]

  y <- train$profit 
  X <- train[, -1]

  index.na <- which(is.na(test$profit)=='TRUE')
  if(length(index.na) > 0)
  {
    test <- test[-index.na, ]
  }

  rf <- randomForest(X, y, ntree=150)
  y.pred <- predict(rf, test)
  pred.ratings[[k]] <- predict(rf, test)
  true.ratings[[k]] <- test$profit
}

df <- data.frame(truth=unlist(true.ratings), prediction=unlist(pred.ratings))


plot(unlist(true.ratings), unlist(pred.ratings), ylim=c(-300,600), xlim=c(-300,600))
lines(1:600, 1:600, lwd=2, col='red')

# plot prediction versus predicted for best performance RRF
library(h2o)
nn.ratings <- nn.true.ratings <- vector('list', 10)
df[, 1] <- as.numeric(as.vector(df[, 1]))
df[, 2] <- as.numeric(as.vector(df[, 2]))
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE) 
for(k in 1:10)
{
  cat('Processing fold', k, '......\n')
  index.test <- folds[[k]]
  train <- df[-index.test, ]
  train <- train[complete.cases(train), ]
  test <- df[index.test, ]

  write.csv(train, file='train.csv', quote=FALSE, row.names=FALSE, col.names=FALSE)
  write.csv(test, file='test.csv', quote=FALSE, row.names=FALSE, col.names=FALSE)
  train_h2o <- h2o.importFile(localH2O, path = "/Users/ThomasVincent/Desktop/MillionDollarStory/model_evaluation/train.csv")
  test_h2o <- h2o.importFile(localH2O, path = "/Users/ThomasVincent/Desktop/MillionDollarStory/model_evaluation/test.csv")
  y_train <- as.numeric(as.matrix(train_h2o[, 1]))
  y_test <- as.numeric(as.matrix(test_h2o[, 1]))
  ## Train the model
  model <- h2o.deeplearning(x = 2:502,  # column numbers for predictors
                            y = 1,   # column number for label
                            data = train_h2o,
                            activation = "Tanh",
                            classification = FALSE,
                            hidden = c(50,50,50),
                            epochs = 500)
  yhat_test <- h2o.predict(model, test_h2o)$predict
  yhat_test <- as.numeric(as.matrix(yhat_test))
  sqrt(mean((yhat_test - y_test)^2, na.rm=TRUE))

  pred.ratings[[k]] <- predict(rf, test)
  true.ratings[[k]] <- test$profit
}


