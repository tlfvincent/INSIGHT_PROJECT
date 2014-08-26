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
library(cvTools)
source('misc.R')

# extract TMDB movie metadata
setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/databases')
movie.metadata <- ExtractMovieInfo()
profit <- (movie.metadata$revenue / movie.metadata$budget) 
profit <- log(profit, 2)

# Generate folds for cross-validation
K=10
folds <- GenerateCvFolds(K=10)

# read in word2vec clusters
setwd('~/Desktop/MillionDollarStory/exploratory_analysis/word2vec')
size <- seq(100, 500, 100)
rmse.perf <- vector('list', length(size))
rmspe.perf <- vector('list', length(size))
for(s in 1:length(size))
{
  input.file <- sprintf('word2vec_mat_%s.Rdata', size[s])
  load(input.file)
  #row.names(word2vec.mat) <- movie.metadata$title

  # create dataframe
  df <- data.frame(profit=profit, budget= movie.metadata$budget/1000000 )
  df <- as.data.frame(cbind(df, word2vec.mat))
  #row.names(df) <- movie.metadata$title

  rmse.perf[[s]] <- mat.or.vec(10, 1)
  rmspe.perf[[s]] <- mat.or.vec(10, 1)
  # compute RMSE using 10-fold cross-validation
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

    rmspe.perf[[s]][k] <- rmspe(test$profit, y.pred)
    rmse.perf[[s]][k] <- sqrt(mean((y.pred - test$profit )^2, na.rm=TRUE))
  }
}



