setwd('~/Desktop/INSIGHT/PROJECT')
source('misc.R')
library(RSQLite)
library(ggplot2)
library(earth)
library(cvTools)

movie.metadata <- ExtractMovieInfo()

# compute Pearson and Spearman correlation
cor(movie.metadata$budget, movie.metadata$revenue, method='pearson')
cor(movie.metadata$budget, movie.metadata$revenue, method='spearman')

# run simple linear regression with only budget as covariate
fit <- lm(movie.metadata$revenue ~ movie.metadata$budget)
summary(fit)

# generate folds
folds <- GenerateCvFolds(K=10)

# concatenate movie metadata matrix and movie genre matrix
dat.with.genre <- movie.metadata
RMSE.lm.budgetOnly <- mat.or.vec(K, 1)
RMSE.mars.budgetOnly <- mat.or.vec(K, 1)
RMSE.lm <- mat.or.vec(K, 1)
RMSE.mars <- mat.or.vec(K, 1)
for(k in 1:K)
{
  #index.test <- which(folds$which == k)
  index.test <- folds[[k]]
  train <- dat.with.genre[-index.test, -c(1, 2, 5, 6, 7, 8, 9)]
  test <- dat.with.genre[index.test, -c(1, 2, 5, 6, 7, 8, 9)]
  
  # run multiple regression with only budget covariate
  fit <- lm(revenue ~ budget, data=train)
  y.pred <- predict(fit, test)
  RMSE.lm.budgetOnly[k] <- sqrt(mean((test$revenue - y.pred)^2))

  # run MARS with only budget covariate
  fit <- earth(revenue ~ budget, data = train)
  y.pred <- predict(fit, test$budget)
  RMSE.mars.budgetOnly[k] <- sqrt(mean((test$revenue - y.pred)^2))

  # run multiple regression with multiple covariates
  fit <- lm(revenue ~ ., data=train)
  y.pred <- predict(fit, test)
  RMSE.lm[k]<- sqrt(mean((test$revenue - y.pred)^2))

    # run MARS with only budget covariate
  fit <- earth(revenue ~ ., data = train)
  y.pred <- predict(fit, test)
  RMSE.mars[k] <- sqrt(mean((test$revenue - y.pred)^2))
}

# implement randomforest
library(randomForest)
RMSE.rf <- mat.or.vec(10, 1)
for(k in 1:K)
{
  index.test <- folds[[k]]
  train <- dat.with.genre[-index.test, -c(1, 2, 5, 6, 7, 8, 9)]
  test <- dat.with.genre[index.test, -c(1, 2, 5, 6, 7, 8, 9)]
  rf <- randomForest(revenue ~ ., data=train, ntree=50)
  y.pred <- predict(rf, test)
  RMSE.rf[k] <- sqrt(mean((test$revenue - y.pred))^2)
}

# implement BLR
library(BLR)
RMSE.blr <- mat.or.vec(10, 1)
for(k in 1:K)
{
  yNa <- dat.with.genre$revenue
  whichNa <- folds[[k]]
  yNa[whichNa]<-NA
  blr <- BLR(y=yNa, XF=dat.with.genre[, -c(1, 2, 5, 6, 7, 8, 9)],
        nIter=5500,burnIn=500,thin=1)
  #yHatCV[whichNa]<-blr$yHat[blr$whichNa]
  y.pred <- blr$yHat[blr$whichNa]
  RMSE.blr[k] <- sqrt(mean((test$revenue- y.pred))^2)
}

# plot RMSE obtained by multiple regression and MARS at each fold  
df <- data.frame(RMSE=c(RMSE.lm.budgetOnly, 
                      RMSE.mars.budgetOnly, 
                      RMSE.lm, 
                      RMSE.mars, 
                      RMSE.rf,
                      RMSE.blr), 
          Model=c(rep('lm-budgetOnly', 10), 
                  rep('MARS-budgetOnly', 10), 
                  rep('lm', 10), 
                  rep('MARS', 10),
                  rep('RF', 10),
                  rep('BLR', 10)), 
          fold=rep(1:10, times=6))

df$Model <- factor(df$Model, levels=c('lm-budgetOnly', 'lm', 'MARS-budgetOnly', 'MARS', 'BLR', 'RF'))

png(file='box_office_prediction_performance.png', width=900, height=350)
ggplot(df, aes(x=factor(fold), y=RMSE, fill=Model)) +
      theme_bw() +
      geom_bar(colour="black", stat="identity", position=position_dodge(), size=.1) +
      scale_fill_manual(values=c("skyblue4", "royalblue4", 'red2', 'tomato1', '#E69F00', 'darkgreen')) +
      theme(axis.text.y  = element_text(size=16),
            axis.title.y  = element_text(size=20),
            axis.text.x  = element_text(size=16),
            axis.title.x  = element_text(size=20)) +
      theme(legend.title=element_text(size=16),
            legend.text = element_text(size = 14)) +
      ylab('RMSE') + xlab('Fold')
dev.off()
