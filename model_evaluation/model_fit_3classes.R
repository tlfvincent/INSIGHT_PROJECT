setwd('~/Desktop/MillionDollarStory/model_evaluation')
library(RSQLite)
library(superpc)
source('misc.R')

setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/databases')
# extract TMDB movie metadata
movie.metadata <- ExtractMovieInfo()
# read in sentiment word matrix
#sentiment.dat <- ExtractMovieSentimentMatrix()
# extract movie script readability
#movie.readability <- ExtractMovieReadability()


setwd('/Users/ThomasVincent/Desktop/MillionDollarStory/create_tdm_idf')

#tdm <- read.table(file='tdm_idf.tab', sep='\t', header=TRUE)

# read in term document matrix
tdm <- scan(file='tdm_idf.tab', sep='\n', what='raw()')
headers <- strsplit(tdm[1], split='\t', fixed=TRUE)[[1]][-1]
tdm.mat <- mat.or.vec(length(tdm)-1, length(headers))
for(i in 2:length(tdm))
{
  temp <- strsplit(tdm[i], split='\t', fixed=TRUE)[[1]][-1]
  tdm.mat[i-1,] <- as.numeric(temp)
}

# define covariate matrix to use for model fitting
df <- movie.metadata[, c(3, 4, 10:42)]
df$revenue <- log(df$revenue, 10)
df$budget <- log(df$budget, 10)

# compute profit ratio and define response labels
profit.ratio <- df$revenue / df$budget
labels <- mat.or.vec(nrow(df), 1)
labels[which(profit.ratio>= 0.9)] <- 1
labels[which(profit.ratio>= 1.1)] <- 2
prior <- table(labels) / sum(labels)

# fit linear regression model with budget only
#fit <- lm(revenue ~ budget, data=df)
#summary(fit)

# fit lasso model 
library(lars)
library(LiblineaR)
all.dat <- as.matrix(cbind(df[, -2], tdm.mat))
y <- df[, 'revenue']

K=10
folds <- GenerateCvFolds(K=10)
library(nnet)
library(caTools)
library(caret)
auc <- mat.or.vec(10, 2)
for(k in 1:10)
{
  index.test <- folds[[k]]
  train <- df[-index.test, ]
  test <- df[index.test, ]
  train$labels <- labels[-index.test]
  test$labels <- labels[index.test]
  #fit <- lars(X, y, type='lar', use.Gram=FALSE)
  fit <- multinom(labels ~ budget, data=train)
  y.pred <- predict(fit, test, type='probs')
  pred1 <- apply(y.pred, 1, function(x) which.max(x/prior)[1] - 1)
  auc[k, 1] <- confusionMatrix(pred1, test$labels, prevalence=prior)$overall[1]

  X <- all.dat[-index.test, ]
  y <- labels[-index.test]
  X.test <- all.dat[index.test, ]
  y.test <- labels[index.test]
  # Center and scale data
  #s=scale(X, center=TRUE, scale=TRUE)
  # Tune the cost parameter of a logistic regression according to the Joachim's heuristics
  #co=heuristicC(s)
  m=LiblineaR(data=X,
          labels=y,
          type=6,
          cost=1,
          bias=TRUE,
          verbose=FALSE)
  # Scale the test data
  #s2=scale(X.test, attr(s,"scaled:center"), attr(s,"scaled:scale"))
  p=predict(m, X.test, proba=TRUE, decisionValues=TRUE)
  index.order <- colnames(p$probabilities)
  pred1 <- apply(p$probabilities, 1, function(x) which.max(x/prior[index.order])[1] - 1)
  auc[k, 2] <- confusionMatrix(pred1, y.test, prevalence=prior)$overall[1]
}



res=table(p$predictions, y.test)
print(res)

# Display confusion matrix
res=table(p$predictions,yTest)




# fit supervised PCA
X <- t(as.matrix(cbind(df[, -2], tdm.mat)))
featurenames <- colnames(X)
y <- df[, 'revenue']
data<-list(x=X, y=y, featurenames=featurenames)

a <- superpc.train(data, type="regression")
aa <- superpc.cv(a, data)
iter.max

K=10
folds <- GenerateCvFolds(K=10)
k <- 1
index.test <- folds[[k]]
train <- X[, -index.test]
test <- X[, index.test]

data <- list(x=train, y=y[-index.test], featurenames=featurenames)
data.test <- list(x=test, y=y[index.test], featurenames=featurenames)
a <- superpc.train(data, type="regression")
fit <- superpc.predict(a, data, data.test, 
      threshold=0.5, 
      n.components=3, 
      prediction.type="continuous")

superpc.fit.to.outcome(a, data.test, fit$v.pred)


plot(fit$v.pred[,1], data.test$y)
plot(fit$v.pred[,1], fit$v.pred[,2])

fit.red<- superpc.predict.red(a, data, data.test, threshold=.5)
superpc.listfeatures(data, a, fit.red, num.features=20)


