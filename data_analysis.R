setwd('~/Desktop/INSIGHT/PROJECT')
library(RSQLite)
library(ggplot2)

file.dat <- scan(file='tmdb_metadata.tab', what='raw()', sep='\n', )
headers <- strsplit(file.dat[1], split='\t')[[1]]
temp.metadata <- mat.or.vec(length(file.dat)-1, 9)
colnames(temp.metadata) <- headers
#row.names(movie.ratings) <- movie.title$Title
for(f in 2:length(file.dat))
{
  temp <- strsplit(file.dat[f], split='\t')[[1]]
  #if(temp[1] != 'NULL')
  #{
    # record gene of movie
    temp.metadata[f-1, ] <- temp
}

# only keep rows with non null entries in budget, revenue, year, runtime and genre
null.entries <- apply(temp.metadata, 2,  function(x) which(x == 'NULL'))
rows.to.remove <- unlist(null.entries[c(2, 3, 4, 6, 9)])
movie.metadata <- temp.metadata[-rows.to.remove, ]

# convert matrix to dataframe
movie.metadata <- as.data.frame(movie.metadata)
movie.metadata$budget <- as.numeric(as.matrix(movie.metadata$budget))
movie.metadata$revenue <- as.numeric(as.matrix(movie.metadata$revenue))
movie.metadata$runtime <- as.numeric(as.matrix(movie.metadata$runtime))
movie.metadata$vote_average <- as.numeric(as.matrix(movie.metadata$vote_average))

# compute Pearson and Spearman correlation
cor(movie.metadata$budget, movie.metadata$revenue, method='pearson')
cor(movie.metadata$budget, movie.metadata$revenue, method='spearman')

# run simple linear regression with only budget as covariate
fit <- lm(movie.metadata$revenue ~ movie.metadata$budget)
summary(fit)

# run MARS model
# perform 10-fold CV using linear regression and MARS
library(earth)
library(cvTools)

K <- 10 #the number of folds
folds <- split(sample(nrow(movie.metadata)),rep(1:K,length=nrow(movie.metadata))) # defne the fold ids
#folds <- cvFolds(nrow(dat), K=K)
dat <- movie.metadata
dat$revenue <- log(dat$revenue, 10)
dat$budget <- log(dat$budget, 10)
RMSE.lm.budgetOnly <- mat.or.vec(K, 1)
RMSE.mars.budgetOnly <- mat.or.vec(K, 1)
for(k in 1:K)
{
  #index.test <- which(folds$which == k)
  index.test <- folds[[k]]
  train <- dat[-index.test, ]
  test <- dat[index.test, ]
  
  # run multiple regression
  fit <- lm(revenue ~ budget, data=train)
  y.pred <- predict(fit, test)
  RMSE.lm.budgetOnly[k] <- sqrt(mean((test$revenue - y.pred)^2))

  # run MARS
  fit <- earth(revenue ~ budget, data = train)
  y.pred <- predict(fit, test$budget)
  RMSE.mars.budgetOnly[k] <- sqrt(mean((test$revenue - y.pred)^2))
}

# create matrix of movie genres
movies.by.genre <- lapply(dat$genre, function(x) strsplit(as.vector(x), split='-', fixed=TRUE)[[1]])
unique.genre <- unique(unlist(movies.by.genre))
genre.matrix <- mat.or.vec(nrow(movie.metadata), length(unique.genre))
colnames(genre.matrix) <- unique.genre
for(i in 1:length(movies.by.genre))
{
  index <- match(movies.by.genre[[i]], unique.genre)
  genre.matrix[i, index] <- 1
}

# concatenate movie metadata matrix and movie genre matrix
dat.with.genre <- cbind(dat, as.data.frame(genre.matrix))
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

ggplot(df, aes(x=factor(fold), y=RMSE, fill=Model)) +
      theme_bw() +
      geom_bar(colour="black", stat="identity", position=position_dodge(), size=.1) +
      scale_fill_manual(values=c("skyblue4", "royalblue4", 'red2', 'tomato1', '#E69F00', 'darkgreen')) +
      theme(axis.text.y  = element_text(size=20),
            axis.title.y  = element_text(size=28),
            axis.text.x  = element_text(size=20),
            axis.title.x  = element_text(size=26)) +
      theme(legend.title=element_text(size=20),
            legend.text = element_text(size = 15)) +
      ylab('RMSE') + xlab('Fold')


