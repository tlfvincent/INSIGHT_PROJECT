setwd('~/Desktop/MillionDollarStory/movie_data_analysis')
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
K=10
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

library(rpart)
RMSE.cart <- mat.or.vec(10, 1)
for(k in 1:K)
{
  #index.test <- which(folds$which == k)
  index.test <- folds[[k]]
  train <- dat[-index.test, ]
  test <- dat[index.test, ]
  cart <- rpart(revenue ~ ., data=train, method="anova")
  y.pred <- predict(cart, test)
  RMSE.cart[k] <- sqrt(mean((test$revenue - y.pred)^2, na.rm=TRUE))
}

# plot RMSE obtained by multiple regression and MARS at each fold  
df <- data.frame(RMSE=c(RMSE.lm.budgetOnly, 
                      RMSE.mars.budgetOnly, 
                      RMSE.lm, 
                      RMSE.mars, 
                      RMSE.rf,
                      RMSE.cart), 
          Model=c(rep('lm-budgetOnly', 10), 
                  rep('MARS-budgetOnly', 10), 
                  rep('lm', 10), 
                  rep('MARS', 10),
                  rep('RF', 10),
                  rep('CART', 10)), 
          fold=rep(1:10, times=6))

df$Model <- factor(df$Model, levels=c('lm-budgetOnly', 'lm', 'MARS-budgetOnly', 'MARS', 'CART', 'RF'))
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





















# read in data from SQL database
drv <- dbDriver('SQLite')
con <- dbConnect(drv, 'movie_titles_year.db')

# list all the tables in the database
dbListTables(con)
query <- sprintf('SELECT *  FROM MOVIE_TITLE')
#query <- sprintf('SELECT *  FROM NBA WHERE YEAR=%s', year[y])
movie.title = dbGetQuery(con, query)
dbDisconnect(con)

# define target variables
kpi <- c('Genre', 'tomatoMeter', 'tomatoRating', 'tomatoUserRating', 'imdbRating', 'BoxOffice')







# read basic information on movies including 
# boxoffice returns and ratings
file.dat <- scan(file='imdb_rt_api.tab', what='raw()', sep='\n')
movie.genre <- vector('list', length(file.dat))
names(movie.genre) <- movie.title$Title
movie.ratings <- mat.or.vec(length(file.dat), length(kpi)-1)
row.names(movie.ratings) <- movie.title$Title
colnames(movie.ratings) <- kpi[-1]
for(f in 1:length(file.dat))
{
  temp <- strsplit(file.dat[f], split='\t')[[1]]
  if(temp[1] != 'NULL')
  {
    # record gene of movie
    movie.genre[[f]] <- strsplit(temp[1], split=', ')[[1]]
    # extrcat box office returns
    temp[6] <- gsub('\\$', '', temp[6])
    sum.money <- substr(temp[6], nchar(temp[6]), nchar(temp[6]))
    if(sum.money == 'M')
    {
      temp[6] <- as.numeric(substr(temp[6], 1, nchar(temp[6])-1)) * 1000000
    }
    else if(sum.money == 'k')
    {
      temp[6] <- as.numeric(substr(temp[6], 1, nchar(temp[6])-1))*1000
    }
    # store ratinsg and returns in matrix
    temp.ratings <- temp[-1]
    for(t in 1:length(temp.ratings))
    {
      if(temp.ratings[t]!='N/A')
      {
        movie.ratings[f, t] <- as.numeric(temp.ratings[t])
      }
    }
  }
}

# remove rows with no data
row.remove <- which(apply(movie.ratings, 1, sd) == 0)
movie.ratings <- movie.ratings[-row.remove, ]
movie.title <- movie.title[-row.remove, ]
movie.genre <- movie.genre[-row.remove]

boxplot(movie.ratings[, c(2, 4, 3)])

# plot dictribution of box office returns and ratings
returns <- as.vector(movie.ratings[, 5])
returns <- returns[which(returns!=0)]
hist(log(returns, 10), breaks=100)

ratings.rt <- as.vector(movie.ratings[, 2])
ratings.rt <- ratings.rt[which(ratings.rt!=0)]
ratings.imdb <- as.vector(movie.ratings[, 4])
ratings.imdb <- ratings.imdb[which(ratings.imdb!=0)]
par(mfrow=c(1, 2))
hist(ratings.rt, breaks=100, xlim=c(0,10), xlab='ratings', ylab='frequency', main='Rotten Tomatoes')
hist(ratings.imdb, breaks=100, xlim=c(0,10), xlab='ratings', ylab='frequency', main='IMDB')

# plot correlation between user ratings in IMDB and RT
df <- data.frame(imdb=movie.ratings[,4], rt=movie.ratings[,2])
ggplot(df, aes(x=rt, y=imdb)) +
      geom_point() +
      theme_classic() +
      ylab('IMDB user ratings') + xlab('RT user ratings') +
       theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
             axis.text.y=element_text(size=15),
             axis.title.y=element_text(size=18))

# plot correlation between user ratings in IMDB and box office returns
df <- data.frame(imdb=movie.ratings[,4], returns=movie.ratings[,5])
ggplot(df, aes(x=imdb, y=log(returns, 10))) +
      geom_point() +
      theme_classic() +
      ylab('log10(Box office returns)') + xlab('IMDB user ratings') +
       theme(axis.text.x=element_text(size=15),
            axis.title.x=element_text(size=18),
             axis.text.y=element_text(size=15),
             axis.title.y=element_text(size=18))

              +
       ylim(0, 2e8)

# plot all genres compared to box office returns
all.genres <- unique(unlist(movie.genre))
# create matrix with box office returns
returns.by.genre <- mat.or.vec(nrow(movie.ratings), length(all.genres))
genre <- money <- c()
for(i in 1:nrow(movie.ratings))
{
  cashflow <- movie.ratings[i, 5]
  if(cashflow != 0)
  {
    index.genre <- match(movie.genre[[i]], all.genres)
    returns.by.genre[i, index.genre] <- cashflow
    genre <- c(genre, movie.genre[[i]])
    money <- c(money, rep(cashflow, length(movie.genre[[i]])))
  }
}
row.remove <- which(apply(returns.by.genre, 1, sd) == 0)
returns.by.genre <- returns.by.genre[-row.remove, ]

df <- data.frame(returns=money, genre=genre)
ggplot(df, aes())

# add year column to dataframe
movie.ratings <- as.data.frame(movie.ratings)
movie.ratings$year <- movie.title$Year

#unique.year <- sort(unique(movie.ratings$year), decreasing=FALSE)
unique.year <- 1980:2014
median.returns <- mat.or.vec(length(unique.year), 1)
for(y in 1:length(unique.year))
{
  index.year <- which(movie.ratings$year == unique.year[y])
  returns <- as.numeric(as.matrix(movie.ratings$BoxOffice[index.year]))
  median.returns[y] <- sum(returns)
  #null.entries <- which(returns == 0)
  #if(length(null.entries) > 0){returns <- returns[-null.entries]}
  #if(length(returns) > 0){median.returns[y] <- median(returns)}
}

df <- data.frame(returns=median.returns, year=unique.year)
ggplot(df, aes(x=factor(year), y=returns, fill=returns)) + 
       geom_bar(stat='identity', colour="black") +
       theme_classic() +
       ylab('$Box Office Returns$') + xlab('Year') +
       theme(axis.text.x=element_text(size=15, angle=90),
              axis.title.x=element_text(size=18),
              axis.text.y=element_text(size=15),
              axis.title.y=element_text(size=18),
              legend.title=element_text(size=14)) + 
       scale_fill_gradient2(low='white', mid='lightblue', high='darkblue', space='Lab')





# read basic information on movies including 
# boxoffice returns and ratings
drv <- dbDriver('SQLite')
db <- dbConnect(drv, dbname="movie_ratings.db")
dbSendQuery(conn = db,
       "CREATE TABLE MOVIE_RATINGS
       (tomatoMeter INTEGER,
        tomatoRating FLOAT,
        tomatoUserRating FLOAT,
        genre TEXT,
        imdbRating FLOAT,
        BoxOffice INTEGER)")

file.dat <- scan(file='imdb_rt_api_old.tab', what='raw()', sep='\n')
for(f in 1:length(file.dat))
{
  temp <- strsplit(file.dat[f], split='\t')[[1]]
  temp[4] <- gsub(', ', '-', temp[4])
  temp[6] <- gsub('\\$', '', temp[6])
  sum.money <- substr(temp[6], nchar(temp[6]), nchar(temp[6]))
  if(sum.money == 'M')
  {

    temp[6] <- as.numeric(substr(temp[6], 1, nchar(temp[6])-1)) * 1000000
  }
  else if(sum.money == 'k')
  {
    temp[6] <- as.numeric(substr(temp[6], 1, nchar(temp[6])-1))*1000
  }
  stmt <- sprintf('INSERT INTO MOVIE_RATINGS VALUES (%s, %s, %s, %s, %s, %s)',
        temp[1], temp[2], temp[3], temp[4], temp[5], temp[6])
  dbSendQuery(conn = db, stmt)
}
dbDisconnect(db)