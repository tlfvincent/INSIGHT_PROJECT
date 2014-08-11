setwd('~/Desktop/MillionDollarStory')
source('misc.R')
library(RSQLite)
library(ggplot2)
library(earth)
library(cvTools)
library(rMaps)
librar(gridExtra)

#movie.metadata <- ExtractMovieInfo()
movie.profit <- ExtractMovieProfit()
imdb.info <- ExtractIMDBdata()

######################################################
# plot budget vs. worldwide revenue
budget <- as.numeric(gsub(',', '', as.vector(movie.profit$budget)))
worldwide <- as.numeric(gsub(',', '', as.vector(movie.profit$worldwide)))
domestic <- as.numeric(gsub(',', '', as.vector(movie.profit$domestic)))
valid.index <- intersect(which(budget!=0), which(worldwide!=0))
df <- data.frame(budget=log(budget[valid.index], 10), worldwide=log(worldwide[valid.index], 10))
ggplot(df, aes(x=budget, y=worldwide)) +
  geom_point(size=2.5, alpha=0.7, color='royalblue3') +
  geom_smooth(method='lm', size=2, color='black') +
  theme_bw() +
  ylab('Worldwide Gross Profit') + xlab('Budget') +
  theme(axis.text.x=element_text(size=18), 
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=15))

# plot domestic vs. worldwide revenue
valid.index <- intersect(which(worldwide!=0), which(domestic!=0))
df <- data.frame(worldwide=log(worldwide[valid.index], 10), 
        domestic=log(domestic[valid.index], 10),
        title=movie.profit$title[valid.index])
ggplot(df, aes(x=domestic, y=worldwide)) +
  geom_point(size=2.5, alpha=0.7, color='royalblue3') +
  geom_smooth(method='lm', size=2, color='black') +
  theme_bw() +
  xlab('Domestic Gross Profit') + ylab('Worlwide Gross Profit') +
  theme(axis.text.x=element_text(size=18), 
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=15))

# find movies that are most different between domestic and worldwide profit
index.order <- order(df[,1]/df[,2], decreasing=TRUE)
top.movies <- df$title[index.order][1:20]
match(top.movies, imdb.info$Title)

######################################################
# find countries where movies are produced
countries <- as.vector(unlist(sapply(imdb.info$Country, 
                function(x) as.vector(strsplit(x, split=', ', fixed=TRUE)[[1]]))))
countries[which(countries=='West Germany')] <- 'Germany'
countries[which(countries=='Hong Kong')] <- 'China'
countries[which(countries=='Soviet Union')] <- 'Russia'
table.countries <- table(countries)
country.hits <- data.frame(country=names(table.countries), 
                  movie_count=as.vector(table.countries))
# import all countries avaliable in DataMaps / rMaps
country.dat <- read.csv(file='~/Desktop/MillionDollarStory/data/datamaps_countries.txt', header=FALSE) 
country <- as.vector(country.dat[, 1])
country.abb <- as.vector(country.dat[, 2])
index.country <- match(country.hits$country, country)
# add country abbreviation to dataframe
country.hits$abb <- country.abb[index.country]
# plot world map of movie production
map <- ichoropleth_edit(movie_count ~ abb, 
                data=country.hits, 
                map='world', 
                ncuts = 9, 
                pal = 'YlOrRd',
                labels=T)

######################################################
# plot distribution fo runtime for movies
movie.year <- imdb.info$Year
movie.runtime <- imdb.info$Runtime
movie.runtime <- unlist(sapply(movie.runtime, 
                function(x) strsplit(x, split =' ', fixed=TRUE)[[1]][1]))
movie.runtime <- as.numeric(movie.runtime) # change runtime to numeric
df <- data.frame(runtime=movie.runtime, year=movie.year) # add year info
df <- df[complete.cases(df), ] # remove NAs
df <- df[-which(df$runtime <= 10), ] # remove movies shorter than 10 minutes

# plot histogram distribution of movie runtimes
ggplot(df, aes(x=runtime)) + 
    geom_histogram(aes(y=..density..), binwidth=5, colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    theme_classic()
# plot runtime versus year of production
ggplot(df, aes(x=factor(year), y=runtime)) +
    geom_boxplot() +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))

######################################################
# make image analysis of picture posters


######################################################
# performen sentiment analysis on tomato consensus review
review <- imdb.info$tomatoConsensus


######################################################
# correlate ratings, runtime with production
movie.production <- imdb.info$Production
movie.ratings <- imdb.info$imdbRating
movie.runtime <- imdb.info$Runtime
movie.runtime <- unlist(sapply(movie.runtime, 
                function(x) strsplit(x, split =' ', fixed=TRUE)[[1]][1]))
movie.runtime <- as.numeric(movie.runtime) # change runtime to numeric
df <- data.frame(runtime=movie.runtime, 
                  production=movie.production, 
                  rating=movie.ratings)
df <- df[complete.cases(df), ] # remove NAs

df[which(), ]
ggplot(df, aes(x=factor(production), y=rating)) +
    geom_boxplot() +
    theme_bw() 

