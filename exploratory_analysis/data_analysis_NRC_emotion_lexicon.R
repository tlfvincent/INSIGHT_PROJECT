setwd('~/Desktop/MillionDollarStory')
source('misc.R')
library(RSQLite)
library(ggplot2)
library(earth)
library(cvTools)
library(rMaps)
library(rCharts)
library(gridExtra)
library(knitr)
library(RColorBrewer)
library(nnet)
library(reshape2)
library(googleVis)
cols = brewer.pal(9, 'Set1')


#movie.metadata <- ExtractMovieInfo()
movie.profit <- ExtractMovieProfit()
imdb.info <- ExtractIMDBdata()

# read in sentiment data
setwd("/Users/ThomasVincent/Desktop/MillionDollarStory/data")
sentiment <- read.table(file='NRC_emotion_lexicon.txt', sep='\t', header=FALSE)
unique.feeling <- as.vector(unique(sentiment[, 2]))
unique.word <- as.vector(unique(sentiment[, 1]))
sentiment.mat <- mat.or.vec(length(unique.word), length(unique.feeling))
for(w in 1:length(unique.word))
{
  index <- which(sentiment[, 1] == unique.word[w])
  sentiment.mat[w, ] <- as.vector(sentiment[index, 3])
}

# find feelings in scripts


setwd("~/Desktop/MillionDollarStory/exploratory_analysis")
######################################################
# plot histogram of profit margins
budget <- as.numeric(gsub(',', '', as.vector(movie.profit$budget)))
worldwide <- as.numeric(gsub(',', '', as.vector(movie.profit$worldwide)))
profit <- (worldwide - budget) / 1000000
hist(profit, breaks=75)

setwd("~/Desktop/INSIGHT/shiny_app/html")
save(movie.profit[, -c(1,2)], file='movie_profit.Rdata')
PopTable <- gvisTable(as.data.frame(movie.profit[1:200, ]), 
                      options=list(page='enable'))
plot(PopTable)


######################################################
# plot budget vs. worldwide revenue
budget <- as.numeric(gsub(',', '', as.vector(movie.profit$budget)))
worldwide <- as.numeric(gsub(',', '', as.vector(movie.profit$worldwide)))
domestic <- as.numeric(gsub(',', '', as.vector(movie.profit$domestic)))
valid.index <- intersect(which(budget!=0), which(worldwide!=0))
df <- data.frame(budget=log(budget[valid.index], 10), worldwide=log(worldwide[valid.index], 10))
png(file='img1_budget_worldwide_revenue.png', width=500, height=450)
ggplot(df, aes(x=budget, y=worldwide)) +
  geom_point(size=2.5, alpha=0.7, color='royalblue3') +
  geom_smooth(method='lm', size=2, color='black') +
  theme_bw() +
  ylab('Worldwide Gross Profit') + xlab('Budget') +
  theme(axis.text.x=element_text(size=18), 
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=15))
dev.off()

# plot domestic vs. worldwide revenue
valid.index <- intersect(which(worldwide!=0), which(domestic!=0))
df <- data.frame(worldwide=log(worldwide[valid.index], 10), 
        domestic=log(domestic[valid.index], 10),
        title=movie.profit$title[valid.index])
png(file='img2_domestic_worldwide_revenue.png', width=500, height=450)
ggplot(df, aes(x=domestic, y=worldwide)) +
  geom_point(size=2.5, alpha=0.7, color='royalblue3') +
  geom_smooth(method='lm', size=2, color='black') +
  theme_bw() +
  xlab('Domestic Gross Profit') + ylab('Worlwide Gross Profit') +
  theme(axis.text.x=element_text(size=18), 
        axis.title.x=element_text(size=15),
        axis.text.y=element_text(size=18),
        axis.title.y=element_text(size=15))
dev.off()

# find movies that are most different between domestic and worldwide profit
index.order <- order(df[,1]/df[,2], decreasing=TRUE)
top.movies <- df$title[index.order][1:20]
match(top.movies, imdb.info$Title)

######################################################
# plot dependence of profit against month and year release date
budget <- as.numeric(gsub(',', '', as.vector(movie.profit$budget)))
worldwide <- as.numeric(gsub(',', '', as.vector(movie.profit$worldwide)))
profit <- (worldwide - budget) / 1000000
hist(profit, breaks=75)

# plot profit margins by month
df <- data.frame(profit=profit, month=movie.profit$month)
df$month <- factor(df$month, levels=1:12)
season <- c('winter', 'winter', 'spring', 'spring', 'spring', 'summer', 'summer', 'summer', 'fall', 'fall', 'fall', 'winter')
df$season <- season[df$month]
png(file='img3_profit_by_month.png', width=800, height=450)
ggplot(df, aes(x=month, y= profit, fill=season)) +
  geom_boxplot(outlier.size=0, alpha=0.4) +
  geom_point(alpha = 0.3, size=1) +
  theme_bw() + 
  ylim(0, 500) +
  xlab('Month') + ylab('Profit Ratio in Millions\n(Worldwide gross / budget)') +
  theme(axis.text.x=element_text(size=15), 
        axis.title.x=element_text(size=18),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=18))
dev.off()

# plot profit margins by year
df <- data.frame(profit=profit, year=as.numeric(as.vector(movie.profit$year)))
df <- df[which(df$year>=1980), ]
png(file='img4_profit_by_year.png', width=800, height=450)
ggplot(df, aes(x=factor(year), y= profit)) +
  geom_boxplot(outlier.size=0, alpha=0.4) +
  geom_point(alpha=0.5, size=1) +
  theme_bw() +
  ylim(0, 500) +
  ylab('Profit Ratio in Millions\n(Worldwide gross / budget)') + xlab('Year') +
  theme(axis.text.x=element_text(size=13, angle=90, hjust=0), 
        axis.title.x=element_text(size=16),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=18))
dev.off()

# find movie released in 1992
#kable(head(movie.profit[which(movie.profit$year==1992), -c(1:2)], 20))
movies.released.1992 <- head(movie.profit[which(movie.profit$year==1992), -c(1:2)], 50)
setwd("~/Desktop/INSIGHT/shiny_app/html")
save(movies.released.1992, file='movies_released_1992.Rdata')
PopTable <- gvisTable(as.data.frame(movies.released.1992), 
                      options=list(page='enable'))
plot(PopTable)


# plot time series for each month of each year
date <- paste(movie.profit$day, movie.profit$month, movie.profit$year, sep='/')
date <- as.Date(date, format="%d/%m/%Y")
df <- data.frame(profit=profit, date=date, title=movie.profit$title)
#df$month <- factor(df$month, levels=1:12)
#df.summary <- by(df$profit, df$date, median)
#df <- data.frame(date=names(df.summary), profit=as.vector(df.summary))
ggplot(df, aes(x=date, y= profit)) +
  geom_point(alpha=0.7, size=2, color='royalblue3') +
  #geom_smooth() +
  theme_classic() +
  ylim(0, 500)


#p1 <- rPlot(profit ~ date, data = df, type = 'point', 
#  size = list(const = 2), color = list(const = '#888'), 
#  tooltipContent="#!function(item){return item.title +'\n' + item.profit + '\n' + item.date}!#"
#)
#p1$print('chart1')

#r1 <- rPlot(profit ~ date, data = df, type = 'point')
#r1$addControls('x', 'wt', names(mtcars))
#r1$addControls('y', 'mpg', names(mtcars))
#r1$addControls('color', 'gear', names(mtcars))

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
png(file='movie_runtime_distribution.png', width=550, height=450)
ggplot(df, aes(x=runtime)) + 
    geom_histogram(aes(y=..density..), binwidth=5, colour="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    theme_classic() +
    theme(axis.text.x=element_text(size=13, angle=90, hjust=0), 
        axis.title.x=element_text(size=16),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=18))
dev.off()

# plot runtime versus year of production
png(file='movie_runtime_vs_year.png', width=800, height=450)
ggplot(df, aes(x=factor(year), y=runtime)) +
    geom_boxplot() +
    theme_bw() +
    ylim(0, 250) +
    theme(axis.text.x = element_text(angle=90)) +
    theme(axis.text.x=element_text(size=13, angle=90, hjust=0), 
        axis.title.x=element_text(size=16),
        axis.text.y=element_text(size=15),
        axis.title.y=element_text(size=18))
dev.off()

######################################################
# find power of directors
budget <- as.numeric(gsub(',', '', as.vector(movie.profit$budget)))
worldwide <- as.numeric(gsub(',', '', as.vector(movie.profit$worldwide)))
profit <- (worldwide - budget) / 1000000

all.directors <- imdb.info$Director
all.directors <- all.directors[-which(all.directors=='NULL')]
all.directors <- all.directors[-which(all.directors=='N/A')]
unique.directors <- unique(imdb.info$Director)
hist(table(all.directors))
# find profit ratio by director
profit.by.director <- vector('list', length(unique.directors))
names(profit.by.director) <- unique.directors
for(i in 1:length(unique.directors))
{
  index <- which(imdb.info$Director == unique.directors[i])
  profit.by.director[[i]] <- profit[index]
}
# compute mean profit ratio per director
mean.profit <- sort(unlist(lapply(profit.by.director, mean)), decreasing=TRUE)
# plot table of profit for top directors
top.rank <- c()
for(i in 1:50)
{
  temp<-cbind(imdb.info[which(imdb.info$Director==names(mean.profit)[i]), c(4, 18, 24, 19)],
            movie.profit[which(imdb.info$Director==names(mean.profit)[i]), c(5, 7)])
  top.rank <- rbind(top.rank, temp)
}
setwd("~/Desktop/INSIGHT/shiny_app/html")
save(top.rank, file='top_directors.Rdata')
PopTable <- gvisTable(as.data.frame(top.rank), 
                      options=list(page='enable'))
plot(PopTable)

######################################################
# find power of actors
all.actors <- imdb.info$Actors
actor.lineup <- sapply(all.actors, function(x) strsplit(x, split=', ', fixed=TRUE)[[1]])
all.actors <- as.vector(unlist(actor.lineup))
all.actors <- all.actors[-which(all.actors=='NULL')]
all.actors <- all.actors[-which(all.actors=='N/A')]
unique.actors <- unique(all.actors)
# find profit ratio by actor
profit.by.actor <- vector('list', length(unique.actors))
names(profit.by.actor) <- unique.actors
for(i in 1:length(unique.actors))
{
  total.profit <- c()
  index <- unlist(lapply(actor.lineup, 
                      function(x) length(which(x == unique.actors[i]))))
  index.actor <- which(index > 0)
  profit.by.actor[[i]] <- profit[index.actor]
}
# compute mean profit ratio per director
movies.by.actor <- unlist(lapply(profit.by.actor, length))
profit.by.actor <- profit.by.actor[which(movies.by.actor >= 5)]
mean.profit <- sort(unlist(lapply(profit.by.actor, mean)), decreasing=TRUE)
# plot table of profit for top directors
top.rank.actor <- c()
for(i in 1:50)
{
  index <- unlist(lapply(actor.lineup, 
                      function(x) length(which(x == names(mean.profit)[i]))))
  index.actor <- which(index > 0)
  temp <- cbind(names(mean.profit)[i], imdb.info[index.actor, c(4, 24, 19)],
            movie.profit[index.actor, c(5, 7)])
  top.rank.actor <- rbind(top.rank.actor, temp)
}
#unique(top.rank.actor)
colnames(top.rank.actor) <- c('Actor', 'Title', 'IMDB rating', 'Released', 'Budget', 'Box Office')
setwd("~/Desktop/INSIGHT/shiny_app/html")
save(top.rank.actor, file='top_actors.Rdata')
PopTable <- gvisTable(as.data.frame(top.rank.actor), 
                      options=list(page='enable'))
plot(PopTable)

######################################################
# make image analysis of picture posters


######################################################
# performen sentiment analysis on tomato consensus review
review <- imdb.info$tomatoConsensus
profit.ratio <- 

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

