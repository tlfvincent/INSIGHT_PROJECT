setwd('~/Desktop/MillionDollarStory/model_evaluation')
source('misc.R')

# read in word2vec clusters
setwd('/Users/ThomasVincent/Desktop/INSIGHT/word2vec')
word.vec <- scan(file='classes.sorted.txt', sep='\n', what='raw()')
word.clusters <- mat.or.vec(length(word.vec), 2)
for(w in 1:length(word.vec))
{
  temp <- strsplit(word.vec[w], split= ' ', fixed=TRUE)[[1]]
  word.clusters[w, ] <- temp
}
unique.clusters <- as.vector(unique(word.clusters[, 2]))

# extract TMDB movie metadata
setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/databases')
movie.metadata <- ExtractMovieInfo()

# extract genre information for each movie
setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/movie_scripts')
all.files <- list.files(pattern='.txt')
file.id <- as.numeric(sapply(all.files, function(x) strsplit(x, split='.', fixed=TRUE)[[1]][1]))
file.id <- sort(file.id)
movie.sentiment <- vector('list', length(file.id))
movie.genre <- lapply(movie.metadata$genre, function(x) strsplit(as.vector(x), split='-', fixed=TRUE)[[1]])
unique.genres <- unique(unlist(movie.genre))

# loop over movies and match sentiment to genre
setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/movie_scripts')
rollercoaster.genre <- mat.or.vec(length(unique.genres), length(unique.clusters))
row.names(rollercoaster.genre) <- unique.genres
for(i in 1:length(unique.genres))
{
  print(i)
  genre.row <- mat.or.vec(length(unique.clusters), 1)
  for(u in 1:length(unique.clusters))
  {
  	temp <- c()
  	target.words <- word.clusters[which(word.clusters[, 2]==(u-1)), 1]
	  for(j in 1:length(movie.genre))
	  {
	    index <- which(movie.genre[[j]] == unique.genres[i])
	    if(length(index)>0)
	    {
	      lines <- scan(file=paste(file.id[j], '.txt', sep=''), sep='\n', what='raw()')
	      words <- unlist(sapply(lines, function(x) strsplit(x, '\\s+', perl=TRUE)[[1]]))
	      words.freq <- table(words)
	      words.match <- match(names(words.freq), target.words)
	      #remove NAs
	      words.match <- words.match[!is.na(words.match)]
	      if(length(words.match) > 0)
	      {
	      	temp <- temp + length(words.match)
	    	}
	    }
	  }
	  if(length(temp)>0){ genre.row[u] <- temp }
	 }
  rollercoaster.genre[i, ] <- genre.row
}
