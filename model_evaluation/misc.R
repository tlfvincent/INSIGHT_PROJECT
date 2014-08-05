'ExtractMovieInfo' <- function()
{
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

	# create matrix of movie genres
	movies.by.genre <- lapply(movie.metadata$genre, function(x) strsplit(as.vector(x), split='-', fixed=TRUE)[[1]])
	unique.genre <- unique(unlist(movies.by.genre))
	genre.matrix <- mat.or.vec(nrow(movie.metadata), length(unique.genre))
	colnames(genre.matrix) <- unique.genre
	for(i in 1:length(movies.by.genre))
	{
	  index <- match(movies.by.genre[[i]], unique.genre)
	  genre.matrix[i, index] <- 1
	}

  # concatenate movie metadata matrix and movie genre matrix
  dat.with.genre <- cbind(movie.metadata, as.data.frame(genre.matrix))
  colnames(dat.with.genre) <- gsub(' ', '_', colnames(dat.with.genre))
  return(dat.with.genre)
}

'GenerateCvFolds' <- function(K=10)
{
  folds <- split(sample(nrow(movie.metadata)),rep(1:K,length=nrow(movie.metadata))) # defne the fold ids
}
