
# read in word2vec clusters
setwd('/Users/ThomasVincent/Desktop/INSIGHT/word2vec')
word.vec <- scan(file='classes.sorted.txt', sep='\n', what='raw()')
word.clusters <- mat.or.vec(length(word.vec), 2)
for(w in 1:length(word.vec))
{
  temp <- strsplit(word.vec[w], split= ' ', fixed=TRUE)[[1]]
  word.clusters[w, ] <- temp
}
setwd('/Users/ThomasVincent/Desktop/INSIGHT/shiny_app/data')
word2vec.clusters <- word.clusters
save(word2vec.clusters, file='word2vec_clusters.Rdata')

# count number of clusters
nb.clusters <- unique(word.clusters[,2])

# read in movie scripts and create cluster matrix
setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/movie_scripts')
all.files <- list.files(pattern='.txt')
file.id <- as.numeric(sapply(all.files, function(x) strsplit(x, split='.', fixed=TRUE)[[1]][1]))
file.id <- sort(file.id)
word2vec.mat <- mat.or.vec(length(file.id), length(nb.clusters))
for(f in 1:length(file.id))
{
  print(f)
	lines <- scan(file=paste(file.id[f], '.txt', sep=''), sep='\n', what='raw()')
	words <- unlist(sapply(lines, function(x) strsplit(x, '\\s+', perl=TRUE)[[1]]))
	words.freq <- table(words)
  words.match <- match(names(words.freq), word.clusters[, 1])
  #remove NAs
  index.na <- which(is.na(words.match=='TRUE'))
  if(length(index.na) > 0)
  {
    words.match <- words.match[-index.na]
    words.freq <- words.freq[-index.na]
  }
  # populate matrix
	for(w in 1:length(words.match))
	{
    index.col <- as.numeric(word.clusters[words.match[w], 2]) + 1
    word2vec.mat[f, index.col] <- word2vec.mat[f, index.col] + as.vector(words.freq[w])
	}
}
save(word2vec.mat, file='word2vec_mat.Rdata')