# read in movie scripts and create cluster matrix
setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/movie_scripts')
all.files <- list.files(pattern='.txt')
file.id <- as.numeric(sapply(all.files, function(x) strsplit(x, split='.', fixed=TRUE)[[1]][1]))
file.id <- sort(file.id)

size <- seq(100, 500, 100)
for(s in 1:length(size))
{
  # read in word2vec clusters
  setwd('/Users/ThomasVincent/Desktop/INSIGHT/word2vec')
  word.vec <- scan(file=sprintf('classes_sorted_%s.txt', size[s]), sep='\n', what='raw()')
  word.clusters <- mat.or.vec(length(word.vec), 2)
  for(w in 1:length(word.vec))
  {
    temp <- strsplit(word.vec[w], split= ' ', fixed=TRUE)[[1]]
    word.clusters[w, ] <- temp
  }
  word2vec.clusters <- word.clusters
  save(word2vec.clusters, file=sprintf('word2vec_clusters_%s.Rdata', size[s]))

  # count number of clusters
  nb.clusters <- unique(word.clusters[,2])

  setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/movie_scripts')
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

  setwd('/Users/ThomasVincent/Desktop/INSIGHT/word2vec')
  save(word2vec.mat, file=sprintf('word2vec_mat_%s.Rdata', size[s]))
}


