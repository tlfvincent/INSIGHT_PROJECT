'ExtractMovieReadability' <- function()
{
  # read in readability data
  #setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/databases')
  drv = dbDriver('SQLite')
  con = dbConnect(drv, db='movie_screenplay_readability.db')
  dbListTables(con) # show tables
  df = dbGetQuery(con,statement='SELECT * FROM MOVIE_READABILITY')
  # do stuff
  dbDisconnect(con)
  return(df)
}

'ExtractMovieSentimentMatrix' <- function()
{
  # read in sentiment word matrix
  sentiment.dat <- read.csv(file='sentiment_movie_matrix.tab', header=TRUE)
  # remove words with zero occurence
  null.col <- which(apply(sentiment.dat, 2, sum) <= 10)
  sentiment.dat <- sentiment.dat[, -null.col]
  return(sentiment.dat)
}

'ExtractMoviePolarity' <- function()
{
  # run sentiment140 API
  setwd('/Users/ThomasVincent/Desktop/INSIGHT/PROJECT/movie_scripts')
  all.files <- list.files(path='.', pattern='.txt')
  file.id <- sapply(all.files, function(x) strsplit(x, split='.', fixed=TRUE)[[1]][1])
  file.id <- sort(as.numeric(file.id))
  polarity.dict <- vector('list', length(file.id))
  for(f in 1:length(file.id))
  {
    input.file <- sprintf('%s.txt', file.id[f])
    if(file.info(input.file)$size > 0)
    {
      stmt <- sprintf('curl --data-binary @%s "http://www.sentiment140.com/api/bulkClassify?query=movie" >sentiment.csv', input.file)  
      system(stmt)
      sentiment <- read.csv(file='sentiment.csv', header=FALSE)
      sentiment <- sentiment[, 1]-2
      polarity.dict[[f]] <- sentiment
    }
    else
    {
      polarity.dict[[f]] <- 0
    }
    unlink('sentiment.csv', recursive = FALSE)
  }
  save(polarity.dict, file='polarity_dict.Rdata')
}

#sentiment <- polarity.dict[[2]]
 #   window <- c()
  #  for(i in 1:(length(sentiment)-5))
   # {
    #  window <- c(window, sum(sentiment[i:(i+5)]))
    #}

'DefineGDP' <- function()
{
  gdp <- c("2.3","6.09999999999999","4.40000000000001","5.80000000000001","6.40000000000001","6.5","2.49999999999999","4.80000000000001","3.09999999999999","3.20680725745434","3.29185832079213","5.24933305971678","5.64264545312747","-0.51677678933973","-0.198508404140924","5.38711776187392","4.609034625086","5.56098877010744","3.17551594147345","-0.244612335892995","2.5949033119161","-1.91056787583574","4.63272828215837","7.25897976329091","4.23926785027209","3.51187102047281","3.46149437730023","4.20364603207824","3.68086564131647","1.9186510197105","-0.0726630446933285","3.55524728993501","2.74503872870457","4.03646380956395","2.71862714382442","3.79586174326279","4.48741646523554","4.44982717796543","4.84652665589711","4.09066056961085","0.948651832043794","1.77619753397849","2.79092465355562","3.7980406932931","3.35124146943487","2.66655427238724","1.78991734632432","-0.290385029038262","-2.80242152950024","2.5072999535297","1.84716564948495","2.77895883712883","1.87645032222238", "2.1")
  names(gdp) <- 1961:2014
  return(gdp)

}

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

# plot a radial plot showing the user-defined input
# for whiskey flavors

radial_fun <- function(headers, cor.value)
{
  #body, sweetness, smoky, medicinal, tobacco, honey, spicy, winey, nutty, malty, fruity, floral
  headers <- headers
  cor.max <- rep(0.3, length(cor.value))
  cor.min <- rep(-0.3, length(cor.value))
  dat <- rbind(cor.max, cor.min, cor.value)
  colnames(dat) <- headers
  
  par(mai=c(1,0.1,0.5,0.1))
  radarchart(as.data.frame(dat), 
            axistype=7, 
            plty=1, 
            plwd=3.5, 
            pcol="red", 
            seg=6, 
            cex=1, paxislabels=seq(-0.3, 0.3, 0.1))
}

'radarchart' <- function (df, axistype = 0, seg = 4, pty = 16, pcol = 1:8, plty = 1:6,
plwd = 1, pdensity = NULL, pfcol = NA, cglty = 3, cglwd = 1,
cglcol = "navy", axislabcol = "blue", title = "", maxmin = TRUE,
na.itp = TRUE, centerzero = FALSE, vlabels = NULL, caxislabels = NULL,
paxislabels = NULL, cex=3, ...)
{
  if (!is.data.frame(df)) {
    cat("The data must be given as dataframe.\n")
    return()
  }
  if ((n <- length(df)) < 3) {
    cat("The number of variables must be 3 or more.\n")
    return()
  }
  if (maxmin == FALSE) {
    dfmax <- apply(df, 2, max)
    dfmin <- apply(df, 2, min)
    df <- rbind(dfmax, dfmin, df)
  }
  plot(c(-1.2, 1.2), c(-1.2, 1.2), type = "n", frame.plot = FALSE,
  axes = FALSE, xlab = "", ylab = "", main = title, cex.main=3, asp = 1,
  ...)
  theta <- seq(90, 450, length = n + 1) * pi/180
  theta <- theta[1:n]
  xx <- cos(theta)
  yy <- sin(theta)
  CGap <- ifelse(centerzero, 0, 1)
  for (i in 0:seg) {
    polygon(xx * (i + CGap)/(seg + CGap), yy * (i + CGap)/(seg +
    CGap), lty = cglty, lwd = cglwd, border = cglcol)
    if (axistype == 1 | axistype == 3)
    CAXISLABELS <- paste(i/seg * 100, "(%)")
    if (axistype == 4 | axistype == 5)
    CAXISLABELS <- sprintf("%3.2f", i/seg)
    if (!is.null(caxislabels) & (i < length(caxislabels)))
    CAXISLABELS <- caxislabels[i + 1]
    if (axistype == 1 | axistype == 3 | axistype == 4 | axistype ==
    5)
    text(-0.05, (i + CGap)/(seg + CGap), CAXISLABELS,
    col = axislabcol)
  }
  if (centerzero) {
    arrows(0, 0, xx * 1, yy * 1, lwd = cglwd, lty = cglty,
    length = 0, col = cglcol)
  }
  else {
    arrows(xx/(seg + CGap), yy/(seg + CGap), xx * 1, yy *
    1, lwd = cglwd, lty = cglty, length = 0, col = cglcol)
  }
  PAXISLABELS <- df[1, 1:n]
  if (!is.null(paxislabels))
  PAXISLABELS <- paxislabels
  if (axistype == 2 | axistype == 3 | axistype == 5) {
    text(xx[1:n], yy[1:n], PAXISLABELS, col = axislabcol)
  }
  VLABELS <- colnames(df)
  if (!is.null(vlabels))
  VLABELS <- vlabels
  text(xx * 1.2, yy * 1.2, VLABELS, cex=1)
  series <- length(df[[1]])
  if (length(pty) < (series - 2)) {
    ptys <- rep(pty, series - 2)
    pcols <- rep(pcol, series - 2)
    pltys <- rep(plty, series - 2)
    plwds <- rep(plwd, series - 2)
    pdensities <- rep(pdensity, series - 2)
    pfcols <- rep(pfcol, series - 2)
  }
  else {
    ptys <- pty
    pcols <- pcol
    pltys <- plty
    plwds <- plwd
    pdensities <- pdensity
    pfcols <- pfcol
  }
  for (i in 3:series) {
    xxs <- xx
    yys <- yy
    scale <- CGap/(seg + CGap) + (df[i, ] - df[2, ])/(df[1,
    ] - df[2, ]) * seg/(seg + CGap)
    if (sum(!is.na(df[i, ])) < 3) {
      cat(sprintf("[DATA NOT ENOUGH] at %d\n%g\n", i, df[i,
      ]))
    }
    else {
      for (j in 1:n) {
        if (is.na(df[i, j])) {
          if (na.itp) {
            left <- ifelse(j > 1, j - 1, n)
            while (is.na(df[i, left])) {
              left <- ifelse(left > 1, left - 1, n)
            }
            right <- ifelse(j < n, j + 1, 1)
            while (is.na(df[i, right])) {
              right <- ifelse(right < n, right + 1, 1)
            }
            xxleft <- xx[left] * CGap/(seg + CGap) +
            xx[left] * (df[i, left] - df[2, left])/(df[1,
            left] - df[2, left]) * seg/(seg + CGap)
            yyleft <- yy[left] * CGap/(seg + CGap) +
            yy[left] * (df[i, left] - df[2, left])/(df[1,
            left] - df[2, left]) * seg/(seg + CGap)
            xxright <- xx[right] * CGap/(seg + CGap) +
            xx[right] * (df[i, right] - df[2, right])/(df[1,
            right] - df[2, right]) * seg/(seg + CGap)
            yyright <- yy[right] * CGap/(seg + CGap) +
            yy[right] * (df[i, right] - df[2, right])/(df[1,
            right] - df[2, right]) * seg/(seg + CGap)
            if (xxleft > xxright) {
              xxtmp <- xxleft
              yytmp <- yyleft
              xxleft <- xxright
              yyleft <- yyright
              xxright <- xxtmp
              yyright <- yytmp
            }
            xxs[j] <- xx[j] * (yyleft * xxright - yyright *
            xxleft)/(yy[j] * (xxright - xxleft) - xx[j] *
            (yyright - yyleft))
            yys[j] <- (yy[j]/xx[j]) * xxs[j]
          }
          else {
            xxs[j] <- 0
            yys[j] <- 0
          }
        }
        else {
          xxs[j] <- xx[j] * CGap/(seg + CGap) + xx[j] *
          (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) *
          seg/(seg + CGap)
          yys[j] <- yy[j] * CGap/(seg + CGap) + yy[j] *
          (df[i, j] - df[2, j])/(df[1, j] - df[2, j]) *
          seg/(seg + CGap)
        }
      }
      polygon(xxs, yys, lty = pltys[i - 2], lwd = plwds[i -
      2], border = pcols[i - 2], density = pdensities[i -
      2], col = pfcols[i - 2])
      points(xx * scale, yy * scale, pch = ptys[i - 2],
      col = pcols[i - 2])
    }
  }
}
