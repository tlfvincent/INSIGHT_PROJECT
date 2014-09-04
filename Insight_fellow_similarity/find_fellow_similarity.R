# load required libraries
library(pheatmap)
library(e1071)

# read in demo schedule for fellows
demos <- read.csv(file='NYC_Fellows_Demos.csv', header=TRUE)

# find all fellows at Insight
all.fellows <- unique(sort(apply(demos, 2, function(x) as.vector(x))))
all.fellows <- all.fellows[-1] # remove whitespace

# Convert to matrix similarity format
demos.mat <- mat.or.vec(length(all.fellows), ncol(demos))
row.names(demos.mat) <- all.fellows
colnames(demos.mat) <- colnames(demos)
for(i in 1:ncol(demos))
{
	fellow.in.demos <- match(as.vector(demos[, i]), all.fellows)
	fellow.in.demos <- fellow.in.demos[!is.na(fellow.in.demos)] # remove NAs
	demos.mat[fellow.in.demos, i] <- 1 # populate matrix
}

# plot fellow demo similarity matrix as heatmap
png(file='fellows_group_by_demos.png', height=630, width=670)
pheatmap(demos.mat, 
        col =c('white', 'red'),
        fontsize_row=15,
        fontsize_col=15)
dev.off()

# find most similar fellows
hamming.fellows <- hamming.distance(demos.mat)
hamming.fellows <- 1 - (hamming.fellows / max(hamming.fellows))
png(file='fellow_similarity_index.png', height=630, width=670)
pheatmap(hamming.fellows,
        fontsize_row=15,
        fontsize_col=15)
dev.off()

