dataset <- read.delim("../data/TEGDataAlirezabahadori.csv", sep=",")
colnames(dataset) <- c("pressure", "temp", "teg")

nf <- function(x) {
	x <- x-min(x)
	x <- log(x/(max(x)-min(x)) + 1)
	return(.7*x/log(2) + .15)
}

# Normalizing only covariates
dataset_norm <- dataset
dataset_norm[,1:2] <- as.data.frame(lapply(dataset[,1:2], nf))
