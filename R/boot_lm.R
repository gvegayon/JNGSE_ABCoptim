rm(list=ls())

library(boot)

dataset <- read.delim("../data/TEGDataAlirezabahadori.csv", sep=",")
colnames(dataset) <- c("gasflowrate","temp","teg")

fun <- function(data, indices) {
	# Subsample
	data <- data[indices,]
	
	# New indices for pick
	N <- nrow(data)
	n <-0
	while(!n) {
		pick <- which(runif(N) > .5)
		# test data must have at least df
		# this is n + df
		n <- length(pick)
		n <- (n >= 3) & (n < N)
	}
	# Test data
	testdata   <- data[pick,]
	predictdata <- data[!(1:N %in% pick),]

	# Predicting model and measuring error
	fit <- lm(teg~temp+gasflowrate, data=testdata)
	newdata <- predict(fit, predictdata[,c("temp","gasflowrate")])

#print(cbind(newdata,predictdata$teg))

	return(mean(newdata-predictdata$teg))

}

bs <- boot(dataset, fun, R=500)
bs
summary(bs)

plot(bs)
