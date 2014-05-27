rm(list=ls())

library(boot)
library(nnet)
# library(monmlp)
# monmlp.fit(as.matrix(dataset_norm[,c("pressure","temp")]),as.matrix(dataset_norm$teg),hidden1=11)

# Loading data
source("../R/00_load_and_normalize.R")

fun <- function(data, indices, ...) {
	# Subsample
	data <- data[indices,]

	# New indices for pick
	N <- nrow(data)
	n <-0
	while(!n) {
		pick <- runif(N) > .5
		# test data must have at least df
		# this is n + df
		n <- sum(pick)
		n <- (n >= 3) & (n < N)
	}
	# Test data
	testdata   <- data[pick,]
	predictdata <- data[!pick,,drop=FALSE]

	# Predicting model and measuring error
	suppressWarnings({
	fit_lm <- lm(  teg~temp+pressure, data=testdata)
	fit_nn <- nnet(teg~temp+pressure, data=testdata, size=11) # Paper uses 11
	

	newdata_lm <- predict(fit_lm, predictdata[,c("temp","pressure")])
	newdata_nn <- predict(fit_nn, predictdata[,c("temp","pressure")])
	})

  nrep <- length(newdata_lm)
	return(c(
		lm         = mean((newdata_lm-predictdata$teg)/predictdata$teg),
		nn         = mean((newdata_nn-predictdata$teg)/predictdata$teg),
		predictsize= nrep,
		testsize   = N-nrep
		)
	)

}

bs <- boot(dataset_norm, fun, R=1000)
out <- as.data.frame(bs$t[,1:2])

colnames(out) <- c("Linear Model", "ANN 2-11-1")
boxplot(out, main="Average Prediction Error")


