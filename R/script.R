#### packages ####
rm(list=ls())
library(ggplot2)
library(nnet)
library(randomForest)
library(gbm)
library(plyr)
library(Metrics)

#### load data & eda ###
data <- read.csv("data/TEGDataAlirezabahadori.csv")
head(data)
names(data) <- c("pressure", "temp", "teg")
str(data)

ggplot(data, aes(pressure, teg)) + geom_point() + geom_smooth()
ggplot(data, aes(temp, teg)) + geom_point() + geom_smooth()
ggplot(data, aes(temp, pressure)) + geom_point() + geom_density2d() 
cor(data)

# dato 'anómalo'
data <- data[data$teg > .96, ]
ggplot(data, aes(pressure, teg)) + geom_point() + geom_smooth()
ggplot(data, aes(temp, teg)) + geom_point() + geom_smooth()
ggplot(data, aes(temp, pressure)) + geom_point() + geom_density2d() 
cor(data)


#### models ###
# data$teg <- data$teg * 100 # Quería asegurarme de que los modelos no predigan probs.
formula <- formula(teg ~ pressure + temp)

mod.lm <- lm(formula, data)
data$pred.lm <- predict(mod.lm)

mod.gbm <- gbm(formula, data=data, distribution="gaussian", n.trees=500)
data$pred.gbm <- predict(mod.gbm, n.trees=500)

mod.rf <- randomForest(formula, data, ntree=500)
data$pred.rf <- predict(mod.rf)



#### Validation ####
head(data)
preds <- names(data)[grepl("^pred", names(data))]

results <- ldply(preds, function(pred){
  # pred <- preds[1]  
  data.frame(model = gsub("pred\\.", "", pred), MSE = mse(data[[pred]],data$teg))
})
results <- results[order(results$MSE), ]
results


