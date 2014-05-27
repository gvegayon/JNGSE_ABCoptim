#### packages ####
rm(list=ls())
library(ggplot2)
library(nnet)
library(randomForest)
library(plyr)
library(Metrics)
library(reshape2)
library(kknn)


#### load data & eda ####
data <- read.csv("data/TEGDataAlirezabahadori.csv")
head(data)
names(data) <- c("pressure", "temp", "teg")
str(data)

summary(data)

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
head(data)


#### models ####
# data$teg <- data$teg * 50 # Quería asegurarme de que los modelos no predigan probs.
fml <- formula(teg ~ pressure + temp)
niter <- 1000
prop <- .75
nobs <- nrow(data)

k.neighbor.size <- c(1,5,10,15)
ntree.rf.size <- c(100,250,500)
size.nn <- c(5,10,15,20)

results <- ldply(seq(niter), function(x){
  idx <- sample(nobs)[seq(nobs*prop)]
  d.train <- data[idx,]
  d.test <- data[-idx,]
    
  lm.mod <- lm(fml, d.train)
  lm.prd <- predict(lm.mod, newdata=d.test)
  lm.mse <- mse(d.test$teg, lm.prd)
  
  rfs.mse <- laply(ntree.rf.size , function(nt){
    rf.mod <- randomForest(fml, d.train, ntree=nt)
    rf.prd <- predict(rf.mod, newdata=d.test)
    rf.mse <- mse(d.test$teg, rf.prd)
  })
  rfs.mse <- t(setNames(rfs.mse, paste0("rf.mse", ntree.rf.size)))
  
  knns.mse <- laply(k.neighbor.size , function(k){
    knn.mod <- kknn(fml, train=d.train, k=k, test=d.test)
    knn.prd <- predict(knn.mod, newdata=d.test)
    knn.mse <- mse(d.test$teg, knn.prd)
    knn.mse
  })
  knns.mse <- t(setNames(knns.mse, paste0("knn.mse", k.neighbor.size)))
  
  nns.mse <- laply(size.nn , function(s){
    nn.mod <- nnet(fml, data=d.train, size=s, trace=FALSE, linout=TRUE)
    nn.prd <- predict(nn.mod, newdata=d.test)
    nn.mse <- mse(d.test$teg, nn.prd)
    nn.mse
  })
  nns.mse <- t(setNames(nns.mse, paste0("nn.mse", size.nn)))
  
  data.frame(niter = x,
             lm.mse,
             rfs.mse,
             nns.mse,
             knns.mse)
  
}, .progress="text")

head(results)
results <- data.frame(lapply(results, function(x) { cumsum(x)/seq(length(x))  }))
results$niter <- seq(nrow(results))
head(results)
results.final <- data.frame(mse = t(sort(tail(results, 1)[,-1])))
results.final

results2 <- melt(results, id.vars="niter")
head(results2)


p <- ggplot(results2) + geom_line(aes(niter, value, group=variable, color=variable), size = 2.9) +
  scale_color_manual(values=c("#7CB5EC", "#313131", "#F7A35C", "#90EE7E", "#7798BF", "#AAEEEE",
                              "#FF0066", "#EEAAEE", "#55BF3B", "#DF5353", "#7798BF", "#AAEEEE")
  ) + 
  scale_y_log10() + 
  theme(
    text                = element_text(size = 10),
    title               = element_text(hjust=0), 
    axis.title.x        = element_text(hjust=.5),
    axis.title.y        = element_text(hjust=.5),
    panel.grid.major.y  = element_line(color='gray', size = .3),
    panel.grid.minor.y  = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank(),
    panel.border        = element_blank(),
    panel.background    = element_blank(),
    legend.position     = "bottom",
    legend.title        = element_blank()
  )

p

