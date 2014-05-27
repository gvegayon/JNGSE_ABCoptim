library(iplots)
library(nnet)

hacerCluster = function(n = nCPU,scripts = c("")){
  library("doSNOW")
  library("snow")
  library("foreach")
  cl = makeSOCKcluster(n,outfile="cl.txt")
  registerDoSNOW(cl)
  return(cl)
}

unNorm = function(x){
  with(dataset, x*sd(teg)+mean(teg))
}

dataset <- read.delim("./data/TEGDataAlirezabahadori.csv", sep=",")
colnames(dataset) <- c("pressure", "temp", "teg")

dataset = subset(dataset,teg > 0.97)
dataset_norm = data.frame(apply(dataset,2, scale))

# iplot(dataset$pressure,dataset$temp,"P","T")
# iplot(dataset$pressure,dataset$teg,"P","Y")
# iplot(dataset$temp,dataset$teg,"T","Y")
# summary(dataset)

cor(dataset$teg,dataset$temp)
cor(dataset$teg,dataset$pressure)

#Linear Model
fitLin = lm(teg ~ temp + pressure, dataset)
summary(fitLin)
sum((fitLin$fitted.values - dataset$teg)^2)
plot(fitLin$fitted.values,dataset$teg)

#Nnet
fitNnet = nnet(teg~temp+pressure, data=dataset_norm, size=c(11), linout = T,maxit = 1e5)
predict = fitNnet$fitted.values*sd(dataset$teg)+dataset$teg
sum( ( predict - dataset$teg )^2)
plot( predict, dataset$teg)
cor( predict,dataset$teg)

dataset$fitNet = predict
fitLin = lm(teg ~ fitNet, dataset)
sum((fitLin$fitted.values - dataset$teg)^2)
plot(fitLin$fitted.values,dataset$teg)


#nls
f = function(c){
  with(dataset, c[1] + c[2]*temp^c[3] + c[4]*pressure^c[5] + c[6]*( (pressure^c[7])*(temp^c[8] ) )^c[9]  )
}
fObj = function(c){
  sum( (f(c)-dataset$teg)^2 )         
}
library(ABCoptim)
x0 = abc_optim(rnorm(9),fObj, ub=10,lb=-10,maxCycle = 5000)$par
fitAbc = optim(x0,fObj,method = "BFGS",control = list(maxit = 20000,fnscale = sd(dataset$teg), trace = T))
fObj(fitAbc$par)
plot(f(fitAbc$par),dataset$teg)
cor(f(fitAbc$par),dataset$teg)

library(rgenoud)
fitGenoud = genoud(fObj,9,solution.tolerance=0.00001, optim.method = "BFGS")
fObj(fitGenoud$par)
plot(f(fitGenoud$par),dataset$teg)
cor(f(fitGenoud$par),dataset$teg)


