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

muestra = sample.int(nrow(dataset),nrow(dataset)*.7)
test = setdiff(1:nrow(dataset),muestra)


#Nnet
fitNnet = nnet(teg~temp+pressure, data=dataset_norm[muestra,], size=3, linout = T,maxit = 1e5)
predict = predict(fitNnet,dataset_norm[test,])*sd(dataset$teg[muestra])+mean(dataset$teg[muestra])
sum( ( predict - dataset$teg[test] )^2)
plot( predict, dataset$teg[test])
cor( predict,dataset$teg[test])

dataset$fitNet = predict(fitNnet,dataset_norm)*sd(dataset$teg)+mean(dataset$teg)
fitLin = lm(teg ~ fitNet, dataset[muestra,])
sum((predict(fitLin,dataset[test,]) - dataset$teg[test])^2)
plot(predict(fitLin,dataset[test,]),dataset$teg[test])


#nls
indice = muestra
f = function(c){
  with(dataset[indice,], c[1] + c[2]*temp^c[3] + c[4]*pressure^c[5] + c[6]*( (pressure^c[7])*(temp^c[8] ) )^c[9]  )
}
fObj = function(c){
  sum( (f(c)-dataset$teg[indice])^2 )         
}
library(ABCoptim)
x0 = abc_optim(rnorm(9),fObj, ub=10,lb=-10,maxCycle = 5000)$par
fitAbc = optim(x0,fObj,method = "BFGS",control = list(maxit = 20000,fnscale = sd(dataset$teg), trace = T))
indice = test
fObj(fitAbc$par)
plot(f(fitAbc$par),dataset$teg[test])
cor(f(fitAbc$par),dataset$teg[test])

library(rgenoud)
indice = muestra
fitGenoud = genoud(fObj,9,solution.tolerance=0.00001, optim.method = "BFGS")
indice = test
fObj(fitGenoud$par)
plot(f(fitGenoud$par),dataset$teg[test])
cor(f(fitGenoud$par),dataset$teg[test])

# NOTAS
#se uso abcoptim para pto de partida
# se comparo abcoptim + optim con rgenoud
# se agrego una etapa mas al modelo nnet, en la que se vuelve a calibrar con una reg lineal, mejorando el resultado
# se midio la calidad en sum(R^2) y cor
# se obtuvo una mejor prediccion en la nnet de 3 perceptrones, seguramente con 11 estaba sobre calibrada
# falta hacer bootstrapping para validar