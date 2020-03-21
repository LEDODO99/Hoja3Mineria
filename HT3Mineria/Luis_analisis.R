library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(NbClust) #Para determinar el n?mero de clusters ?ptimo
library(factoextra) #Para hacer gr?ficos bonitos de clustering


a=read.csv("sample_submission.csv")
testData=read.csv("test.csv")
trainData=read.csv("train.csv")
trainData["PoolQC"]

totalData <- trainData
