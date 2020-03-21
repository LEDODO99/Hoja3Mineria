install.packages("dplyr")
install.packages("moments")
install.packages("cluster")
install.packages("e1071")
install.packages("mclust")
install.packages("fpc")
install.packages("NbClust")
install.packages("factoextra")
install.packages("tidyverse")
install.packages("rpart")
install.packages("caret")
install.packages("tree")
install.packages("rpart.plot")
install.packages("randomForest")
install.packages("ggplot2")
install.packages("corrplot")


library('dplyr')
library('moments')
library('cluster')
library('e1071')
library('mclust') 
library('fpc')
library('NbClust')
library('factoextra')
library("tidyverse")
library("dplyr")
library('rpart')
library('caret')
library('tree')
library('rpart.plot')
library('randomForest')
library('ggplot2')
library('corrplot')



test <- read.csv("test.csv")
train <- read.csv("train.csv")
dataset <- read.csv("sample_submission.csv")

variance <- function(x) sum((x-mean(x))^2)/(length(x)-1)

#----Precio-------------------------------
hist(train$SalePrice, xlab = "Precio")

summary(train$SalePrice)


sd(train$SalePrice)

variance(train$SalePrice)

length(train$SalePrice)

quantile(train$SalePrice)

skewness(train$SalePrice)
kurtosis(train$SalePrice)

#------OverallQual--------------------------------

hist(train$OverallQual, xlab = "Calidad")

summary(train$OverallQual)


sd(train$OverallQual)

variance(train$OverallQual)

length(train$OverallQual)

quantile(train$OverallQual)

skewness(train$OverallQual)
kurtosis(train$OverallQual)

#--------Condicion--------------------------------------


hist(train$OverallCond, xlab = "Condicion")

summary(train$OverallCond)

sd(train$OverallCond)

variance(train$OverallCond)

length(train$OverallCond)

quantile(train$OverallCond)

skewness(train$OverallCond)
kurtosis(train$OverallCond)


#---------Area de Lote------------------------------------


hist(train$LotArea, xlab = "Lote")

summary(train$LotArea)

sd(train$LotArea)

variance(train$LotArea)

quantile(train$LotArea)

skewness(train$LotArea)
kurtosis(train$LotArea)

#----------------------------------------------


#Analisis de Grupos


#-----------------------------------------------

promedio <- mean(train$SalePrice)
promedio

caras <- promedio + (promedio*0.3)
caras

baratas <- promedio - (promedio*0.3)
baratas

#--------------5 y 6------------------

x <- train[c(5, 81)]
y <- train[c(18)]

#Haciendo el split
set.seed(123)

sample <- sample(1:nrow(train),0.75*nrow(train))
ttrain <- train[sample, ]
ttest <- train[-sample, ]

fitLMPW <- lm(SalePrice~LotArea, data = ttrain)

summary(fitLMPW)

ggplot(data = ttrain, mapping = aes(x = SalePrice, y = LotArea)) + 
  geom_point(color = "firebrick", size = 2) + 
  geom_smooth(moethod = "lm", se = TRUE, color = "black")+
  labs(title = "Precio ~ Area", x = "Precio", y = "Area")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))



predL <- predict(fitLMPW, newData = ttest)

head(predL)
length(predL)

head(fitLMPW$residuals)



plot(fitLMPW)



#--------------Multicolinealidad--------------------
plot(train[c(81,5)], xlab = "Precio", ylab = "Area")


matriz_cor <- cor(train[c(81, 5)])
matriz_cor
corrplot(matriz_cor)


hist(fitLMPW$residuals)
boxplot((fitLMPW$residuals))


#-------Analisis del algortimo-----------------

predMLM <- predict(fitLMPW, newdata = ttest[,c(81,5)])

rmseFunc <- function(error)
{
  sqrt((mean(error^2)))
}

rmseFunc(predMLM)


plot(ttest$SalePrice, col="blue")
points(predMLM, col = "red")


summary(ttest$SalePrice - predMLM)

#------Trabajo de model de regresion Lineal-----------
matriz_cor<-cor(ttrain[,c(4,5,18,19,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,62,67,68,69,70,71,72,81)])
round(matriz_cor,2)
corrplot(matriz_cor)

a<-lm(SalePrice~OverallQual+GrLivArea+X1stFlrSF, data=ttrain)
summary(a)

predic<-predict(a , newdata = ttest)

rmseFunc <- function(error)
{
  sqrt((mean(error^2)))
}

rmseFunc(predic)
plot(ttest$SalePrice, col="blue")
points(predic, col = "red")

summary(ttest$SalePrice-predic)
b=as.data.frame(predic)
