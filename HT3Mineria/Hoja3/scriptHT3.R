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



#Haciendo el split
set.seed(123)

sample <- sample(1:nrow(train),0.75*nrow(train))
ttrain <- train[sample, ]
ttest <- train[-sample, ]



#------Trabajo de model de regresion Lineal-----------
matriz_cor<-cor(ttrain[,c(4,5,18,19,27,35,37,38,39,44,45,46,47,48,49,50,51,52,53,55,57,62,67,68,69,70,71,72,81)])
round(matriz_cor,2)
corrplot(matriz_cor)

a<-lm(SalePrice~OverallQual+GrLivArea+X1stFlrSF, data=ttrain)
summary(a)


#----Plots del lm
ggplot(data = ttrain, mapping = aes(x = SalePrice, y = OverallQual)) + 
  geom_point(color = "firebrick", size = 2) + 
  geom_smooth(moethod = "lm", se = TRUE, color = "black")+
  labs(title = "Precio ~ Calidad", x = "Precio", y = "Calidad")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = ttrain, mapping = aes(x = SalePrice, y = GrLivArea)) + 
  geom_point(color = "firebrick", size = 2) + 
  geom_smooth(moethod = "lm", se = TRUE, color = "black")+
  labs(title = "Precio ~ GrLivArea", x = "Precio", y = "GrLivArea")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = ttrain, mapping = aes(x = SalePrice, y = X1stFlrSF)) + 
  geom_point(color = "firebrick", size = 2) + 
  geom_smooth(moethod = "lm", se = TRUE, color = "black")+
  labs(title = "Precio ~ X1stFlrSF", x = "Precio", y = "X1stFlrSF")+
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#----Multicolinealidad
plot(train[c(81,18)], xlab = "Precio", ylab = "Calidad")
plot(train[c(81,47)], xlab = "Precio", ylab = "GrLivArea")
plot(train[c(81,44)], xlab = "Precio", ylab = "X1")

#---Matricces de correlacion
#Qual
matriz_corQ <- cor(train[c(81, 18)])
matriz_corQ
corrplot(matriz_corQ)

#Gr
matriz_corG <- cor(train[c(81, 47)])
matriz_corG
corrplot(matriz_corG)

#X1
matriz_corX <- cor(train[c(81, 44)])
matriz_corX
corrplot(matriz_corX)

#-----Histograma Residuos
hist(a$residuals)

plot(a$residuals)

boxplot(a$residuals)

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

plot(a$residuals)
