install.packages("dplyr")
install.packages("moments")

library('dplyr')
library('moments')
test <- read.csv("test.csv")
train <- read.csv("train.csv")

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





























