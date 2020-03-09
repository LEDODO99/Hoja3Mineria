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

#--------------------------------------

hist(train$)

