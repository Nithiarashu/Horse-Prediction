data <- read.csv("hourse.csv", stringsAsFactors = T)
str(data)


install.packages("pacman")
pacman::p_load("dummies","caret","mice","missMDA","OneR","ROSE","grid","libcoin","mvtnorm")

nearZeroVar(data, names = TRUE, saveMetrics = TRUE)
data$qualifier <- NULL
data$change_in_driver <- NULL
data$quarter_break <- NULL
data$half_break <- NULL
data$third_break <- NULL
data$stretch_break <-NULL
data$finish_break <-NULL
data$scratched <-NULL
data$disqualified <-NULL
data$win_dollars <- NULL
data$place_dollars <- NULL
data$post_break <- NULL
data$claim_price <- NULL

sapply(data,function(x) sum(is.na(x)))

#outliers
boxplot(subset(data, select=c(5,19:24)))



normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
#now let's normalise our dataset so that calculating the distances in the feature space makes sense
data_n <- subset(data, select=c(5,19:24)) #get the numerical for normalisation -- kNN also doesn't support levelled factors either
data_n <- as.data.frame(lapply(data_n, normalize)) #normalise
summary(data_n) 

data_c <- subset(data, select=c(1:4,6:18,25:28))
new_data <- cbind(data_c ,data_n)


str(new_data)

library(caret)
sample <- createDataPartition(new_data$win_roi, p = .75, list = FALSE) 
train <- new_data[sample, ]
test <- new_data[-sample, ]

library(partykit)
library(party)
library(party)
cForest <- cforest(win_roi ~., data=train, controls=cforest_unbiased(ntree=200, mtry=3))
cFp <- predict(cForest, newdata = test[, -2])
(cForestAcc <- 1- mean(cFp != test$Attrition))

#NB
library(OneR)
library(naivebayes)
library(e1071)
nb <- naiveBayes(win_roi ~., data=train)
nbP <- predict(nb, newdata=test[,-15], type = "class")
(nbAcc <- 1- mean(nbP != test$win_roi))


library(rpart)
library(rpart.plot)
library(RColorBrewer)
regressionTree <- rpart::rpart(win_roi ~ ., data=train, method="class")
library(rattle)
fancyRpartPlot(regressionTree)
rpartPrediction <- predict(regressionTree, testing, type = "class")
(CARTModelAccuracy <- mean(rpartPrediction == testing$Survived))
