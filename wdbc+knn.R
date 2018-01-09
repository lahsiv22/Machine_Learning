wdbc <- read.csv(file= "G:/datasets/wdbc.data", sep = ",", header = FALSE)
wdbc
wdbc <- as.data.frame(wdbc[,-1])
summary(wdbc)
data_norm <- function(X){
  (X -min(X))/(max(X) - min(X))
}
wdbc_norm <- as.data.frame(lapply(wdbc[,-1], data_norm))
wdbc_norm
summary(wdbc_norm)
wdbc_tar <- wdbc[,1]

set.seed(1234)
ind <- sample(2, nrow(wdbc), replace = TRUE, prob = c(0.7, 0.3))
wdbc_xtrain <- wdbc_norm[ind == 1, ]
wdbc_xtest <- wdbc_norm[ind == 2, ]
wdbc_ytrain <- wdbc[ind == 1, 1]
wdbc_ytest <- wdbc[ind == 2, 1]


library(class)
wdbc_pred <- knn(wdbc_xtrain, wdbc_xtest, wdbc_ytrain, k = 12)
table(wdbc_pred, wdbc_ytest)



library(caret)
model <- train(wdbc_xtrain, wdbc_ytrain, method = 'knn', preProcess = c("center", "scale"))
pred <- predict.train(object = model, wdbc_xtest, type = 'raw')
table(pred)
confusionMatrix(pred, wdbc_ytest)

