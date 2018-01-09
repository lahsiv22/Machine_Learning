wine <- read.csv("G:/wine.csv", header = FALSE, sep = ",")
wine <- as.data.frame.matrix(wine)
names(wine) = c("Origin", "Alcohol", "Malic-Acid", "Ash", "Ash-Alacanity", "Magnesium", "Phenols", "Flavanoids", "Non-Flavanoids", "Proanthocyanins", "Color-Intensity", "Hue", "OD280/OD315", "Proline")
print(wine)

#Using KNN
library(class)
set.seed(1234)
s <- sample(2, nrow(wine), replace = TRUE, prob = c(0.7,0.3))
wine.Ftrain <- wine[s==1, 2:13]
wine.Ftest <- wine[s==2, 2:13]
wine.Ttrain <- wine[s==1, 1]
wine.Ttest <- wine[s==2, 1]
wine_pred <- knn(train = wine.Ftrain, test = wine.Ftest, cl = wine.Ttrain, k = 3)
wine_pred
Wine.Ttest <- as.data.frame(wine.Ttest)
m <- data.frame(Wine.Ttest, wine_pred)
names(m) <- c("Observed", "Predicted")
print(m)
CrossTable(x= wine.Ttest, y = wine_pred, prop.chisq = FALSE)

#Using Caret
library(caret)
wine$Origin <- as.factor(wine$Origin)
split <- createDataPartition(wine$Origin, p=0.7, list = FALSE)
Wine.train <- wine[split, ]
Wine.test <- wine[-split, ]
mod <- train(Wine.train[,2:13], Wine.train[,1], method ='knn', preProcess = c("center", "scale"))
p <- predict(object = mod, Wine.test[,2:13], type = 'raw')
p<- round(p)
table(p)
confusionMatrix(p, Wine.test[,1])