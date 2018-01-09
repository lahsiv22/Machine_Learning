data = read.csv(file="G:/iris.csv", sep= ",", header = FALSE)
names(data)= c("Slen", "Swid","Plen", "Pwid", "Class")
data
summary(data)

ggplot(data, aes(x=Plen, y=Pwid,col=Class))+
  geom_point()
ggplot(data, aes(x=Slen, y= Swid,col=Class))+
  geom_point()
ggplot(data, aes(x=Plen,fill=Class))+
  geom_histogram()
ggplot(data, aes(x=Slen,fill=Class))+
  geom_histogram()
ggplot(data, aes(x=Swid,fill=Class))+
  geom_histogram()
ggplot(data, aes(x=Pwid,fill=Class))+
  geom_histogram()


data[,5]
set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
data.train <- data[ind==1, 1:4]
data.test <- data[ind==2, 1:4]
data.target.train <- data[ind==1, 5]
data.target.test <- data[ind==2, 5]
data_pred <- knn(train = data.train, test = data.test, cl=data.target.train, k=3)
data_pred


data.Target.test <- data.frame(data.target.test)
merged <- data.frame(data.Target.test, data_pred)
names(merged) <- c("Observed", "Predicted")
print(merged)


ggplot(data = merged,aes(x= Observed, y = Predicted))+
  geom_line()
ggplot(data = merged,aes(x= Observed, y = Predicted))+
  geom_point()


library(caret)

split <- createDataPartition(data$Class, p= 0.7, list =FALSE)
Data.train <- data[split,]
Data.test <- data[-split,]
model <- train(Data.train[,1:4],Data.train[,5], method='knn', preProcess = c("center", "scale"))
pred <- predict.train(object = model, Data.test[,1:4], type = "raw")
table(pred)
confusionMatrix(pred, Data.test[,5])


