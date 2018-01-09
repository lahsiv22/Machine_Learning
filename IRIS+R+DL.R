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

data.target.train.hot <- to_categorical(data.target.train)
data.target.test.hot <- to_categorical(data.target.test)
print(data.target.test.hot)

kerasmodel <- keras_model_sequential()

kerasmodel %>%
  layer_dense(unit = 8, activation = 'relu', input_shape=c(4))%>%
  layer_dense(unit = 3, activation = 'softmax')

kerasmodel %>%
  compile(loss='categorical_crossentropy', optimizer = 'adam', metrics = 'accuracy')

kerasmodel %>%
  fit(data.train,data.target.train.hot, epochs = 200, batch_size = 5, validation_steps = 0.2)

history <- kerasmodel %>%
  fit(data.train,data.target.train.hot, epochs = 200, batch_size = 5, validation_steps = 0.2)
plot(history)

predicted <- kerasmodel %>%
  predict_classes(data.target.test, batch_size = 128)

table(data.target.test.hot,predicted)

score <- kerasmodel %>%
  evaluate(data.test, data.target.test.hot, batch_size = 128)
print(score)

sgd <- optimizer_sgd(lr = 0.01)
kerasmodel %>%
  compile(loss='categorical_crossentropy', optimizer = 'sgd', metrics = 'accuracy')
kerasmodel %>%
  fit(data.train,data.target.train.hot, epochs = 200, batch_size = 5, validation_steps = 0.2)
predicted <- kerasmodel %>%
  predict_classes(data.target.test, batch_size = 128)
table(data.target.test.hot,predicted)
score <- kerasmodel %>%
  evaluate(data.test, data.target.test.hot, batch_size = 128)
print(score)