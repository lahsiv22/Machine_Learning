url = "https://raw.githubusercontent.com/madmashup/targeted-marketing-predictive-engine/master/banking.csv"
dat <- read.csv(url, sep = ",", header = TRUE)
head(dat)
dat <- as.data.frame(dat)

unique(dat$education)
dat$education <- revalue(dat$education, c("basic.4y" = "Basic"))
dat$education <- revalue(dat$education, c("basic.6y" = "Basic"))
dat$education <- revalue(dat$education, c("basic.9y" = "Basic"))
unique(dat$education)


barplot(table(dat$y), col = c("red","green"))
aggregate(dat[,0:20], list(dat$y), mean, na.rm=TRUE)


ggplot(dat, aes(x = job))+
  geom_bar(aes(fill = (y==1)))
ggplot(dat, aes(x = marital))+
  geom_bar(aes(fill = (y==1)))
ggplot(dat, aes(x = education))+
  geom_bar(aes(fill = (y==1)))
ggplot(dat, aes(x = month))+
  geom_bar(aes(fill = (y==1)))
hist(dat$age)


split <- createDataPartition(dat$y, p= 0.7, list =FALSE)
dat.train <- dat[split, ]
dat.test <- dat[-split,]
model <- glm(y ~ ., data= dat.train, family = binomial)

pred <- predict(model, type = 'response')
table(dat.train$y, pred > 0.5)


ROCRpred <- prediction(pred, dat.train$y)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

