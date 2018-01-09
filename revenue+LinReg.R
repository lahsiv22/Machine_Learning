#Loading the data
file = "g:/datasets/customerdata.csv"
cus <- read.csv(file, sep = ",",header = TRUE)
head(cus)

#Removing irrelevant columns i.e, the Customer ID
cus <- cus[,-1]
head(cus)


#Converting the city values into factors
cus$City <- as.factor(as.character(cus$City))


#Splitting the data into training set and test set(70:30 ratio)
n <- sample(2, nrow(cus), replace = TRUE, prob = c(0.7,0.3))
cus.train <- cus[n==1, ]
cus.test <- cus[n==2, ]


ggplot(cus, aes(x = City, y = TotalRevenueGenerated))+
  geom_bar(stat = 'identity')

ggplot(cus, aes(x = NoOfChildren,y = TotalRevenueGenerated))+
  geom_bar(stat = 'identity',aes(fill = City))

ggplot(cus, aes(x = Tenure, y = TotalRevenueGenerated))+
  geom_point(size = 1, alpha = 0.3)+
  geom_smooth()

ggplot(cus, aes(x =FrquncyOfPurchase, y = TotalRevenueGenerated))+
  geom_point(size = 1, alpha = 0.3)+
  geom_smooth()

ggplot(cus, aes(x = NoOfUnitsPurchased, y = TotalRevenueGenerated))+
         geom_point(size = 1, alpha = 0.3)+
         geom_smooth()

ggplot(cus, aes(x = FrequencyOFPlay, y = TotalRevenueGenerated))+
  geom_point(size = 1, alpha=0.3)+
  geom_smooth()


ggplot(cus, aes(x = NoOfGamesPlayed, y = TotalRevenueGenerated))+
  geom_point(size = 1, alpha = 0.3)+
  geom_smooth()


ggplot(cus, aes(x = NoOfGamesBought, y = TotalRevenueGenerated))+
  geom_point(size = 1, alpha = 0.3)+
  geom_smooth()

ggplot(cus, aes(x = FavoriteChannelOfTransaction, y = TotalRevenueGenerated))+
  geom_bar(stat = 'identity')

ggplot(cus, aes(x =FavoriteGame, y = TotalRevenueGenerated))+
  geom_bar(stat = 'identity')


ggplot(cus, aes(x =MinAgeOfChild, y = TotalRevenueGenerated))+
  geom_point(size = 1, alpha = 0.3)+
  geom_smooth()

ggplot(cus, aes(x =MaxAgeOfChild, y = TotalRevenueGenerated))+
  geom_point(size = 1, alpha = 0.3)+
  geom_smooth()

LR=lm(TotalRevenueGenerated~., data=cus)
summary(LR)

#from the visualizations we can infer that MinAgeOfChild, MaxAgeOfChild and Tenure are not important variables
LR1 = lm(TotalRevenueGenerated~City+NoOfChildren+FrquncyOfPurchase+NoOfUnitsPurchased+FrequencyOFPlay+NoOfGamesPlayed+NoOfGamesBought+FavoriteChannelOfTransaction+FavoriteGame, data = cus)
summary(LR1)

library(DMwR)
regr.eval(cus.train$TotalRevenueGenerated, LR1$fitted.values)
p = predict(LR1, cus.test)
regr.eval(cus.test$TotalRevenueGenerated, p)

LR2 = lm(TotalRevenueGenerated~City+
           FrquncyOfPurchase+NoOfUnitsPurchased+FrequencyOFPlay+
           NoOfGamesBought+FavoriteChannelOfTransaction+FavoriteGame ,data = cus)
summary(LR2)
regr.eval(cus.train$TotalRevenueGenerated, LR2$fitted.values)
p2 = predict(LR2, cus.test)
regr.eval(cus.test$TotalRevenueGenerated, p2)

library(MASS)
step <- stepAIC(LR2, direction="both")