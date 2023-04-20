setwd("C:/Users/karan/OneDrive/Desktop/k-cw/Spring-23/Engr Data Analytics/Project-1")

#import dataset

train <- read.csv("CancerData.csv", stringsAsFactors = TRUE)
test <- read.csv("CancerHoldoutData.csv", stringsAsFactors = TRUE)

#Summary of data
summary(train)
summary(test)

#From summary it is evident that Median age contains erratic value and PctSomeCol18_24
#has significant NA's thus, taking overview of these two features and doing necessary
#imputations
train$Total <- rowSums(train[, c("PctSomeCol18_24",'PctNoHS18_24','PctHS18_24','PctBachDeg18_24')])

# From above, it is clear that sum of Pcts' columns where rows is non NA is nearly 100,
# thus, replacing NA in PctSomeCol18_24 by difference of 100 and rest of Pcts'
train$PctSomeCol18_24 <- 100-(train$PctNoHS18_24+train$PctHS18_24+train$PctBachDeg18_24)
test$PctSomeCol18_24 <- 100-(test$PctNoHS18_24+test$PctHS18_24+test$PctBachDeg18_24)
boxplot(train$MedianAge)
boxplot(test$MedianAge)

# There are some values in Median age which are above 300 which is not possible,
# thus, replacing and imputing those values.
train$MedianAge[train$MedianAge>200] <- NA
test$MedianAge[test$MedianAge>200] <- NA

#Visualize distribution before imputation
library(ggplot2)
ggplot(train, aes(MedianAge)) + 
  geom_histogram()  

# Performing Mean imputation

train$MedianAge[is.na(train$MedianAge)] <- mean(train$MedianAge[!is.na(train$MedianAge)])
train$MedianAge[is.na(test$MedianAge)] <- mean(test$MedianAge[!is.na(test$MedianAge)])
#Visualize distribution after imputation
ggplot(train, aes(MedianAge)) + 
  geom_histogram()

# After Imputation, no significant change observed in distribution
train.s <- subset(train, select = -c(Total, Geography))
summary(train.s)
test.s <- subset(test, select = -Geography)
#Correlation Matrix
library(mlbench)
library(caret)
library(ggcorrplot)
correlationMatrix <- cor(train.s[,2:21])
# summarize the correlation matrix
print(correlationMatrix)
# Plot matrix
ggcorrplot(correlationMatrix)
# find features that are highly correlated (>0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# Remove highly correlated:
train.f <- subset(train.s, select = -c(13,8,16,4,5))
test.f <- subset(test.s, select = -c(13,8,16,4,5))
findCorrelation(cor(train.f), cutoff=0.75)
# Remove highly correlated:
train.g <- subset(train.f, select = -c(4,6,12,11))
test.g <- subset(test.f, select = -c(4,6,12,11))
# Making Linear regression model:
lrm.g <- lm(TARGET_deathRate~. , data = train.g)
library(car)
library(mctest)
omcdiag(lrm.g)
# Removing PctSomeCol18_24:
train.h <- subset(train.g, select = -PctSomeCol18_24)
test.h <- subset(test.g, select = -PctSomeCol18_24)
lrm.h <- lm(TARGET_deathRate~. , data = train.h)
summary(lrm.h)
omcdiag(lrm.h)
#No feature has VIF greater than 5, going forward with this dataset:
ggcorrplot(cor(train.h))

# 2. Removing PctAsian:
train.i <- subset(train.h, select = -PctAsian)
test.i <- subset(test.h, select = -PctAsian)
summary(train.i)
summary(test.i)
lrm.i <- lm(TARGET_deathRate~. , data = train.i)
summary(lrm.i)

# Model Diagnosis:
plot(lrm.i)
par(mfrow=c(3,4))

for (feature in 1:length(train.i)){
  plot(train.i[[feature]],train.i$TARGET_deathRate, main = paste0(colnames(train.i[feature])))
}

#looking at plot, medIncome forms nonlinear relationship with Target_deathrate
# thus making it non-linear term and trying LR model
train.j <- train.i
train.j$medIncome <- train.j$medIncome[log(train.j$medIncome)]
lrm.j <- lm(TARGET_deathRate~.+incidenceRate:PctOtherRace , data = train.j)
summary(lrm.j)

#KNN Model:
train.x <- sample(1:nrow(train.i), size = round(0.7*nrow(train.i)), replace = FALSE)
test.x <- setdiff(1:nrow(train.i), train.x)

train.dx <- train.i[train.x, ]
test.dx <- train.i[test.x, ]

set.seed(100)
kv <- c(1:10)
Mean_se <- rep(0, 10)
for (i in kv) {
  knn.fit <- train(TARGET_deathRate ~ ., data = train.dx, method = "knn", 
                     tuneGrid = data.frame(k = kv[i]))
  knn.pd <- predict(knn.fit, newdata = test.dx)
  Mean_se[i] <- mean((knn.pd - test.dx$TARGET_deathRate)^2)
}

paste("k= ",c(kv),"Mean sqaured error is",Mean_se)
Min.k <- kv[which.min(Mean_se)]
Min.k
#important features identification
lrm.k <- lm(TARGET_deathRate~.+incidenceRate:PctOtherRace , data = train.dx)
summary(lrm.k)
#removing insignificant features
train.k <- subset(train.dx, select = -c(PctNoHS18_24,PctOtherRace,PctMarriedHouseholds))
test.k <- subset(test.dx, select = -c(PctNoHS18_24,PctOtherRace,PctMarriedHouseholds))
#knn model
kv2 <- c(1:10)
Mean_se2 <- rep(0, 10)
for (i in kv2) {
  knn.fit2 <- train(TARGET_deathRate ~ ., data = train.k, method = "knn", 
                   tuneGrid = data.frame(k = kv2[i]))
  knn.pd2 <- predict(knn.fit2, newdata = test.k)
  Mean_se2[i] <- mean((knn.pd2 - test.k$TARGET_deathRate)^2)
}

paste("k= ",c(kv2),"Mean sqaured error is",Mean_se2)
Min.k2 <- kv2[which.min(Mean_se2)]
Min.k2

#5: Hold out Data
hd  = lm(TARGET_deathRate ~., data= test.i)
summary(hd)

# Linear Regression:
test.y.pred <- predict(hd, data = test.i)
test.y.act <- test.i$TARGET_deathRate
Mean_setestlr <- mean((test.y.act - test.y.pred)^2)
Mean_setestlr

#KNN
set.seed(100)
train.ts <- sample(1:nrow(test.i), size = round(0.7*nrow(test.i)), replace = FALSE)
test.ts <- setdiff(1:nrow(test.i), train.ts)

train.dxs <- test.i[train.ts, ]
test.dxs <- test.i[test.ts, ]

#knn model
kv3 <- c(1:10)
Mean_se3 <- rep(0, 10)
for (i in kv3) {
  knn.fit3 <- train(TARGET_deathRate ~ ., data = train.dxs, method = "knn", 
                    tuneGrid = data.frame(k = kv3[i]))
  knn.pd3 <- predict(knn.fit3, newdata = test.dxs)
  Mean_se3[i] <- mean((knn.pd3 - test.dxs$TARGET_deathRate)^2)
}

paste("k= ",c(kv3),"Mean sqaured error is",Mean_se3)
Min.k3 <- kv3[which.min(Mean_se3)]
Min.k3

paste("k= ",c(kv3),"Mean sqaured error is",Mean_se3)

# Min k for test dataset
Min.k3 <- kv3[which.min(Mean_se3)]
Min.k3


