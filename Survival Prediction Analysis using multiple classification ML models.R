setwd("C:/Users/karan/OneDrive/Desktop/k-cw/Spring-23/Engr Data Analytics/Assignment-6")

#Import test and train
train.org <- read.csv("ProjectTrain.csv", stringsAsFactors = TRUE)
test.org <- read.csv("ProjectTest.csv", stringsAsFactors = TRUE)

#make a selection
train.org <- subset(train.org, select=c(Pclass,Sex,Age,SibSp,Parch,Embarked,Survived))
test.org <- subset(test.org, select=c(Pclass,Sex,Age,SibSp,Parch,Embarked,Survived))

#Transform to vectors
train.org <- sapply(train.org, unclass)
test.org <- sapply(test.org, unclass)

#overview about missing values
library(naniar)
# No. of missing values
n_miss(train.org)
n_miss(test.org)
# Which variable
train.org %>% is.na() %>% colSums()

#imputation using mice
library(mice)
pred_mattr <- quickpred(train.org, mincor = 0.3)
pred_matts <- quickpred(test.org, mincor = 0.3)
#mice imputes
train.org_imp <- mice(train.org, m=10, method = 'pmm', seed = 10, 
                      predictorMatrix = pred_mattr)

test.org_imp <- mice(test.org, m=10, method = 'pmm', seed = 10, 
                      predictorMatrix = pred_matts)
# create imputed dataset
train.imp <- mice::complete(train.org_imp,1)
test.imp <- mice::complete(test.org_imp,1)
is.na(train.imp)
attach(train.imp)
attach(test.imp)

# define response variable testing:
survived.test <- Survived[unlist(test.imp)]

#Step 2: Model training
library(MASS)
#Logistic model
logistic.fit=glm(Survived~., data=train.imp, family=binomial)
summary(logistic.fit)
#LDA model
lda.fit=lda(Survived~., data=train.imp)
lda.fit
#QDA Model
qda.fit = qda(Survived~., data=train.imp)
qda.fit
#KNN MOdel
library(class)
# Feature Scaling
#train_scale <- scale(train.imp[, 1:4])
#test_scale <- scale(test.imp[, 1:4])
# Model Evaluation - Choosing K
# Calculate out of Sample error
knn_error <- numeric(10)
knn_err <- NULL
k <- numeric(10)
min_err <- 1
min_k <- NULL
for (i in 1:10)
{
  knn.fit <- knn(train.imp, test.imp, train.imp$Survived, k=i)
  knn_err <- mean(knn.fit!=test.imp$Survived)
  knn_error[i] <- mean(knn.fit!=test.imp$Survived)
  k[i] <- i
    if (knn_err < min_err) {
      min_err <- knn_err
      min_k <- i
  }
}
paste("For k=",c(k),"Error is",round(c(knn_error),digits = 3))
paste("The minimum knn_error is", min_err,"and it occurs at k =", min_k)
knn.fit = knn(train.imp, test.imp, train.imp$Survived, k=min_k)

#Predictions of models:
## For Logistic Regression - 
logistic.probs=predict(logistic.fit,test.imp,type="response")

logistic.pred=rep("0", length(test.imp$Age))
logistic.pred
logistic.pred[logistic.probs>0.5] = "1"

mean(logistic.pred==test.imp$Survived)

#  For LDA -
lda.pr=predict(lda.fit,test.imp)
lda.pr
lda.pred = predict(lda.fit,test.imp)$class
lda.pred
# For QDA - 
qda.pr=predict(qda.fit,test.imp)
qda.pred = predict(qda.fit, test.imp)$class

#Confusion Matrices-
table(logistic.pred,test.imp$Survived)
table(lda.pred, test.imp$Survived)
table(qda.pred, test.imp$Survived)
table(knn.fit, test.imp$Survived)

library(ROCR)
library(pROC)
# Function to calculate performance metrics
p_metric <- function(actual, predicted) {
  tp <- sum(predicted == 1 & actual == 1)
  tn <- sum(predicted == 0 & actual == 0)
  fn <- sum(predicted == 0 & actual == 1)
  fp <- sum(predicted == 1 & actual == 0)
  sensitivity <- tp / (tp + fn)
  specificity <- tn / (tn + fp)
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  error.rate <- 1 - accuracy
  return(c(tp,fp,sensitivity, specificity, accuracy, error.rate))
}

glm.met <- round(p_metric(test.imp$Survived, logistic.pred),4)
lda.met <- round(p_metric(test.imp$Survived, lda.pred),4)
qda.met <- round(p_metric(test.imp$Survived, qda.pred),4)
knn.met <- round(p_metric(test.imp$Survived, knn.fit),4)

# Combine metrics into a table
table <- rbind(
  c("Logistic Regression", glm.met),
  c("LDA", lda.met),
  c("QDA", qda.met),
  c("KNN", knn.met)
)
colnames(table) <- c("Model","tp","fp","Sensitivity (%)", "Specificity (%)", "Accuracy (%)", "Error Rate (%)")
print(table)
      
# 2 -  evaluate performance improvement with and without including the cabin feature using test data

#Import test and train
train <- read.csv("ProjectTrain.csv", stringsAsFactors = TRUE)
test <- read.csv("ProjectTest.csv", stringsAsFactors = TRUE)
train.cab <- subset(train, select=c(Pclass,Sex,Age,Cabin,SibSp,Parch,Embarked,Survived))
train.cab
test.cab <- subset(test, select=c(Pclass,Sex,Age,Cabin,SibSp,Parch,Embarked,Survived)) 
#Making levels of teat and train same (for prediction)
levels(test.cab$Cabin) <- levels(train.cab$Cabin)
#Categorizing Cabin into broad classes by removing numbers and keeping alphabets
train.cab$Cabin <- gsub("[0-9]", "", train.cab$Cabin)
test.cab$Cabin <- gsub("[0-9]", "", test.cab$Cabin)
#Impute empty cells with NA:
train.cab$Cabin[train.cab$Cabin==""] <- NA
test.cab$Cabin[test.cab$Cabin==""] <- NA
#Logistic Regression with cabin feature
logistic.fit2 <- glm(Survived~., data = train.cab, family = binomial)
summary(logistic.fit2)
logistic.probs2 <- predict(logistic.fit2, newdata = test.cab, type = "response")
logistic.pred2=rep("0", length(test.imp$Age))
logistic.pred2[logistic.probs2>0.5] = "1"
mean(logistic.pred2==test.cab$Survived)

## Accuracy of LR model went down after including 'cabin' feature from .78 to .69

# 3 - Determine the adjusted odds ratio for Sex, Pclass, and Embarked using LR.
Adjusted_Odds_Ratio <- exp(coef(summary(logistic.fit))[, 'Estimate'])
Adjusted_Odds_Ratio
# 4 - For the LR models, vary the threshold to 0.8, 0.5, and 0.2.
mis_classification <- numeric(3)
f <- c(0.8, 0.5, 0.2)
for (i in 1:3){
  logistic.pred=rep("0", length(test.imp$Age))
  logistic.pred[logistic.probs>f[i]] = "1"
  mis_classification[i] <- mean(logistic.pred!=test.imp$Survived)
  
}
paste("For threshold=",f,"Misclassification rate is ",round(c(mis_classification),digits = 3))

# 5 - Develop ROC plot for LDA model
#ROC Curve
library(pROC)
#LDA ROC-
ldaroc <- roc(test.imp$Survived, lda.pr$posterior[,2],percent=TRUE, 
              plot=TRUE, ci=TRUE)
plot(ldaroc, main = "ROC Curve - LDA")
auc(ldaroc)

# 6 - What features do you think are important to make the prediction?
# Why? Evaluate the KNN model performance by including just the important features
## As per summary from logistics regression, features Pclass, Sex, Age, Sibsp are
## Important because, their p-value is less than 0.05. Thus they are significant
## towards prediction of survivors.

# Selecting only above mentioned features-
train.knn <- subset(train.imp, select=c(Pclass,Sex,Age,SibSp,Survived))
test.knn <- subset(test.imp, select=c(Pclass,Sex,Age,SibSp,Survived))

# KNN model - 
opt_knn_error <- numeric(10)
Opt_knn_err <- NULL
opt_k <- numeric(10)
opt_min_err <- 1
opt_min_k <- NULL
for (i in 1:10)
{
  opt_knn.fit <- knn(train.knn, test.knn, train.knn$Survived, k=i)
  opt_knn_err <- mean(opt_knn.fit!=test.knn$Survived)
  opt_knn_error[i] <- mean(opt_knn.fit!=test.knn$Survived)
  opt_k[i] <- i
  if (opt_knn_err < opt_min_err) {
    opt_min_err <- opt_knn_err
    opt_min_k <- i
  }
}
paste("For k=",c(opt_k),"Error is",round(c(opt_knn_error),digits = 3))
paste("The minimum knn_error is", opt_min_err,"and it occurs at k =", opt_min_k)


