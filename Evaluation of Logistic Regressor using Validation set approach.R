library(ISLR)
attach(Default)
?Default

#Setting seed
set.seed(10)

# a - Logistic Regression
glm.fit1 <- glm(default ~ income+balance,family = binomial)
summary(glm.fit1)

# b - Using the validation set approach, estimate the test error of this model
n <- length(default)
train <- sample(n,6000)
train
default.test <- Default$default[-train]
default.test
glm.fit2 <- glm(default ~ income+balance,data=Default, subset= train, family = binomial)
summary(glm.fit2)

##precitdtion on test data:
glm.prob2 <- predict(glm.fit2, Default[-train,], type="response")
glm.pred2 <- rep("No",4000)
length(glm.prob2)
 glm.pred2[glm.prob2>0.5] <- "Yes"

##Assess prediction accuracy
table(glm.pred2, default.test)
error <- mean(glm.pred2!=default.test)
error
# c- three different splits of the observations

Err <- numeric(3)
div <- c(5500, 9000, 7500)
set.seed(1)
for (i in 1:3)
  {
  train.lp <- sample(n, div[i])
    default.tlp <- default[-train.lp]
    glm.fitlp <- glm(default~income+balance, subset = train.lp, family = binomial)
    glm.problp <- predict(glm.fitlp, Default[-train.lp,], type="response")
    glm.predlp <- rep("No",(10000-div[i]))
    glm.predlp[glm.problp>0.5] <- "Yes"
    table(glm.predlp, default.tlp)
    Err[i] <- mean(glm.predlp!=default.tlp)
}
Err

# d- Consider another logistic regression model that predicts default using income, 
##balance and student (qualitative)
set.seed(10)
glm.fitd <- glm(default ~ income+balance+student,subset= train, family = binomial)
summary(glm.fitd)

glm.probd <- predict(glm.fitd, Default[-train,], type="response")
glm.predd <- rep("No",4000)
length(glm.probd)
glm.predd[glm.probd>0.5] <- "Yes"
##Assess prediction accuracy
table(glm.pred2, default.test)
error <- mean(glm.pred2!=default.test)
error


## Problem - 2
#simulated data set
set.seed(10)
x <- rnorm(200)
y <- x-2*x^2+rnorm(200)

# scatter plot of 𝑌 vs X
plot(x,y, main = "Plot of Y vs X")

# c - LOOCV errors
library(boot)
ip_dt <- data.frame(x, y)

#Using loop to generate models; 
cv.err <- rep(0,4)
for (i in 1:4)
{
  model <- glm(y ~ poly(x, i), data=ip_dt)
  cv.err[i] = cv.glm(ip_dt, model)$delta[1]
}
cv.err

# d - Repeat (c) using another random seed
set.seed(50)
x1 <- rnorm(200)
y1 <- x1-2*x1^2+rnorm(200)
ip_dt1 <- data.frame(x1, y1)
cv.err1 <- rep(0,4)
for (i in 1:4)
{
  model <- glm(y1 ~ poly(x1, i), data=ip_dt)
  cv.err1[i] = cv.glm(ip_dt1, model)$delta[1]
}
cv.err1

#f - 5-fold CV for the model selection:
cv.err5 <- rep(0,4)
for (i in 1:4)
{
  model <- glm(y ~ poly(x, i), data=ip_dt)
  cv.err5[i] = cv.glm(ip_dt, model, K=5)$delta[1]
}
cv.err5

#g - 10-fold CV for the model selection:
cv.err10 <- rep(0,4)
for (i in 1:4)
{
  model <- glm(y ~ poly(x, i), data=ip_dt)
  cv.err10[i] = cv.glm(ip_dt, model, K=10)$delta[1]
}
cv.err10
