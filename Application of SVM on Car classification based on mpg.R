library(ISLR)

x <- Auto
attach(x)

# Create a binary variable that takes on a 1 for cars with gas mileage above the median, and a 0
# for cars with gas mileage below the median. Use this variable as response in the following
# analysis. Randomly partition 70% of data for training and 30% for testing.

x$mpg <- factor(ifelse(x$mpg<median(x$mpg),0,1))
n <- nrow(x)
set.seed(10)
random <- sample(1:n, size = round(0.7*n), replace = FALSE)
train.x <- x[random,]
test.x <- x[-random,]

# Develop a support vector classifier to predict whether a car gets high or low gas mileage.
# Evaluate performance of five different values of cost using Cross Validation (CV). Report
# performance on test data and comment on your results.

# Tuning of hyper parameters :-
library(e1071)
set.seed(100)
hyper.tune=tune(svm,mpg~.,data= train.x, importance = TRUE,
                ranges=list(cost=c(0.01, 0.5, 5, 20, 50)),kernel="linear")

#best performance 
opt.cost = hyper.tune$best.model
opt.cost$cost
opt.cost

##calculate accuracy
ypred.c=predict(opt.cost,newdata=test.x)
table(ypred.c,test.x$mpg)
accuracy.c <- mean(ypred.c==test.x$mpg)
accuracy.c

# Tuning of hyper parameters for Radial:-
set.seed(1000)
radial.tune=tune(svm,mpg~.,data= train.x, importance = TRUE,
                ranges=list(cost=c(0.01, 0.05, 5, 20, 50), 
                            gamma=c(0.1,0.5,0.8,2,4)),kernel="radial")
summary(radial.tune)

#best performance 
opt.rdl.cost = radial.tune$best.model
opt.rdl.cost

##calculate accuracy
ypred.r=predict(opt.rdl.cost,newdata=test.x)
table(ypred.r,test.x$mpg)
accuracy.r <- mean(ypred.r==test.x$mpg)
accuracy.r

# Tuning of hyper parameters for Radial:-
set.seed(3)
polynomial.tune=tune(svm,mpg~.,data= train.x, importance = TRUE,
                 ranges=list(cost=c(0.01, 0.05, 5, 20, 50), 
                             degree=c(2:6)),kernel="polynomial")
summary(polynomial.tune)

#best performance 
opt.pml.cost = polynomial.tune$best.model
opt.pml.cost

##calculate accuracy
ypred.p=predict(opt.pml.cost,newdata=test.x)
table(ypred.p,test.x$mpg)
accuracy.p <- mean(ypred.p==test.x$mpg)
accuracy.p

