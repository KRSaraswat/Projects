library(ISLR)

x <- Carseats
attach(x)

# (a) Split the data set into a training set (70%) and a test set (30%).

n <- nrow(Carseats)
set.seed(10)
train.t <- sample(1:n, size = round(0.7*n), replace = FALSE)
test.t <- setdiff(1:n, train.t)
train.x <- x[train.t,]
test.x <- x[test.t,]
sales.train <- Sales[train.t]
sales.test <- Sales[test.t]

# (b) Fit a regression tree to the training set. Plot the tree, and interpret the results. 
# Then compute the test MSE.
library(tree)

train <- tree(Sales~., data = train.x)
summary(train)
plot(train)
text(train,pretty=0)
tree.pred <- predict(train, test.x)
tree.pred.num <- as.numeric(as.character(tree.pred))
sales.test.num <- as.numeric(as.character(sales.test))
test_mse <- mean((tree.pred.num-sales.test.num)^2)
test_mse

# (c) Prune the tree obtained in (b). Use cross validation to determine the optimal level
# of tree complexity. Plot the pruned tree and interpret the results. Compute the test
# MSE of the pruned tree. Does pruning improve the test error?
set.seed(113)
cv.train <- cv.tree(train)

names(cv.train)  
cv.train
# Visualization of best 
plot(cv.train$size,cv.train$dev,type="b")

##pruning based on cv results
prune.train=prune.tree(train, best=7)
plot(prune.train)
text(prune.train,pretty=0)
##test the pruned tree
tree.pred.prune=predict(prune.train,test.x)
tree.prune.num <- as.numeric(as.character(tree.pred.prune))

test_mse.prune <- mean((tree.prune.num-sales.test.num)^2)
test_mse.prune

#(d) Use the Bagging approach to analyze the data. Explore three different values each for
#hyper-parameter and document Test MSE. Determine which variables are most important.
##bagging: randomforest with mtry=#Predictors
library(randomForest)
set.seed(10)
bag.sales=randomForest(Sales~.,data=train.x, mtry=10, ntree=100, importance=TRUE)
bag.sales
# Tuning of hyperparamter (ntree):-
library(e1071)
hyper.tune=tune(randomForest,Sales~.,data= train.x, importance = TRUE,
              ranges=list(ntree=c(100, 300, 700, 2000),mtry=10))
summary(hyper.tune)
#best performance is at ntree = 300
opt.ntree = hyper.tune$best.model
opt.ntree$ntree
opt.ntree
##calculate test MSE
yhat.bag=predict(opt.ntree,newdata=test.x)
yhat.bag.num <-as.numeric(as.character(yhat.bag)) 
mean((yhat.bag.num-sales.test.num)^2)

##actual observations of test data and predictions
plot(yhat.bag.num,sales.test.num)
abline(0,1)

## variable importance
opt.ntree$importance
#(e) Use Random Forest to analyze the data. Explore three different values for each 
#hyperparameter and document Test MSE. Determine which variables are most important.
set.seed(10)
rf.sales=randomForest(Sales~.,data=train.x, mtry=10, ntree=100, importance=TRUE)
rf.sales
# Tuning of hyperparamter (ntree):-
rf.tune=tune(randomForest,Sales~.,data= train.x, importance = TRUE,
                ranges=list(ntree=c(100, 200, 300, 400,500, 700,1100, 2000),
                            mtry=c(1,2,3,4,5,6,7,8,9,10)))
summary(rf.tune)
#best performance is at ntree = 300
opt.rf = rf.tune$best.model
opt.rf$ntree
opt.rf$mtry
##calculate test MSE
yhat.rf=predict(opt.rf,newdata=test.x)
yhat.rf.num <-as.numeric(as.character(yhat.rf)) 
mean((yhat.rf.num-sales.test.num)^2)

##actual observations of test data and predictions
plot(yhat.rf.num,sales.test.num)
abline(0,1)

## variable importance
opt.rf$importance
#(f) Use the Boosting approach to analyze the data. Explore three different values for each
#hyper-parameter and document Test MSE. Determine which variables are most important.
library(gbm)
set.seed(10)
boost.sales = gbm(Sales~.,data=train.x, distribution="gaussian",
                   n.trees=5000, interaction.depth=4)
summary(boost.sales) ##relative influence plot
#Test MSE:
boost.pred <- predict(boost.sales, newdata = test.x)
test_mse.boost <- mean((boost.pred-sales.test.num)^2)
test_mse.boost

# Tuning of Hyperparamters:
library(caret)
set_tune = expand.grid(interaction.depth=c(2, 3, 5), shrinkage=c(0.1,0.05,0.01,0.001),
                       n.trees = c(100,150,200,250),
                       n.minobsinnode=c(6,8,10))
ctrl.trn <- trainControl(method = 'cv',number = 8)
set.seed(10)
boost.tune = train(Sales~., data=train.x, method = "gbm", verbose=FALSE,
                   distribution="gaussian", bag.fraction=0.75,
                   trControl=ctrl.trn,tuneGrid=set_tune,metric = 'RMSE')
boost.tune$bestTune
yhat.boost = predict(boost.tune,newdata=test.x)
mean((yhat.boost-sales.test)^2)
## variable importance
summary(boost.tune)
