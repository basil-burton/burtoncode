#Read in wine quality data for Problem 2
library(readr)
Q2_winequality <- read_csv("Desktop/Penn State/Spring 2019/Foundations of Predictive Analytics/Assignments/Final Exam/Q2_winequality.csv")

names(Q2_winequality) <- c("fixedAcidity", "volativeAcidity", "citricAcid", "residualSugar", "chlorides", "freeSulfurDioxide", "totalSulfurDioxide",
                          "desity", "pH", "sulfates", "alcohol", "winetype","quality")

#Convert to data fram
Q2_winequality <- as.data.frame(Q2_winequality)

#Explore wine quality data
summary(Q2_winequality)
str(Q2_winequality)
hist(Q2_winequality$quality)
boxplot(Q2_winequality$quality)

#Check for any NA Values
apply(Q2_winequality,2, function(x) sum(is.na(x)))

# remove records with missing values
Q2_winequality <- Q2_winequality[complete.cases(Q2_winequality), ]

#Check if quality and wine type columns are factors
is.factor(Q2_winequality$quality)
is.factor(Q2_winequality$winetype)

#Converty quality and wine type columns into factors
Q2_winequality$quality <- factor(Q2_winequality$quality, levels = 3:9, labels = c("3","4","5","6","7","8","9"))
Q2_winequality$winetype <- factor(Q2_winequality$winetype)

#Split data into test and training data
library(caret)
set.seed(300)
winequality_sampling_vector <- createDataPartition(Q2_winequality$quality, p = 0.70, list = FALSE)
winequality_train <- Q2_winequality[winequality_sampling_vector,]
winequality_test <- Q2_winequality[-winequality_sampling_vector,]

#CART Tree for training data
library(rpart)
wqtree_train <- rpart(quality ~. , method = "class", data = winequality_train)
printcp(wqtree_train)
summary(wqtree_train)

#Plotted CART Tree for training data
plot(wqtree_train, uniform=TRUE, main = "CART tree for wine quality data")
text(wqtree_train, use.n=TRUE, all=TRUE, cex=.6)
plotcp(wqtree_train)

#CART Tree for test data
wqtree_test <- rpart(quality ~. , method = "class", data = winequality_test)
printcp(wqtree_test)
summary(wqtree_test)

#Plotted tree for test data
plot(wqtree_test, uniform=TRUE, main = "CART tree for wine quality data")
text(wqtree_test, use.n=TRUE, all=TRUE, cex=.6)
plotcp(wqtree_test)

#CART training data performance
trainCartPredictions <- predict(wqtree_train, data = winequality_train, type = "class")
mean(winequality_train$quality == trainCartPredictions)

#CART test data performance
testCartPredictions <- predict(wqtree_test, data = winequality_test, type ="class")
mean(winequality_test$quality == testCartPredictions)


#Logistic Regression Model for test data
library(nnet)
wqlog_train <- multinom(quality ~., data = winequality_train, maxit = 1000)
summary(wqlog_train)

#Logistic Regression Model for train data
wqlog_test <- multinom(quality ~., data = winequality_test, maxit = 1000)
summary(wqlog_test)

#Performance for Logistic Tree Model train set using multinom
trainLogPredictions <- predict (wqlog_train, winequality_train)
mean(trainLogPredictions == winequality_train$quality)

#Performance for Logistic Tree Model test set using multinom
testLogPredictions <- predict (wqlog_test, winequality_test)
mean(testLogPredictions == winequality_test$quality)

#C5.0 Tree for train data
library(C50)
wqc_train <- C5.0(quality ~., data = winequality_train)
summary(wqc_train)

#C5.0 Tress for test data
wqc_test <- C5.0(quality ~., data = winequality_test)


#C5.0 train performance
trainCpredictions <- predict(wqc_train, winequality_train)
mean(winequality_train$quality == trainCpredictions)

#C5.0 test performance
testCpredictions <- predict(wqc_test, winequality_test)
mean(winequality_test$quality == testCpredictions)

#Neural Network for train data
library(nnet)
wqnnet_train <- nnet(quality ~., data = winequality_train, size = 10 , maxit = 1000, decay = 0.01)

#Neural Network for test data
wqnnet_test <- nnet(quality ~., data = winequality_test, size = 10 , maxit = 1000, decay = 0.01)

#Performance of nnet train data
trainNnetPredictions <- predict(wqnnet_train, winequality_train, type = "class")
mean(trainNnetPredictions == winequality_train$quality)

#Performance of nnet test data
testNnetPredictions <- predict(wqnnet_test, winequality_test, type = "class")
mean(testNnetPredictions == winequality_test$quality)

#Naive Bayes model for train set
library("e1071")
library(caret)
wq_nb_train <- naiveBayes(quality ~., data = winequality_train)

#Naive Bayes model for test set
wq_nb_test <- naiveBayes(quality ~., data = winequality_test)

#Performance of Naive Bayes train set
wq_nb_train_predictions <- predict(wq_nb_train, winequality_train)
mean(wq_nb_train_predictions == winequality_train$quality)

#Performance of Naive Bayes test set
wq_nb_test_predictions <- predict(wq_nb_test, winequality_test)
mean(wq_nb_test_predictions == winequality_test$quality)


#C5.0 Train Model with Boosting
wqc_train_boosted <- C5.0(quality ~., data = winequality_train, trials = 10)

#C5.0 Test Model with Boosting
wqc_test_boosted <- C5.0(quality ~., data = winequality_test, trials = 10)

# predict response variable on training dataset
wqc_train_boosted_predictions <- predict(wqc_train_boosted, winequality_train, type = 'class')

# performance of C5.0 Train model with boosting
mean(wqc_train_boosted_predictions  == winequality_train$quality)

# predict response variable on test dataset
wqc_test_boosted_predictions <- predict(wqc_test_boosted, winequality_test, type = 'class')

# performance of C5.0 test model with boosting
mean(wqc_test_boosted_predictions  == winequality_test$quality)

#Train Confusion Matrix
table(predicted = wqc_train_boosted_predictions , actual = winequality_train$quality)

#Test Confusion Matrix
table(predicted = wqc_test_boosted_predictions , actual = winequality_test$quality)