###############################
# Question 1                  #
###############################

# Read CSV 
churn_dataset <- read.csv('assign2_ChurnData.csv')

# EDA
str(churn_dataset)
head(churn_dataset)
summary(churn_dataset)

# Looking at the summary

# Churn Rate: No - 0.7384% & Yes - 0.2616%
# Based on the training data, we can safely assume that the churn rate will have a hihger probability of No!

# Converting to data frame for easier data processing 
adm_churn <- as.data.frame(churn_dataset)

# Remove Null Values From the DataFrame
adm_churn <- na.omit(adm_churn)

# Create a sample training data using 70% of the data frame and saving it as 
# train data
train <- sample(nrow(adm_churn), 0.7*nrow(adm_churn), replace = FALSE)
adm_Train <- adm_churn[train,]

# Test Data
adm_Valid <- adm_churn[-train,]

# Summary of Train and Test Data
summary(adm_Train)
summary(adm_Valid)


###################################
# Create the Decision Tree        #
###################################

# Using the library rpart 
# rpart - Recursive Partitioning and Regression Tress
# rpart done the recursive parttioning and regression tress for me so i don't need to know
# which is more important and less important. The software decide by itself.

library("rpart")
tree <- rpart(Churn ~. , method='class', data=adm_Train)
printcp(tree)

# install.packages("rpart.plot")
# For rpart plotting
library(rpart.plot)
prp(tree)

predTrain <- predict(tree, adm_Train, type = "class")  # prediction on train set
mean(predTrain == adm_Train$Churn)                     # classification accuracy
predValid <- predict(tree, adm_Valid, type = "class")  # prediction on validation set
mean(predValid == adm_Valid$Churn)                     # classification accuracy

##############################
# Inference                  #
##############################

# From the inference, the prediction on train set and the prediction on the test/validation set is about
# 79% - 80%. From this, we can tell that we have done a tree that can garner us about minimum of 79% accuracy.

# This however is not tested with the evaluation dataset. It may be possible that we get a lower degree of accuracy for 
# evaluation dataset.


###########################
# Random Forest           #
###########################

# Library for the randomForest
library(randomForest)
rf.model <- randomForest(Churn ~., data=adm_Train, ntree=2000)

print(rf.model)

# Accuracy: (2281 + 483) / (2281 + 279 + 450 + 483) 
# 79%

# We are able to see what is the prediction of our model with this. [Yes / No]
rf.model$predicted

# Create the prediction for our validation dataset.
Prediction <- predict(rf.model, adm_Valid)
submit <- data.frame(Churn = Prediction)

# Save it into a csv file in case for kaggle submission or professor's own engine.
write.csv(submit, file = "churn_prediction.csv", row.names = FALSE)


##########################
# Q2                     #
##########################

# Read the Data
titanic_train <- read.csv("titanic_train.csv")
titanic_test <- read.csv("titanic_test.csv")

# EDA
summary(titanic_train)
head(titanic_train)

# For me, I didn't take into account all the variable because I don't think the name 
# affect the dataset, it doesn't mean that you are a McDonald means that you will be saved
# The Cabin has a ton of other data and it doesn't make sense to make a mean or predict them
# Finally, it doesn't really matter where the passenger embarked.

# For the null values I decided to use the model created by rpart which is a tree model to get the estimated Age Value
# The code below shows that on the training and testing data set
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare,
                data=titanic_train[!is.na(titanic_train$Age),], 
                method="anova")
titanic_train$Age[is.na(titanic_train$Age)] <- predict(Agefit, titanic_train[is.na(titanic_train$Age),])

AgefitTest <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare,
                data=titanic_test[!is.na(titanic_test$Age),], 
                method="anova")
titanic_test$Age[is.na(titanic_test$Age)] <- predict(AgefitTest, titanic_test[is.na(titanic_test$Age),])

summary(titanic_test)
head(titanic_test)

# Fare has 1 NA for test as well.. to get it i will use a mean 
which(is.na(titanic_test$Fare))
titanic_test$Fare[153] <- mean(titanic_test$Fare, na.rm = T)

summary(titanic_test)
head(titanic_test)

# Once this is done, I then create the tree model for the dataset.

# Similar to Question 1. I will just use rpart to determine the best regression tree 
# based on recursive function.
library("rpart")
titanic_tree <- rpart(Survived ~Pclass + Sex + Age + SibSp + Parch + Fare, method='class', data=titanic_train)
printcp(titanic_tree)

# Display the nicer tree plot
prp(titanic_tree)

predTrain <- predict(titanic_tree, titanic_train, type = "class")  # prediction on train set
mean(predTrain == titanic_train$Survived)                     # classification accuracy

# The accuracy is 84.4% which is quite high! This shows that we have removed the useless 
# variables and that most of the variable that are present here is useful to determine whether 
# the passenger survived

# Predict test
predTest <- predict(titanic_tree, titanic_test, type="class") # prediction on test set
print(predTest) # Prediction based on test 


###########################
# Random Forest           #
###########################

library(randomForest)

# Build the randomForest model !
fit <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare , method='class', data=titanic_train, ntree=2000)

# Predict the test set
Prediction <- predict(fit, titanic_test)
submit <- data.frame(PassengerId = titanic_test$PassengerId, Survived = Prediction)

# Save it into a csv file in case professor is uploading to Kaggle
write.csv(submit, file = "firstforest.csv", row.names = FALSE)

# The random forest is built and then we saved it into the csv file for validation online.

##########################
# Summary                #
##########################

# In this assignment, we learn about how to build regression model and random forest model
# To get a high accuracy of our model, we need to remove the variable that we think are not neccessary
# This is shown by Q2 of this assignment which based on my human intellect remove three variables.
# I garner an accuracy of 80% for Q1 and 84% for Q2 which is quite accurate however,
# I won't be so naive to say that will be a guaranteed for the evaluation dataset
# However, I have a much higher confidence that it will be accurate rather than inaccurate
# Thank you Prof :)
