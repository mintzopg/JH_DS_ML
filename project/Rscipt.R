# Set the working directory
setwd("~/Courses/JH_DS_Machine_Learning/project")

#*****************************************************************************************
# Read the data directly from internet
library(curl)

# train_url <- curl("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
# test_url <- curl("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

# training <- read.csv(train_url, header = T, sep = ",")    # training data set
# testing <- read.csv(test_url, header = T, sep = ",")    # testing data set
# variables <- names(training)
#*****************************************************************************************

# or read the data from the hard drive to make it quicker; consider NAs, "" and #DIV/0! as NA values
training <- read.csv("pml-training.csv", header = T, sep = ",", na.strings = c("", "NA", "#DIV/0!"))
testing <- read.csv("pml-testing.csv", header = T, sep = ",", na.strings = c("", "NA", "#DIV/0!"))


# The goal of your project is to predict the manner in which they did the exercise. 
# This is the "classe" variable in the training set. 
# You may use any of the other variables to predict with. 
# You should create a report describing how you built your model, 
#       how you used cross validation, 
#       what you think the expected out of sample error is, 
#       and why you made the choices you did. 
# You will also use your prediction model to predict 20 different test cases. 

library(caret)

# Since the data set is large I am going to ommit as predictors the columns with NAs
na <- apply(training,2, is.na)
sumna <- apply(na, 2, sum); rm(na)
index <- which(sumna != 0); rm(sumna)

# I am also ommiting the irrelevat columns that keep logisitc info for the subjects {columns 1:7}
training <- subset(training, select = -c(1:7, index))

# next I filter for near-zero variance predictors, since a tree based model (classification) ...
# is impervious to this type of predictor since it would never be used in a split.
index <- nearZeroVar(training)
if (length(index) != 0) {
  training <- subset(training, select = -c(index))
}


# split the training set into myTraining 60% and myValidation 40% sets
inTrain <- createDataPartition(y = training$classe, p = 0.6, list = F)
myTraining <- training[inTrain, ]
myValidation <- training[-inTrain, ]

# do PCA
preProc <- preProcess(myTraining[, -53], method = "pca")
preProc

# ...must do same tranformation for Validation and testing sets and apply the machine learning algorithm

myTrainPC <- predict(preProc, myTraining[, -53]) # calculate PC for training data set
myValidationPC <- predict(preProc, myTraining[, -53]) # calculate PC for validation data set 

#### Train model #####


