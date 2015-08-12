# Set the working directory
setwd("~/Courses/JH_DS_Machine_Learning/project")

#*****************************************************************************************
# Read the data directly from internet
library(curl)

train_url <- curl("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
test_url <- curl("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

training <- read.csv2(train_url, header = T, sep = ",")    # training data set
testing <- read.csv2(test_url, header = T, sep = ",")    # testing data set
variables <- names(training)
#*****************************************************************************************

# or read the data from the hard drive to make it quicker
training <- read.csv2("pml-training.csv", header = T, sep = ",")
testing <- read.csv2("pml-testing.csv", header = T, sep = ",")

library(dplyr)
training <- tbl_df(training); testing <- tbl_df(testing)

# there is a dirrenece in the column name of column 160
# sum(variables == names(testing))
which (names(training) != names(testing)) # see below

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
training1 <- subset(training, select = -c(1:7, index))

# next I filter for near-zero variance predictors, since a tree based model (classification) ...
# is impervious to this type of predictor since it would never be used in a split.

index <- nearZeroVar(training1)
training2 <- subset(training1, select = -c(index))

# coerse values to numeric
training2 <- cbind(as.data.frame(apply(training2[, -53], 2, as.numeric)), training2[, 53]) 

# split the training set into myTraining 60% and myValidation 40% sets
inTrain <- createDataPartition(y = training2$classe, p = 0.6, list = F)
myTraining <- training2[inTrain, ]
myValidation <- training2[-inTrain, ]

# do PCA
preProc <- preProcess(myTraining[, -53], method = "pca")

# ...must do sam tranformation for Validation and testing sets and apply the machine learning algorithm


