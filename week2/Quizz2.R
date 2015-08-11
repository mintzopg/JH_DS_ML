#### question 1 ####


#Which of the following commands will create training and test sets with about 50% ... 
# ... of the observations assigned to each?
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]

#### question 2 ####

# Make a histogram and confirm the SuperPlasticizer variable is skewed. 
# Normally you might use the log transform to try to make the data more symmetric. 
# Why would that be a poor choice for this variable?
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

## Answ: 
# There are a large number of values that are the same ...
# ... and even if you took the log(SuperPlasticizer + 1) they would still all be identical ...
# ... so the distribution would not be symmetric.



####  question 3 ####

# Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function from the caret package. 
# Calculate the number of principal components needed to capture 80% of the variance. 
# How many are there?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Find all the predictor variables in the training set that begin with IL
n <- grep("^IL", names(training))

pp <- preProcess(training[, n], method = "pca", thresh = 0.9)
train_PCs <- predict(pp, training[, n])
# number of Principla components required to capture 90% of the variance
ncol(train_PCs)


#### question 4 ####
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# Create a training data set consisting of only the predictors 
# with variable names beginning with IL and the diagnosis.
n <- grep("^IL", names(training))
training <- training[, c(1, n)]

#  Build two predictive models, one using the predictors as they are and one ...
# using PCA with principal components explaining 80% of the variance in the predictors. 
# Use method="glm" in the train function. 
# What is the accuracy of each method in the test set? Which is more accurate?

### Model fit using PCA for principal components
pp <- preProcess(training[, -1], method = "pca", thresh = 0.8) # create preprocess object {-1 excludes the outcome "diagnosis" variable}
train_PCs <- predict(pp, training[, -1]) # calc. Principal Components for train data
# run model on outcome
model_fit_PC <- train(training$diagnosis ~., method = "glm", data = train_PCs)

# create PCs for test set
test_PCs <- predict(pp, testing[, n])

# compare results
confusionMatrix(testing$diagnosis, predict(model_fit_PC, test_PCs))

### Using Predictors as they are
model_fit <- train(training$diagnosis ~., method = "glm", data = training)
confusionMatrix(testing$diagnosis, predict(model_fit, testing))


