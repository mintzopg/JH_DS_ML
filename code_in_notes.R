### Plotting Predictors

# load relevant libraries
library(ISLR); library(caret); 
# load wage data
data(Wage)
# create training and test sets
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
# plot relationships between the predictors and outcome
featurePlot(x=training[,c("age","education","jobclass")], y = training$wage,plot="pairs")

# qplot plus linear regression lines
qplot(age,wage,colour=education,data=training)+geom_smooth(method='lm',formula=y~x)

# load Hmisc and gridExtra packages
library(Hmisc);library(gridExtra);
# cute the wage variable
cutWage <- cut2(training$wage,g=3)
# plot the boxplot
p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
# plot boxplot and point clusters
p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
# plot the two graphs side by side
grid.arrange(p1,p2,ncol=2)

# tabulate the cutWage and jobclass variables
t <- table(cutWage,training$jobclass)
# print table
t

# convert to proportion table based on the rows
prop.table(t,1)


# produce density plot
qplot(wage,colour=education,data=training,geom="density")


### Preprocessing

# load spam data
data(spam)
# create train and test sets
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
# create preProcess object for all predictors ("-58" because 58th = outcome)
preObj <- preProcess(training[,-58],method=c("center","scale"))
# normalize training set
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
# normalize test set using training parameters
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
# compare results for capitalAve variable
rbind(train = c(mean = mean(trainCapAveS), std = sd(trainCapAveS)),
      test = c(mean(testCapAveS), sd(testCapAveS)))

# set up BoxCox transforms
preObj <- preProcess(training[,-58],method=c("BoxCox"))
# perform preprocessing on training data
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
# plot histogram and QQ Plot
# Note: the transformation definitely helped to
# normalize the data but it does not produce perfect result
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# Make some values NA
library(RANN)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=0.05)==1
training$capAve[selectNA] <- NA
# Impute and standardize
preObj <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj,training[,-58])$capAve
# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
# compute differences between imputed values and true values
quantile(capAve - capAveTruth)

### Covariate creation
# setting up data
inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
# create a dummy variable object
dummies <- dummyVars(wage ~ jobclass,data=training)
# create the dummy variable columns
head(predict(dummies,newdata=training))

# print nearZeroVar table
nearZeroVar(training,saveMetrics=TRUE)

# load splines package
library(splines)

# create polynomial function
bsBasis <- bs(training$age,df=3)
# fit the outcome on the three polynomial terms
lm1 <- lm(wage ~ bsBasis,data=training)
# plot all age vs wage data
plot(training$age,training$wage,pch=19,cex=0.5)
# plot the fitted polynomial function
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)

# predict on test values
head(predict(bsBasis,age=testing$age))


