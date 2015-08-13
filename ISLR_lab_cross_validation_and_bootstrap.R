# ISLR page 191

library(ISLR)
set.seed(1)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train) # linear model

attach(Auto)
print("MSE linear function")
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subest = train) # polynomial quadratic
print("MSE quadratic function")
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subest = train) # cubic
print("MSE cubic function")
mean((mpg - predict(lm.fit3, Auto))[-train] ^ 2)

# choose different training set
set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train) # linear model
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subest = train) # polynomial quadratic
lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subest = train) # cubic

# linear
print("Using a different training set")
print("MSE linear function")
mean((mpg - predict(lm.fit, Auto))[-train] ^ 2)
#square
print("MSE quadratic function")
mean((mpg - predict(lm.fit2, Auto))[-train] ^ 2)
# cubic
print("MSE cubic function")
mean((mpg - predict(lm.fit3, Auto))[-train] ^ 2)

# Leave-One-Out-Cross-Validation
print("************************* LOOCV ******************************")

glm.fit <- glm(mpg ~ horsepower, data = Auto)
coef(glm.fit)

# Use of cv.glm() part of boot library
library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Repeat for increasingly complex polynomialfits
cv.error <- rep(0, 5) # initialize errors vector

for (i in 1:5){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error
plot(1:5, cv.error, type = "l")
# k-Fold Cross Validation
print("**************** k-Fold Cross Validation*********************")

set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10){
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1] 
}
cv.error.10
plot(1:10, cv.error.10, type = "l")

# The Bootstrap
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <-data$Y[index]
  return((var(Y) - cov(X, Y))/ (var(X) + var(Y) -2 * cov(X, Y)))
}

alpha.fn(Portfolio, 1:100)

boot(Portfolio, alpha.fn, R = 1000)





