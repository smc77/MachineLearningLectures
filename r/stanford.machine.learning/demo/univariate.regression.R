# First look at a linear model fit to the housing data

# details about dataset available http://archive.ics.uci.edu/ml/datasets/Housing
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")[, c(6, 14)]
names(housing) <- c("num.rooms", "median.values")

housing.lm <- lm(median.values ~ num.rooms, data=housing)
plot(housing)
abline(housing.lm)
summary(housing.lm)

readline("Press <ENTER> to Continue.")

# Example of randomly chosen lines
plot(housing)
abline(0, 5, col="red")
abline(-50, 10, col="blue")

x <- housing[, "num.rooms"]
y <- housing[, "median.values"]

# Create the loss function
loss <- function(intercept, slope, x, y) sum(((intercept + (slope * x)) - y)^2)/2

# Create some data for a given line and compute the loss
loss(0, 5, x, y)
loss(-30, 10, x, y)

readline("Press <ENTER> to Continue.")

# Test a few different slopes with different intercepts
x <- -50:50
y <- -10:10
z <- sapply(x, function(intercept) (sapply(y, function(slope, intercept) loss(intercept, slope, x, y), intercept=intercept)))
rownames(z) <- y
colnames(z) <- x

# 3D plot of loss function
library(lattice)

wireframe(z, shade=TRUE, xlab="theta0", ylab="theta1", zlab="loss function", aspect =  c(61/87, 0.4), light.source = c(10,0,10))

readline("Press <ENTER> to Continue.")

# Contour plot
library(reshape)
library(ggplot2)

loss.values <- as.data.frame(melt(z))
names(loss.values) <- c("slope", "intercept", "loss")

v <- ggplot(loss.values, aes(intercept, slope, z = loss)) 
v + geom_tile(aes(fill = loss)) + stat_contour()

readline("Press <ENTER> to Continue.")

# Load data and initialize values
data <- read.csv("http://www.statalgo.com/wp-content/uploads/2011/10/housing.csv")

alpha <- 0.01
m <- nrow(data)
x <- matrix(c(rep(1,m), data$area), ncol=2)
y <- matrix(data$price, ncol=1) / 1000

# Z-Score the feature
x.scaled <- x
x.scaled[,2] <- (x[,2] - mean(x[,2]))/sd(x[,2])

# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}

gradient.path <- function(x) {
    # Initialize the parameters
    theta <- matrix(c(0, 0), nrow=1)

    # Look at the values over each iteration
    theta.path <- matrix(ncol=2)
    for (i in 1:500) {
      theta <- theta - alpha * 1/m * grad(x, y, theta)
	if(all(is.na(theta))) break
      theta.path <- rbind(theta.path, theta)
    }
    theta.path
}

unscaled.theta <- gradient.path(x)
scaled.theta <- gradient.path(x.scaled)

summary(lm(y ~ x[, 2]))
summary(lm(y ~ x.scaled[, 2]))

qplot(1:501, scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_1")
