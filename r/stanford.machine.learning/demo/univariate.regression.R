# First look at a linear model fit to the housing data

# details about dataset available http://archive.ics.uci.edu/ml/datasets/Housing
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")[, c(6, 14)]
names(housing) <- c("num.rooms", "median.values")

housing.lm <- lm(median.values ~ num.rooms, data=housing)
plot(housing)
abline(housing.lm)
summary(housing.lm)

blah <- readline("Press <ENTER> to Continue.")

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

blah <- readline("Press <ENTER> to Continue.")

# Test a few different slopes with different intercepts
x <- -50:50
y <- -10:10
z <- sapply(x, function(intercept) (sapply(y, function(slope, intercept) loss(intercept, slope, x, y), intercept=intercept)))
rownames(z) <- y
colnames(z) <- x

# 3D plot of loss function
library(lattice)

wireframe(z, shade=TRUE, xlab="theta0", ylab="theta1", zlab="loss function", aspect =  c(61/87, 0.4), light.source = c(10,0,10))

blah <- readline("Press <ENTER> to Continue.")

# Contour plot
library(reshape)
library(ggplot2)

loss.values <- as.data.frame(melt(z))
names(loss.values) <- c("slope", "intercept", "loss")

v <- ggplot(loss.values, aes(intercept, slope, z = loss)) 
v + geom_tile(aes(fill = loss)) + stat_contour()

