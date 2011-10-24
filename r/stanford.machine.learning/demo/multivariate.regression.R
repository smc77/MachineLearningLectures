# details about dataset available http://archive.ics.uci.edu/ml/datasets/Housing

housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

# Subset the data for our model
housing <- housing[, c("CRIM", "RM", "PTRATIO", "LSTAT", "MEDV")]

plot(housing)

# Look at the linear model
housing.lm = lm(MEDV ~ CRIM + RM + PTRATIO + LSTAT, data=housing)
summary(housing.lm)

readline("Press <ENTER> to Continue.")

# Load data and initialize values
data <- read.csv("http://www.statalgo.com/wp-content/uploads/2011/10/housing.csv")

num.iterations <- 1000

x <- data[, c("area", "bedrooms")]
y <- matrix(data$price, ncol=1) / 1000 # Divide by a thousand so that numbers are in $1000's

# Function to standardize input values
zscore <- function(x, mean.val=NA) {
	if(is.matrix(x)) return(apply(x, 2, zscore, mean.val=mean.val))
	if(is.data.frame(x)) return(data.frame(apply(x, 2, zscore, mean.val=mean.val)))
	if(is.na(mean.val)) mean.val <- mean(x)
	sd.val <- sd(x)
	if(all(sd.val == 0)) return(x) # if all the values are the same
	(x - mean.val) / sd.val 
}

# Standardize the features
x.scaled <- zscore(x)

# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- (1 / nrow(y)) * (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}

gradient.descent <- function(x, y, alpha=0.1, num.iterations=500, threshold=1e-5, output.path=FALSE) {
    # Add x_0 = 1 as the first column
    if(is.vector(x) || (!all(x[,1] == 1))) x <- cbind(rep(1, m), x)
    x <- apply(x, 2, as.numeric)

    m <- if(is.matrix(x)) nrow(x) else length(x)
    num.features <- ncol(x)
    

    # Initialize the parameters
    theta <- matrix(rep(0, num.features), nrow=1)

    # Look at the values over each iteration
    theta.path <- theta
    for (i in 1:num.iterations) {
      theta <- theta - alpha * grad(x, y, theta)
      if(all(is.na(theta))) break
      theta.path <- rbind(theta.path, theta)
      if(i > 2) if(all(abs(theta - theta.path[i-1,]) < threshold)) break 
    }
    
    if(output.path) return(theta.path) else return(theta.path[nrow(theta.path),])
}

unscaled.theta <- gradient.descent(x=x, y=y, num.iterations=num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=x.scaled, y=y, num.iterations=num.iterations, output.path=TRUE)

summary(lm(y ~ area + bedrooms, data=x))
summary(lm(y ~ area + bedrooms, data=x.scaled))

qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")

# Look at output for various different alpha values
vary.alpha <- lapply(c(1e-9, 1e-7, 1e-3, 0.1), function(alpha) gradient.descent(x=x, y=y, alpha=alpha, num.iterations=num.iterations, output.path=TRUE))

par(mfrow = c(2, 2))
for (j in 1:4) {
	plot(vary.alpha[[j]][,2], ylab="area (alpha=1e-9)", xlab="iteration", type="l")
}

readline("Press <ENTER> to Continue.")

data <- read.csv("http://www.statalgo.com/wp-content/uploads/2011/10/housing.csv")
x <- as.matrix(cbind(intercept=rep(1, m), data[, c("area", "bedrooms")]))
theta <- solve(t(x) %*% x) %*% t(x) %*% y

