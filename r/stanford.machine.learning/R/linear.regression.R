
# Create the loss function
loss <- function(intercept, slope, x, y) sum(((intercept + (slope * x)) - y)^2)/2


# Gradient descent function
grad <- function(x, y, theta) {
  gradient <- (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}

# Show the path of a gradient descent parameter optimization
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

