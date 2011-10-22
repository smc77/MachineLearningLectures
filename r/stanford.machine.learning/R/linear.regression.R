
# Create the loss function
loss <- function(intercept, slope, x, y) sum(((intercept + (slope * x)) - y)^2)/2
