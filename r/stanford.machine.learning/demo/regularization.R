#

n <- 10

f <- function(x) sin(2 * pi * x)

x <- seq(0, 1, length=n)
y <- f(x) + rnorm(n, sd=0.2)

plot(data.frame(x, y))
curve(f, type="l", col="green", add=TRUE)

# How to fit explicit polynomial terms4; easier with poly() function
fit <- lm(y ~ x + I(x^2) + I(x^3))
fit <- lm(d ~ poly(x, 3, raw=TRUE))
summary(fit)

# We can predict these values using the coefficients directly 
x.data <- data.frame(rep(1, n), x, x^2, x^3)
y.pred <- apply(fit[["coefficients"]] * t(x.data), 2, sum)

# Or we can just use the predict function to do the same thing
y.pred <- predict(fit)

points(data.frame(x, y.pred), col="red")

error.function <- function(y, y.pred) sum((y.pred - y)^2) / 2

e.rms <- function(y, y.pred) sqrt(2 * error.function(y=y, y.pred=y.pred) / length(y))

e.rms(y, y.pred)


par(mfrow=c(2, 2))

for (i in c(1, 3, 6, 9)) {
  plot(data.frame(x, d), xlab=paste("polynomial fit order", i), ylab="f(x)")
  curve(f, type="l", col="green", add=TRUE)
  fit <- lm(d ~ poly(x, i, raw=TRUE))
  p <- polynom(coef(fit))
  curve(p, col="red", add=TRUE)
}

fit.values <- matrix(ncol=2)
for (i in 1:9) {
  fit.sum <- summary(lm(d ~ poly(x, i, raw=TRUE)))
  fit.values <- rbind(fit.values, c(i, fit.sum["r.squared"][[1]]))
}

colnames(fit.values) <- c("Polynomial Order", "R^2")
plot(fit.values, type="l")

# Adding more data:
par(mfrow=c(2, 2))

for (i in c(10, 50, 100, 1000)) {
  x <- seq(0, 1, length=i)
  d <- f(x) + rnorm(i, sd=0.15)

  plot(data.frame(x, d), xlab=paste("Data size:", i), ylab="f(x)")
  curve(f, type="l", col="blue", add=TRUE)
  fit <- lm(d ~ poly(x, 9, raw=TRUE))
  summary(fit)["r.squared"][[1]]
  p <- polynom(coef(fit))
  curve(p, col="red", add=TRUE)
}


test <- f(x) + rnorm(length(x), sd=0.1)
training <- f(x) + rnorm(length(x), sd=0.1)


#
# Let's look at how the different models generalize between different datasets
#

n.training <- 10
n.test <- 100

build.data <- function(n) {
	f <- function(x) sin(2 * pi * x)
	x <- seq(0, 1, length=n)
	y <- f(x) + rnorm(n, sd=0.2)
	return(data.frame(y=y, x=x))
}

training <- build.data(n=n.training)
test <- build.data(n=n.test)

test.poly.error <- function(training, test, polynomials=1:9) {

	errors.training <- errors.test <- numeric()
	for(i in polynomials) {
		fit <- lm(y ~ poly(x, i, raw=TRUE), data=training)
		y.pred.training <- predict(fit)
		errors.training[i] <- e.rms(training$y, y.pred.training)
		y.pred.test <- predict(fit, newdata=test)		
		errors.test[i] <- e.rms(test$y, y.pred.test)
	}
	errors <- data.frame(polynomial=polynomials, training.error=errors.training, test.error=errors.test)
	return(errors)
}

errors <- test.poly.error(training, test)
errors <- melt(errors, x)
colnames(errors) <- c("polynomial", "dataset", "error")

p <- ggplot(errors, aes(x=polynomial, y=error, grouping=dataset, colour=dataset)) + geom_line()
p
