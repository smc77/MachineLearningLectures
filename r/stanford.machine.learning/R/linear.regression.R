
# First look at a linear model fit to the housing data

# details about dataset available http://archive.ics.uci.edu/ml/datasets/Housing
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")[, c(6, 14)]
housing.lm <- lm(median.values ~ num.rooms, data=housing)
plot(housing)
abline(housing.lm)
summary(housing.lm)
