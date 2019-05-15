# demo uses the dataset "women" containing height and weight of 15 women

simple_linear_model <- lm(weight ~ height, data = women)

simple_linear_model
women

# weight = -87.52 + 3.45 * height

plot(women$height, women$weight, 
     xlab = "Height in inces",
     ylab = "Weight in pound",
     main = "Scatter plot showing regression ine for weight predicted from height")

abline(simple_linear_model)

summary(simple_linear_model)

# residuals provide a quick view of the distribution of the residuals which by definition have 
# a mean of zero.   Therefore the median should not be far from zero and the min and max
# should be roughly equal to absolute value
# Residual standard error (RSE), R^2 and f-statistics are metrics that 
# are used to check how the model fits the data

# The standard error (SE) defines the accuracy of the beta coefficients 
# For a given beta cooefficient the SE refelcts how the coefficient varies under repeat sampling
# It can be used to compute the Confidence Interval (CI) and the t-statistic 

# The correlation coefficent measures the level of association between 2 variables
# and ranges from -1 (perfect negative correlation) to +1 (perfect positive correlation)
# value close to zero equals a weak relationship or no relationship
# A low correlation is between -0.2 and .2 - indicates that a lot of the variation 
# of the outcome (y) against the predictor (x) is unexplained and
# we should then look for better predictor variables

cor(women$height, women$weight)

confint(simple_linear_model)


library(cars)
head(cars)

scatter.smooth(x = cars$speed, y = cars$dist,
               main = "Distances ~ Speed",
               xlab = "Car speed",
               ylab = "Stopping distance")

# Boxplots will show outliers in the data 

par(mfrow = c(1, 2))  # divide graph area into 2 columns

boxplot(cars$speed, main = "Speed",
        sub = paste("Outlier rows:", 
                    boxplot.stats(cars$speed)$out))

boxplot(cars$dist, main = "Distance",
        sub = paste("Outlier rows:", 
                    boxplot.stats(cars$dist)$out))

# Skewness function to examine the normality

install.packages("e1071")
library(e1071)

# Density plot for speed
# Skewness <-1 or > 1 means highly skewed
# -1 to -0.5 and 0.5 to 1 means moderate skewness
# -0.5 to 0.5 means approximate symetric

plot(density(cars$speed), main = "Density plot : speed",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(cars$speed), 2)))

# Fill the area within the density plot to red

polygon(density(cars$speed), col = "red")

plot(density(cars$dist), main = "Density plot : Distance",
     ylab = "Frequency",
     sub = paste("Skewness : ", round(e1071::skewness(cars$dist), 2)))

# Fill the area within the density plot to red

polygon(density(cars$dist), col = "blue")

cor(cars$speed, cars$dist)

# Build the model on full data 
linear_model <- lm(dist ~ speed, data = cars)
linear_model
summary(linear_model)

# this command sets the randomness of the logic - in this case to the 200 value
# will allow the same random values to be regenerated each time
# you would remove this after you confirmed your logic

set.seed(200)

# choose a random sample from 1:all records in cars data set
# 80% of rows 

random_sample <- sample(1:nrow(cars), 0.8 * nrow(cars))
# model training data with 80% of the data
training_data <- cars[random_sample, ]
# model test data with 20% of the data
testing_data <- cars[-random_sample, ]

nrow(cars)
nrow(training_data)
nrow(testing_data)

# Build the model on training data 

lr_model <- lm(dist ~ dist, data = training_data)

summary(lr_model)


distance_predicted <- predict(lr_model, testing_data)
distance_predicted

actuals_predicted <- data.frame(cbind(actuals = testing_data$dist,
                                      predicted = distance_predicted))
actuals_predicted

correlation_accuracy <- cor(actuals_predicted)
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_predicted, 1, min)
                         / apply(actuals_predicted, 1, max))

min_max_accuracy
# MAPE

mape <- mean(abs((actuals_predicted$predicted - actuals_predicted$actuals)) /
               actuals_predicted$actuals)
mape

# Use the state.x77 dataset we will explore the relathionship between murder
# rate and other characteristics including population illeteracy, income and frost

# first step in multiple regression is to examinethe relationships among the variables
# two at a time
# The scatter plot matrix uses the car package

states <- as.data.frame((state.x77[, c("Murder", "Population", "Illiteracy", "Income", "Frost")]))

head(states)

# Useful command to compare relationships between values
cor(states)

library(car)
scatterplotMatrix(states, spread = FALSE, smoother.args = list(lty = 2), 
                  main = "Scatter plot matrix")




