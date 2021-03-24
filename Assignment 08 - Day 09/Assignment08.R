## adding the cars data
data("cars")
View(cars)

## plotting the data
plot(cars)

# add a new column speed2
# to store the square of speeds
# for kinetic energy
cars <- transform(cars, speed2 = speed^2)
View(cars)

# correlations:
## cars' breaking distance
## and speed, or speed^2
cor(cars$speed, cars$dist)
cor(cars$speed2, cars$dist)

# build a linear regression model with
# dist as predictor

## model a
## dist ~ b0 + b1(speed) + b2(speed^2) + e
fit_a <- lm(dist ~ speed + speed2, data=cars)
fit_a

## model b
## dist ~ b1(speed) + b2(speed^2) + e
## intercept is suppressed
fit_b <- lm(dist ~ speed + speed2 + 0, data=cars)
fit_b

## model c
## dist ~ b1(speed) + e
## intercept is suppressed
## can also use -1 in palce of +0
fit_c <- lm(dist ~ speed + 0, data=cars)
fit_c

#residuals(fit_a)
#residuals(fit_b)
#residuals(fit_c)

# to run residual and diagnostic plots
#layout(matrix(c(1,2,3,4),2,2)) # 4 graphs per page 
# plot is the lm() method that produces
# diagnostic plots of assumptions
plot(fit_a)
plot(fit_b)
plot(fit_c)
