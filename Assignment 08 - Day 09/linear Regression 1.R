#############################################
#####             Regression            #####
#############################################

# Regression works by building a function of independent
# variables (known as predictors) which predict one
# or more dependent variables (a response). For
# example, banks assess the risk of home-loam appli-
# cants based on their age, income, expenses, occu-
# pation, number of dependents, total credit limit, etc.

# We will look at an example of building a linear
# regression model to predict CPI(Consumer Price Index) data.



# Linear regression predicts a response variable with
# a linear function of predictors:

# y = C0 + c1x1 + c2x2 + . . . + ckxk + e,

# where x1, x2, ... , xk are predictors, y is the
# response to predict and e is the error term.

#  regression with lm() function on the Australian
# CPI data from 2008 to 2010


ls()
# free memory
rm(list = ls())
gc()
?ls()
# create the data for each quarter 2008-2010
year <- rep(2008:2010, each=4)
year

# make quarters explicit ( 1-4 repeats 4 times)
quarter <- rep(1:4, 3)
quarter

# quarterly CPI data as a vector:
cpi <- c(162.2, 164.6, 166.5, 166.0, 
         166.2, 167.0, 168.6, 169.5, 
         171.0, 172.1, 173.3, 174.0)
cpi

# plot the data (suppressing x axis)
plot(cpi, xaxt="n", ylab="CPI", xlab="")
class(cpi)

# draw custom x-axis on bottom ('1'), 'las'
# makes it vertical:
axis(1, labels=paste(year,quarter,sep="Q"), 
     at=1:12, las=3)

# correlations:
cor(year,cpi)
cor(quarter,cpi)

# build a linear regression model with cpi
# as predictor
fit <- lm(cpi ~ year + quarter)
fit
class(fit)
is.list(fit)
str(fit)

# summary() is better
summary(fit)

# So cpi is cpi = c0+c1*year+c2*quarter
# where co = intercept = -7644.4875
# c1 = 3.8875
# c2 = 1.1667

# what are fit$coefficients?
fit$coefficients
fit$coefficients[[1]]
fit$coefficients[[2]]
fit$coefficients[[3]]

# So the cpi for the 4 quarters of 2011 is:
(cpi2011 <- fit$coefficients[[1]] + fit$coefficients[[2]]*2011 +
    fit$coefficients[[3]]*(1:4))

# These commands provide more details of the model
attributes(fit)

# differences between observed values and fitted values
residuals(fit)

# to run residual and diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # 4 graphs per page 
# plot is the lm() method that produces
# diagnostic plots of assumptions
plot(fit)
layout(matrix(1)) # change back to one graph per page 

# scatterplot3d() creates 3D scatter plot
# and plane3d() draws the fitted plane. Parameter
# lab specifies the number of tickmarks on the
# x- and y-axes
library(scatterplot3d)
?scatterplot3d
s3d <- scatterplot3d(year, quarter, cpi, highlight.3d=T, type="h", lab=c(2,3))
s3d$plane3d(fit)

# With the model, the CPis in year 2011 can be
# predicted like this, with the predicted values
# shown as red triangles. First we set up df:
data2011 <- data.frame(year=2011, quarter=1:4)
data2011
# predict with regression fit
cpi2011 <- predict(fit, newdata=data2011)
cpi2011

# vector with '1' 12 times and '2' 4 times
style <- c(rep(1,12), rep(2,4))
# will vary plotting character and color with style
style

# suppress the x axis, define both plotting character
# and color by the 1's and 2's above
plot(c(cpi, cpi2011), xaxt="n", 
     ylab="CPI", xlab="", pch=style, col=style)
# create custom x-axis
axis(1, at=1:16, las=3,
     # make x-axis labels on the fly
     labels=c(paste(year,quarter,sep="Q"), 
              "2011Q1", "2011Q2", "2011Q3", "2011Q4"))

