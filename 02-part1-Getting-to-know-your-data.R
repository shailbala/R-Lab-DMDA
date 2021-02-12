###########################################
#####    Getting to Know your Data    #####
###########################################

# to remove objects from workspace
rm(list=ls())

# garbage collection good for more efficent memory storage
gc()

### PRE-PROCESSING THE INFORMATION AND GETTING TO 
### KNOW YOUR DATA



# Example 1: 2006 Birth Data Is 2006 birth data set that 
# is used in the book R In a Nutshell: A Desktop Quick
# Reference (Adler, 2009). The data set births2006.smpl 
# consists of 427,323 records and 13 variables, including 
# the day of birth according to the month and the day of
# week (DOB_MM, DOB_WK), the birth weight of the baby 
# (DBWT) and the weight gain of the mother during pregnancy
# (WTGAIN), the sex of the baby and its APGAR score at 
# birth (SEX and APGAR5), whether it was a single or
# multiple birth (DPLURAL), and the estimated gestation
# age in weeks (ESTGEST).

# Information on first five births:
## Install packages from CRAN; use any USA mirror.
# We use trellis graphics and lattice to condition density
# histograms on values of a third variable (multiple births,
# and method of delivery). We have separate histograms
# for birth weight by these variables.

library(lattice)  
library(nutshell) 
births2006.smpl

births2006.smpl[1:5,]
# Shows number of births and number of variables,
# that is, number of rows and columns in the data:
dim(births2006.smpl)

# data for bar charts of frequency of births:
births.dow=table(births2006.smpl$DOB_WK)
births.dom=table(births2006.smpl$DOB_MM)
births.dop=table(births2006.smpl$DPLURAL)

births.dow 
births.dom
births.dop


?barchart
barchart(births.dow,ylab="Day of Week")
## for color, use col="red" or omit the color argument
dob.dm.tbl=table(WK=births2006.smpl$DOB_WK,
                 MM=births2006.smpl$DMETH_REC)
dob.dm.tbl
dob.dm.tbl=dob.dm.tbl[,-3]
dob.dm.tbl=dob.dm.tbl[,-2]
dob.dm.tbl
#trellis.device()
barchart(dob.dm.tbl,ylab="Day of Week")
barchart(births.dom, main="Bar Chart: Birth Frequency for each Month", xlab="Month", col=c("Violet", "Cyan"), horizontal = FALSE)

# separate histograms:
histogram(~DBWT|DPLURAL,data=births2006.smpl,
          layout=c(1,5), col="black")

histogram(~DBWT|DMETH_REC,data=births2006.smpl,
          layout=c(1,3))#,col="black")

# birth weight decreases with multiple births, but birth
# weight is largely unaffected by the method of delivery.

# Smoothed versions of the histograms, using the
# lattice command density plot, are also shown. 
densityplot(~DBWT|DPLURAL,data=births2006.smpl,
            layout=c(1,5),plot.points=FALSE)#,col="black")
densityplot(~DBWT,groups=DPLURAL,data=births2006.smpl,
            plot.points=FALSE)

# Because of the very small sample sizes for quintuplet
# and even more births, the density of birth weight for
# this small group is quite noisy.

# Dot plot shows quite clearly that there are only few
# observations in that last group, while most other 
# groups have many observations:
dotplot(~DBWT|DPLURAL,data=births2006.smpl,
        layout=c(1,5),plot.points=FALSE)#,col="black")

# Scatter plots are shown for birth weight against
# weight gain, and the scatter plots are stratified 
# further by multiple births.

# birth weight by day of the week:
xyplot(DBWT~DOB_WK,data=births2006.smpl)#,col="black")

# here is conditioned on plurality of births:
xyplot(DBWT~DOB_WK|DPLURAL,data=births2006.smpl,
       layout=c(1,5))#,col="black")

# birth weight by weight gain:
xyplot(DBWT~WTGAIN,data=births2006.smpl)#,col="black")

# conditioned on plurality:
xyplot(DBWT~WTGAIN|DPLURAL,data=births2006.smpl,
       layout=c(1,5))#,col="black")

# The last smoothed scatter plot shows there is little 
# association between birth weight and weight gain
# during the course of the pregnancy.
smoothScatter(births2006.smpl$WTGAIN,
              births2006.smpl$DBWT)

# We show box plots of birth weight against the APGAR 
# score and box plots of birth weight against the day 
# of week of delivery. We do not expect a relationship
# between the birth weight and the day of week of
# delivery. 

# The APGAR score is an indication of the health status
# of a newborn, with low scores indicating that the 
# newborn has problems.

# The box plot of birth weight against the APGAR score
# shows a strong relationship. Babies of low birth 
# weight often have low APGAR scores as their health
# is compromised by the low birth weight and its 
# associated complications.

## boxplot is the command for a box plot in standard graphics
## package
boxplot(DBWT~APGAR5,data=births2006.smpl,
        ylab="DBWT",xlab="AGPAR5")
boxplot(DBWT~DOB_WK,data=births2006.smpl,
        ylab="DBWT",xlab="Day of Week")

## bwplot is the command for a box plot in the lattice graphics
## package. There you need to declare the conditioning variables as 
## factors 
bwplot(DBWT~factor(APGAR5)|factor(SEX),
       data=births2006.smpl,xlab="AGPAR5")
bwplot(DBWT~factor(DOB_WK),data=births2006.smpl,
       xlab="Day of Week")

# We calculate the average birth weight as function of 
# multiple births, and we do this for males and females
# separately. For that we use the tapply function.

# Note missing observations in the data set and the 
# option na.rm=TRUE (means remove missing observations 
# from the calculation) is needed to omit the missing
# observations from the calculation of the mean. 

# The bar plot illustrates graphically how average birth
# weight decreases with multiple deliveries. It also 
# shows that average birth weight for males is slightly
# higher than that for females.
fac=factor(births2006.smpl$DPLURAL)
res=births2006.smpl$DBWT
t4=tapply(res,fac,mean,na.rm=TRUE)
t4
t5=tapply(births2006.smpl$DBWT,
          INDEX=list(births2006.smpl$DPLURAL,
                     births2006.smpl$SEX),
          FUN=mean,na.rm=TRUE)
t5
barplot(t4,ylab="DBWT")
barplot(t5,beside=TRUE,ylab="DBWT")

# Here we illustrate the levelplot and the contourplot 
# from the R package lattice. For these plots we first 
# create a cross-classification of weight gain and 
# estimated gestation period by dividing the two continuous 
# variables into 11 nonoverlapping groups.

# For each of the resulting groups, we compute average 
# birth weight. An earlier frequency distribution table
# of estimated gestation period indicates that "99" is 
# used as the code for "unknown". For the subsequent 
# calculations, we omit all records with unknown gestation
# period (i.e., value 99). The graphs show that birth weight 
# increases with the estimated gestation period, but 
# that birth weight is little affected by the weight 
# gain. Note that the contour lines are essentially
# horizontal and that their associated values increase 
# with the estimated gestation period.
t5=table(births2006.smpl$ESTGEST)
t5
new=births2006.smpl[births2006.smpl$ESTGEST != 99,]
t51=table(new$ESTGEST)
t51
t6=tapply(new$DBWT,
          INDEX=list(cut(new$WTGAIN,breaks=10),
                     cut(new$ESTGEST,breaks=10)),
          FUN=mean,na.rm=TRUE)
t6
levelplot(t6,scales = list(x = list(rot = 90)))
contourplot(t6,scales = list(x = list(rot = 90)))

