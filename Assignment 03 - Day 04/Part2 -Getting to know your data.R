### Example 2: Alumni Donations

# The file contribution.csv received by a selective private 
# liberal arts college in the Midwest. The college keeps 
# detailed records on alumni donations. We analyze the 
# contributions of five graduating classes (cohorts who 
# graduated in 1957, 1967, 1977, 1987, and 1997). The data
# consists of n = 1230 living alumni and contains their 
# contributions for the years 2000-2004. 

# Also, the data set includes several other variables such
# as gender, marital status, college major, subsequent 
# graduate work, and attendance at fund-raising events, 
# all variables that may play an important role in 
# assessing the success of future capital campaigns.

# The data contains no missing observations. The first 
# five records of the file are shown below. Alumni not 
# contributing have the entry "0" in the related column. 
# The 1957 cohort is the smallest group. This is because 
# of smaller class sizes in the past and deaths of
# older alumni.

## Install packages from CRAN; use any USA mirror 
library(lattice)
# "drill down" to the file in your stored course materials:
don <- read.csv(file.choose())
dim(don)
# view first five records:
don[1:5,]
# this is a tabulate function, it counts frequencies:
table(don$Class.Year)
# is an integer column but table() sees the integers
# as factors
class(don$Class.Year)
# create a barchart
barchart(table(don$Class.Year),horizontal=FALSE,
         xlab="Class Year",col="black")

# Total contributions for 2000-2004 are calculated for 
# each graduate. Summary statistics (mean, standard 
# deviation, and percentiles) are shown below. More than
# 30% of the alumni gave nothing; 90% gave $1050 or less; 
# and only 3% gave more than $5000. 

# The largest contribution was $172,000. The first histogram
# of total contributions shown below is not very informative
# as it is influenced by both a sizable number of the 
# alumni who have not contributed at all and a few alumni 
# who have given very large contributions. Omitting
# contributions that are zero or larger than $1000 provides 
# a more detailed view of contributions in the $1-$1000 
# range; this histogram is shown to the right of the
# first one. Box plots of total contributions are also 
# shown. The second box plot omits the information from
# outliers and shows the three quartiles of the distribution
# of total contributions (0, 75, and 400).

# add up the donations
don$TGiving=don$FY00Giving+don$FY01Giving+don$FY02Giving+don$FY03Giving+don$FY04Giving
# take a look, note it was adding columns:
don$TGiving
# take mean of total giving, is vectorized
# function so we get a single number
mean(don$TGiving)
median(don$TGiving)
max(don$TGiving)
sum(don$TGiving)
# compute standard deviation of total giving,
# is very large, why is this?
sd(don$TGiving)
# quantile() provides a variety of summary information
# in 5% increments:
quantile(don$TGiving,probs=seq(0,1,0.05))
# in 1% increments starting at 95%....we see that
# one huge donation at the end
quantile(don$TGiving,probs=seq(0.95,1,0.01))
quantile(don$TGiving,probs=seq(0,1,0.25))
# finally, draw a histogram, is distorted because
# of large donation
hist(don$TGiving)
# look at donations up to $1,000 max:
hist(don$TGiving[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=1000])
hist(don$TGiving[don$TGiving!=0][don$TGiving[don$TGiving!=0]<=5000])

## or, if you want to achieve the above histogram slower in two steps
ff1=don$TGiving[don$TGiving!=0]
 hist(ff1)
 ff2=ff1[ff1<=1000]
 hist(ff2)
## hist(ff2,main=paste("Histogram of TGivingTrunc"),xlab="TGivingTrunc")

boxplot(don$TGiving,horizontal=TRUE,
        xlab="Total Contribution")
boxplot(don$TGiving,outline=FALSE,
        horizontal=TRUE,xlab="Total Contribution")

# We identify below the donors who gave at least $30,000 
# during 2000-2004. We also list their major and their 
# next degree. The top donor has a mathematics-physics
# double major with no advanced degree. Four of the top 
# donors have law degrees.

ddd=don[don$TGiving>=30000,]
ddd
# selectively list columns 1 thru 5, and 12
ddd1=ddd[,c(1:5,12)]
ddd1
# put them in order, from highest to lowest
ddd1[order(ddd1$TGiving,decreasing=TRUE),]

# For a university foundation, it is important to know 
# who is contributing, as such information allows the 
# foundation to target their fund-raising resources to 
# those alumni who are most likely to donate. We show 
# below box plots of total 5-year donation for the 
# categories of class year, gender, marital status, 
# and attendance at a foundation event. 

# We have omitted in these graphs the outlying 
# observations (those donors who contribute generously).
# Targeting one's effort to high contributors involves 
# many personal characteristics that are not included 
# in this database (such as special information about
# personal income and allegiance to the college). 

# It may be a safer bet to look at the median amount 
# of donation that can be achieved from the various 
# groups. Class year certainly matters greatly;
# older alumni have access to higher life earnings, 
# while more recent graduates may not have the resources
# to contribute generously. Attendance at a foundation
# sponsored event certainly helps; this shows that it 
# is important to get alumni to attend such events. 

# This finding reminds the author about findings in 
# his consulting work with credit card companies: if 
# one wants someone to sign up for a credit card,
# card, one must first get that person to open up 
# the envelope and read the advertising message.


# We provide box plots of total giving against the 
# alumni's major and second degree. In these, we only
# consider those categories with frequencies exceeding
# a certain threshold (10); otherwise, we would have 
# to look at the information from too many groups
# with low frequencies of occurrence. Alumni with an
# economics/business major contribute most. Among 
# alumni with a second degree, MBAs and lawyers give
# the most.

boxplot(TGiving~Class.Year,data=don,outline=FALSE)
boxplot(TGiving~Gender,data=don,outline=FALSE)
boxplot(TGiving~Marital.Status,data=don,outline=FALSE)
boxplot(TGiving~AttendenceEvent,data=don,outline=FALSE)

# get mean by major:
?tapply
t4=tapply(don$TGiving,don$Major,mean,na.rm=TRUE)
## total contribution by each major
t4
#barchart(t4)
# tabulate by major
## frequency for each major
t5=table(don$Major)
t5
#barchart(t5)
# combine those two columns
## total contribution by frequency of each major
t6=cbind(t4,t5)
t6
## contribution done per major
#barchart(t6)
# get t6's where t6 in second column is more than 10
## where frequency of each major > 10 (# of students in that major > 10)
t7=t6[t6[,2]>10,]
t7
# put them in decreasing order
t7[order(t7[,1],decreasing=TRUE),]
# make a barchart out of this information
barchart(t7[,1],col="black")
# get mean of giving by those who have next degree
t4=tapply(don$TGiving,don$Next.Degree,mean,na.rm=TRUE)
# take a look
t4
# tabulate it
t5=table(don$Next.Degree)
# take a look
t5
# bind these two columns
t6=cbind(t4,t5)
# take a look
t6
# select by those who gave more than 10 times
t7=t6[t6[,2]>10,]
# put in decreasing order
t7 <- t7[order(t7[,1],decreasing=TRUE),]
# make a barchart
barchart(t7[,1],col="black")

# A plot of histogram densities, stratified according
# to year of graduation, shows the distributions of 
# 5-year giving among alumni who gave $1-$1000. 

# It gives a more detailed description of the 
# distribution than the earlier histogram of all
# contributions.

densityplot(~TGiving|factor(Class.Year),
            data=don[don$TGiving<=1000,][don[don$TGiving<=1000,]$TGiving>0,],
            plot.points=FALSE,col="black")

# We now calculate the total of the 5-year donations 
# for the five graduation cohorts. We do this by using 
# the tapply function (applying the summation function
# to the total contributions of each of the graduation 
# classes). The result shows that the 1957 cohort has 
# contributed $560,000, compared to $35,000 of the 1997
# cohort.

t11=tapply(don$TGiving,don$Class.Year,
           FUN=sum,na.rm=TRUE)
t11
barplot(t11,ylab="Average Donation")

# Below we calculate the annual contributions (2000-2004)
# of the five graduation classes. The 5 bar charts are 
# drawn on the same scale to facilitate ready comparisons.
# The year 2001 was the best because of some very large
# contributions from the 1957 cohort.

barchart(tapply(don$FY04Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal=FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY03Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY02Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY01Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")
barchart(tapply(don$FY00Giving,don$Class.Year,
                FUN=sum,na.rm=TRUE),horizontal= FALSE,ylim=c(0,225000),col="black")

# Finally, we compute the numbers and proportions 
# of individuals who contributed.

# We do this by first creating an indicator variable 
# for total giving, and displaying the numbers of the 
# alumni who did and did not contribute. About 66%
# of all alumni contribute. The mosaic plot shows that
# the 1957 cohort has the largest proportion of 
# contributors; the 1997 cohort has the smallest 
# proportion of contributors, but includes the 
# largest number of individuals (the area of the bar
# in a mosaic plot expresses the size of the group). 

# The proportions of contributors shown indicate 
# that 75% of the 1957 cohort contributes, while 
# only 61% of the 1997 graduating class does so. 

# We can do the same analysis for each of the
# 5 years (2000-2004). The results for the most 
# recent year 2004 are also shown.

don$TGivingIND=cut(don$TGiving,
                   c(-1,0.5,10000000),
                   labels=FALSE)-1
# take a look
don$TGivingIND
# calculate mean
mean(don$TGivingIND)
# tabulate it
t5=table(don$TGivingIND,don$Class.Year)
# take a look
t5 

barplot(t5,beside=TRUE)
mosaicplot(factor(don$Class.Year)~factor(don$TGivingIND))
t50=tapply(don$TGivingIND,don$Class.Year,
           FUN=mean,na.rm=TRUE)
t50
barchart(t50,horizontal=FALSE,col="black")

don$FY04GivingIND=cut(don$FY04Giving,
                      c(-1,0.5,10000000),
                      labels=FALSE)-1
t51=tapply(don$FY04GivingIND,don$Class.Year,
           FUN=mean,na.rm=TRUE)
t51
barchart(t51,horizontal=FALSE,col="black")

# Below we explore the relationship between the alumni 
# contributions among the 5 years. For example, if we 
# know the amount an alumnus gives in one year
# (say in year 2000) does this give us information 
# about how much that person will give in 2001? 

# Pairwise correlations and scatter plots show that 
# donations in different years are closely related. 

# We use the command plotcorr in the package
# ellipse to express the strength of the correlation 
# through ellipse-like confidence regions.

Data=data.frame(don$FY04Giving,don$FY03Giving,
                don$FY02Giving,don$FY01Giving,
                don$FY00Giving)
correlation=cor(Data)
correlation
plot(Data)

data2 = data.frame(don$FY04Giving, don$FY03Giving)
correlation = cor(data2)
correlation
plot(data2)

library(ellipse)  
plotcorr(correlation)





