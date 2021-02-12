library(lattice)
load( "births2006.smpl.rda")

births.dow=table(births2006.smpl$DOB_WK)
births.dom=table(births2006.smpl$DOB_MM)
births.dop=table(births2006.smpl$DPLURAL)


barchart(dob.dm.tbl,ylab="Day of Week")
barchart(births.dom, main="Bar Chart: Birth Frequency for each Month", xlab="Month", col=c("Violet", "Cyan"), horizontal = FALSE)

histogram(~DBWT|DPLURAL,data=births2006.smpl,
          layout=c(1,5), col="black")

histogram(~DBWT|SEX,data=births2006.smpl,
          layout=c(1,2), col="Pink", xlab = "Weight at the Time of Birth", main = "Weight for each Sex")

densityplot(~DBWT|DPLURAL,data=births2006.smpl,
            layout=c(1,5),plot.points=FALSE, col="green")

densityplot(~DBWT|SEX, data=births2006.smpl,
            layout=c(1,2), plot.points=FALSE, xlab = "Weight at the Time of Birth")

dotplot(~DBWT|SEX,data=births2006.smpl,
        layout=c(1, 2), plot.points=FALSE, xlab = "Weight", col="yellow")

xyplot(DBWT~DMETH_REC, data=births2006.smpl, ylab = "Weight")

xyplot(DBWT~DOB_WK|DMETH_REC,data=births2006.smpl,
       layout=c(1,3), col="violet")

xyplot(DBWT~WTGAIN|SEX,data=births2006.smpl,
       layout=c(1,2), col="red")

smoothScatter(births2006.smpl$WTGAIN,
              births2006.smpl$DBWT)

boxplot(DBWT~APGAR5,data=births2006.smpl,
        ylab="DBWT",xlab="AGPAR5", horizontal = TRUE)

bwplot(DBWT~factor(APGAR5)|factor(SEX),
       data=births2006.smpl,xlab="AGPAR5", col = "Red")
bwplot(DBWT~factor(DOB_WK)|factor(DMETH_REC), data=births2006.smpl,
       xlab="Day of Week")
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

barplot(t4, col = "Cyan", ylab = "Weight", main = "Bar Plot")

barplot(t5,beside=TRUE, ylab="DBWT", horiz = TRUE)

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
levelplot(t6,scales = list(x = list(rot = 90)), col.regions = c("red", "cyan", "pink", "black"))

contourplot(t6,scales = list(x = list(rot = 90)))

