

# to remove objects from workspace
rm(list=ls())

# garbage collection good for more efficent memory storage
gc()


library(lattice)  
test.data <- read.table("test.txt", header=TRUE)
test.data

pig.data <- read.table("pig.txt", header=TRUE)
pig.data

dim(pig.data)
dim(pig.csv)
pig.data["t1"]

# barchart to show the frequency of column t1 in pig.data
barchart(pig.data["t1"],ylab="t1",col="black")
barchart(test.data$prgtype)

#histogram(~pig.data$t9, xlab="t9", )
histogram(~test.data$schtyp|test.data$prgtype, layout = c(1, 3))

#densityplot(~pig.data$t9, groups=pig.data$t2, plot.points=FALSE)
densityplot(~test.data$schtyp|test.data$prgtype, layout = c(1, 3))
densityplot(~test.data$ses|test.data$prgtype, layout = c(1, 3))

#dotplot(~pig.data$t9, groups=pig.data$t2, plot.points=FALSE)
dotplot(~test.data$schtyp|test.data$prgtype, plot.points=FALSE)

xyplot(pig.data$t3~pig.data$t7, col="black")

smoothScatter(pig.data$t7, pig.data$Pig)

boxplot(pig.data$Pig~pig.data$t7, col="Red")
