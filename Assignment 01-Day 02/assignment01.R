
# Read numeric data in to a variable 'x'
# using the R function scan()
x <- scan()
x

# What is the mode of x?
mode(x)

# Use scan() to read in the following string
# (character or text) data into a variable 'y'
y <- scan(what=" ")
y

# What is the mode of y?
mode(y)

## Read in the file 'pig.txt' (in the book zipped data files)
## and assign it to the variable 'pig.data' as a dataframe
## R data structure.
pig.data <- read.table("pig.txt", header=TRUE)
pig.data

## Read in the file 'piedata.csv' and assign it
## to the variable 'pie.data' as a dataframe.
pie.data <- read.csv("piedata.csv", header=TRUE)
pie.data

## Write the file 'pig.data' back into the
## 'c:\temp' directory as 'pig.txt' (not 'pig.csv')
write.table(pig.data, "pig.txt")
