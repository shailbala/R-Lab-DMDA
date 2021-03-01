# load data
contribution <- read.csv("contribution.csv", header=T)

# Before modeling, split data into 
# two subsets: training (80%) and test (20%).

# for replication
set.seed(1234)
# samples a 1 or a 2 as many times as there are rows 
# with 80% 1's and 20% 2's
ind <- sample(2, nrow(contribution), 
              replace=TRUE, prob=c(0.8, 0.2))

ind

# we use the 80% for training
trainData <- contribution[ind==1,]

# and the 20% for testing
testData <- contribution[ind==2,]

library(party)

myFormula <- Marital.Status ~ FY04Giving + FY03Giving + FY02Giving + 
  FY01Giving + FY00Giving
contri_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(contri_ctree), trainData$Marital.Status)

# printing the rules
print(contri_ctree)

# plotting the tree
plot(contri_ctree)

# simple plot
plot(contri_ctree, type="simple")

# test the classification ability of the test
# tree with the test data
testPred <- predict(contri_ctree, newdata = testData)
table(testPred, testData$Marital.Status)

