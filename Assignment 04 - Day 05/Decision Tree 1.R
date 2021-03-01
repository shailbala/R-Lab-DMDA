#############################################
#####   Introduction to Decision Trees  #####
#####         and Random Forest         #####
#############################################

# We show how to build predictive models with
# packages party, rpart and randomForest. Other
# related packages are partykit and ipred.

# This section shows how to build a decision tree 
# for the iris data with function ctree() in package
# party [Hothorn et al., 2010]. Sepal.Length, 
# Sepal.Width, Petal.Length and Petal.Width are used
# to predict the Species of Iris flowers.

# We first use function ctree() to build  a decision 
# tree, and predict() makes prediction for new data.

data(iris)
str(iris)
iris

# Before modeling, the iris data is split below into 
# two subsets: training (70%) and test (30%).

# for replication
set.seed(1234)
# samples a 1 or a 2 as many times as there are rows 
# in iris with 70% 1's and 30% 2's
ind <- sample(2, nrow(iris), 
              replace=TRUE, prob=c(0.7, 0.3))

ind

# we use the 70% for training
trainData <- iris[ind==1,]

# and the 30% for testing
testData <- iris[ind==2,]

# We then load package party, build a decision tree,
# and check the prediction result. Function ctree()
# provides some parameters, such as MinSplit, MinBucket,
# MaxSurrogate and MaxDepth (all part of the controls =
# ctree_control() argument), to control the training 
# of decision trees. Below we use default settings to 
# build a decision tree.

# In this code, myFormula specifies that Species is the
# target variable and all other variables are independent
# variables.

library(party)

?ctree

myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

# check the prediction
table(predict(iris_ctree), trainData$Species)

# Now we have a look at the tree by printing the rules
print(iris_ctree)

# and plotting the tree
plot(iris_ctree)

# in the above plot, the barplot for each leaf node shows
# the probabilities of an instance falling into the three
# species. In the simple plot below, they are shown as "y"
# in leaf nodes. For example, node 2 is labeled with 
# "n=40, y=(1, 0, 0)", which means that it contains 40 
# training instances and all of them belong to the first
# class "setosa".
?plot
# here is a simple plot:
plot(iris_ctree, type="simple")

# so now we test the classification ability of the test
# tree with the test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)
