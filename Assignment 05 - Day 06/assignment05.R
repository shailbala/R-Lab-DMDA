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
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width



# Random Forest on Iris Data
###############################################
#######        Random Forest             ######
###############################################

### Now we model a random forest

# we load randomForest
library(randomForest)

# we train randomForest
# predict species using all variables

# two limitations of randomForest function:
# 1) cannot handle data with missing values so
#    users must impute values for those missing
# 2) 32 maximum number of levels for each
#    categorical variable; must transform them

# build same model as before; are predicting
# Species based on all other variables:

rf <- randomForest(Species ~ ., 
                   data=trainData, 
                   ntree=100,
                   proximity=TRUE)
#print(rf)

# show conditional table cells' frequencies
table(predict(rf), trainData$Species)

# we take a look at the results
print(rf)

# show the components of the output 'rf' object
attributes(rf)

# plot error rates with various
# number of trees
plot(rf)

# find importance of variables
importance(rf)

# plots importance
varImpPlot(rf)

# test random forest using test data
irisPred <- predict(rf, newdata=testData)

# check the results
table(irisPred, testData$Species)

# check margins of data which is
# proportion of votes for the correct
# class minus maximum proportion of
# votes for other classes. Positive
# margin indicates correct classification

plot(margin(rf, testData$Species))


# Decision Tree on Iris Data
library(party)
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

# However, ctree() does not handle missing values well, and
# another problem is that, when a variable exists in training 
# data and is fed into ctree() but does not appear in the
# built decision tree, the test data must also have that 
# variable to make prediction.

# Otherwise, a call to predict() would fail. Also, if the value 
# levels of a categorical variable in test data are different
# from that in training data, it would also fail to make prediction
# on the test data.

library("rpart")

class(iris$Species)

# look at dimensions (150 rows, 5 variables)
dim(iris)

# look at attributes
attributes (iris)

# train a decision tree
iris_rpart <- rpart(myFormula, data = trainData,
                    # use control arg to restrict # of obs
                    # for potential binary split to 10:
                    control = rpart.control(minsplit = 10))
attributes(iris_rpart)

print(iris_rpart$cptable)

print(iris_rpart)

rpart_pred <- predict(iris_rpart, newdata=testData, type="class")
table(rpart_pred, testData$Species)

# The build tree can be plotted:
plot(iris_rpart)
# adds the text
text(iris_rpart, use.n=T)

# We prune the tree...
# Select the tree with the minimum prediction error:
opt <- which.min(iris_rpart$cptable[,"xerror"])
cp <- iris_rpart$cptable[opt, "CP"]
iris_prune <- prune(iris_rpart, cp = cp)
print(iris_prune)

# Then plot it:
plot(iris_prune)
text(iris_prune, use.n=T)

# Then, the selected tree is used to make prediction 
# and the predicted values are compared with actual
# labels. In the code below, function abline() draws
# a diagonal line. The predictions of a good model
# are expected to be equal or very close to their 
# actual values, that is, most points should be on
# or close to the diagonal line.

Species_pred <- predict(iris_prune, 
                        newdata=testData)#, type="class")
#print(Species_pred)
table(Species_pred, testData$Species)
#xlim <- range(iris$Species)
#plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed",
#ylab="Predicted", ylim=xlim, xlim=xlim)
#abline(a=0, b=1)


## comparison

## random forest
irisPred
table(irisPred, testData$Species)

## ctree
testPred
table(testPred, testData$Species)

## rpart
rpart_pred
table(rpart_pred, testData$Species)

## rpart best pruned tree
Species_pred
table(Species_pred, testData$Species)
