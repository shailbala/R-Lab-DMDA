load("titanic.raw.rdata")
View(titanic.raw)
str(Titanic)

library(arules)
library(arulesViz)

# find association rules with default settings
rules.all <- apriori(titanic.raw)

rules.all

# find all rules with age=Adult and minimum support and confidence of 0.1 each
rules.age <- apriori(data=titanic.raw, control = list(verbose=F),
              parameter = list(minlen=2, supp=0.1, conf=0.1), appearance = list(lhs=c("Age=Adult"), default="rhs"))
inspect(rules.age)

# find all rules with lhs as sex=Female and minimum support and confidence of 0.1 each
rules.female <- apriori(data=titanic.raw, control = list(verbose=F),
              parameter = list(minlen=2, supp=0.1, conf=0.1), appearance = list(lhs=c("Sex=Female"), default="rhs"))
inspect(rules.female)

# find all rules with lhs as class=1st and minimum support and confidence of 0.1 each
rules.c1 <- apriori(data=titanic.raw, control = list(verbose=F),
              parameter = list(minlen=2, supp=0.1, conf=0.1), appearance = list(lhs=c("Class=1st"), default="rhs"))
inspect(rules.c1)

# rules with rhs containing "Survived" only (survived=Yes)
rules.survived <- apriori(titanic.raw, control = list(verbose=F),
              parameter = list(minlen=2, supp=0.005, conf=0.8),
              appearance = list(rhs=c("Survived=Yes"),
                                default="lhs"))   
inspect(rules.survived)

## sorting all rules by support

rules.adult.sorted<-sort(rules.adult, by="support")
inspect(rules.adult.sorted)

rules.female.sorted<-sort(rules.female, by="support")
inspect(rules.female.sorted)

rules.c1.sorted<-sort(rules.c1,by="support")
inspect(rules.c1.sorted)

rules.survived.sorted<-sort(rules.survived,by="support")
inspect(rules.survived.sorted)

## sorting all rules by confidence

rules.age.sorted <- sort(rules.age, by="confidence")
inspect(rules.age.sorted)

rules.female.sorted <- sort(rules.female, by="confidence")
inspect(rules.female.sorted)

rules.c1.sorted <- sort(rules.c1, by="confidence")
inspect(rules.c1.sorted)

rules.survived.sorted <- sort(rules.survived, by="confidence")
inspect(rules.survived.sorted)

##chi square test
interestMeasure(rules.age, titanic.raw, measure = "chiSquared")
interestMeasure(rules.female, titanic.raw, measure = "chiSquared")
interestMeasure(rules.c1, titanic.raw, measure = "chiSquared")
interestMeasure(rules.survived, titanic.raw, measure = "chiSquared")

##cosine
interestMeasure(rules.adult, titanic.raw, measure = "cosine")
interestMeasure(rules.female, titanic.raw, measure = "cosine")
interestMeasure(rules.c1, titanic.raw, measure = "cosine")
interestMeasure(rules.survived, titanic.raw, measure = "cosine")

## rules for all 2nd class children survived
rules2c <- apriori(titanic.raw,
                 parameter = list(minlen=3, supp=0.002, conf=0.01),
                 appearance = list(lhs=c("Age=Child"), rhs=c("Class=1st", "Class=2nd",
                                                             "Class=Crew"),
                                   default="none"),
                 control = list(verbose=F))
inspect(rules2c)

## Find  all  rules  of  children  of  different  classes

rules.child.class <- apriori(titanic.raw,
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child"),
                                   default="rhs"),
                 control = list(verbose=F))
inspect(rules.child.class)

## plotting all
plot(rules.all, method="grouped")

plot(rules.age)
plot(rules.female)
plot(rules.c1) 
plot(rules.survived)
plot(rules2c)
plot(rules.child.class)
