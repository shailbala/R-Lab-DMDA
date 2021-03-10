library(arules)
library(arulesViz)
# find association rules with default settings
rules.all <- apriori(titanic.raw)
quality(rules.all) <- round(quality(rules.all), digits=3)
rules.all
inspect(rules.all)
## use code below if above code does not work
arules::inspect(rules.all)

rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No"),
                                   default="lhs"))
plot(rules)
# rules with rhs containing "Survived" only#
rules <- apriori(titanic.raw, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=Yes"),
                                   default="lhs"))
inspect(rules)
plot(rules)
# Removing redundancy
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)
plot(rules.sorted)
#interpresting Rules
rules <- apriori(titanic.raw,
                 parameter = list(minlen=3, supp=0.002, conf=0.2),
                 appearance = list(rhs=c("Survived=Yes"),
                                   lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                         "Age=Child", "Age=Adult"),
                                   default="none"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)
plot(rules.sorted)
plot(rules.sorted, method="grouped")

plot(rules.all)
plot(rules.all, method="grouped")
