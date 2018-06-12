#DBST667 Week 4 Exercise
#Brandon Russell

#Install and load necessary packages
install.packages("party")
library("party")

#Read the CreditApproval data
credit<-read.csv(file="CreditApproval.csv", head=TRUE, sep=",")
newcredit<-read.csv(file="newcredit.csv", head=TRUE, sep=",")
#Observe credit structure
str(credit) 
str(newcredit)
#Data pre-processing
#Fix "A1" attribute name in newcredit
colnames(newcredit)[1] <- "A1"
#Factor class attribute in newcredit
newcredit$class<-factor(newcredit$class)
#Convert int into num type to match credit
newcredit$A14<-as.numeric(newcredit$A14)
#Replace missing values with mean value
apply(newcredit, 2, function (newcredit) sum(is.na(newcredit)))
apply(credit, 2, function (credit) sum(is.na(credit)))
credit$A2[is.na(credit$A2)]<-mean(credit$A2, na.rm=TRUE)
credit$A14[is.na(credit$A14)]<-mean(credit$A14, na.rm=TRUE)
apply(credit, 2, function (credit) sum(is.na(credit)))

#Split data between training and test sets
set.seed(1234)
ind <- sample(2, nrow(credit), replace = TRUE, prob = c(0.7, 0.3))
train.data <- credit[ind == 1, ]
test.data <- credit[ind == 2, ]

#Run method on credit training data
myFormula<-class~.
credit_ctree <- ctree(myFormula, data = train.data)

#Print credit tree structure
print(credit_ctree) 

#Visualize credit tree
nodes(credit_ctree, 2)
plot(credit_ctree)
plot(credit_ctree, type="simple")

# Generate a confusion matrix
table(predict(credit_ctree), train.data$class)
prop.table(table(predict(credit_ctree), train.data$class))

#Evaluate the credit model on a test data
testPred <- predict(credit_ctree, newdata = test.data)
table (testPred, test.data$class)

#Predict class values in newcredit
newcreditPred <- predict(credit_ctree, newdata = newcredit)
table (newcreditPred, newcredit$class)
newcreditPred

#Adjust levels in newcredit$ class
levels <- levels(newcredit$class)
levels[length(levels) + 1] <- "+"
levels[length(levels) + 1] <- "-"
levels
#Refactor newcredit$class to include "+" and "-" as a factor level
newcredit$class <- factor(newcredit$class, levels = levels)
#Run loop to replace values in newcredit with predicted values
for (i in nrow(newcredit)){
  newcredit$class[i] <- newcreditPred[i]
}

