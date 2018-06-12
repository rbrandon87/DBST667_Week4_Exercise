#install the e1071 package.  Only once.
install.packages("e1071")

#load the arules and e1071 library into memory
#need to do this each time you start the new R session
library("arules")
library("e1071")

#Read the data into a data frame
Bank <- read.csv("Bank.csv")
#remove the unique identifier
Bank$id<-NULL
#discretize the age variable
Bank$age<-discretize(Bank$age, "frequency", breaks=6)
summary(Bank$age)
#discretize the income variable
Bank$income<-discretize(Bank$income, "frequency", breaks=6)
summary(Bank$income)
#convert the number of children to a factor
Bank$children<-factor(Bank$children)
summary(Bank)

#make sure that the result is reproducible
set.seed(1234)

#split the data into a training and test set
ind <- sample(2, nrow(Bank), replace = TRUE, prob = c(0.7, 0.3))
train.data <- Bank[ind == 1, ]
test.data <- Bank[ind == 2, ]

#build the model and store in a variable model
model<-naiveBayes(pep~., train.data)
#output the model
model

#confusion matrix for the training set; need to round the estimated values
table(predict(model, train.data), train.data$pep)
#confusion matrix for the test data
table(predict(model, test.data), test.data$pep)

#mosaic plot
mosaicplot(table(predict(model, test.data), test.data$pep), shade=TRUE, main="Predicted vs. Actual PEP")
