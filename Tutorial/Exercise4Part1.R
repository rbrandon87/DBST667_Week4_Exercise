#0. install the package (only once)
#1. Load the packsge into memory (each time you start the new session)
install.packages("party")
library("party", lib.loc="~/R/win-library/3.3")

#2. set the working directory.  Change the folder path to the file location on your PC.
setwd("E:/Datasets")

#3. Read the cars data
cars<-read.csv(file="cars.csv", head=TRUE, sep=",")
#preview the structure
str(cars) 

#4. Data pre-processing
# a. Remove the unique identifier
cars$Id<-NULL

# b. convert the dependent variable symboling to a factor
cars$symboling<-factor(cars$symboling)

#5. split the data into a training and test set
set.seed(1234)
ind <- sample(2, nrow(cars), replace = TRUE, prob = c(0.7, 0.3))
train.data <- cars[ind == 1, ]
test.data <- cars[ind == 2, ]

#6. Run the method on a training data
myFormula<-symboling~.
cars_ctree <- ctree(myFormula, data = train.data)

#7. output the tree structure
print(cars_ctree) 

#8. visualize the tree
nodes(cars_ctree, 2)
plot(cars_ctree)
plot(cars_ctree, type="simple")

#9. confusion matrix
table(predict(cars_ctree), train.data$symboling)
prop.table(table(predict(cars_ctree), train.data$symboling))

#10. Evaluate the model on a test data
testPred <- predict(cars_ctree, newdata = test.data)
table (testPred, test.data$symboling)

