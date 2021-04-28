#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)

# structure of the data
str(mushrooms)
'data.frame': 8124 obs. of 22 variables:
  $ class : Factor w/ 2 levels "e","p": 2 1 1 2 1 1 1 1 2 1 ...
$ cap.shape : Factor w/ 6 levels "b","c","f","k",..: 6 6 1 6 6 6 1 1 6 1 ...
$ cap.surface : Factor w/ 4 levels "f","g","s","y": 3 3 3 4 3 4 3 4 4 3 ...
$ cap.color : Factor w/ 10 levels "b","c","e","g",..: 5 10 9 9 4 10 9 9 9 10 ...
$ bruises : Factor w/ 2 levels "f","t": 2 2 2 2 1 2 2 2 2 2 ...
$ odor : Factor w/ 9 levels "a","c","f","l",..: 7 1 4 7 6 1 1 4 7 1 ...
$ gill.attachment : Factor w/ 2 levels "a","f": 2 2 2 2 2 2 2 2 2 2 ...
$ gill.spacing : Factor w/ 2 levels "c","w": 1 1 1 1 2 1 1 1 1 1 ...
$ gill.size : Factor w/ 2 levels "b","n": 2 1 1 2 1 1 1 1 2 1 ...
$ gill.color : Factor w/ 12 levels "b","e","g","h",..: 5 5 6 6 5 6 3 6 8 3 ...
$ stalk.shape : Factor w/ 2 levels "e","t": 1 1 1 1 2 1 1 1 1 1 ...
$ stalk.root : Factor w/ 5 levels "?","b","c","e",..: 4 3 3 4 4 3 3 3 4 3 ...
$ stalk.surface.above.ring: Factor w/ 4 levels "f","k","s","y": 3 3 3 3 3 3 3 3 3 3 ...
$ stalk.surface.below.ring: Factor w/ 4 levels "f","k","s","y": 3 3 3 3 3 3 3 3 3 3 ...
$ stalk.color.above.ring : Factor w/ 9 levels "b","c","e","g",..: 8 8 8 8 8 8 8 8 8 8 ...
$ stalk.color.below.ring : Factor w/ 9 levels "b","c","e","g",..: 8 8 8 8 8 8 8 8 8 8 ...
$ veil.color : Factor w/ 4 levels "n","o","w","y": 3 3 3 3 3 3 3 3 3 3 ...
$ ring.number : Factor w/ 3 levels "n","o","t": 2 2 2 2 2 2 2 2 2 2 ...
$ ring.type : Factor w/ 5 levels "e","f","l","n",..: 5 5 5 5 1 5 5 5 5 5 ...
$ spore.print.color : Factor w/ 9 levels "b","h","k","n",..: 3 4 4 3 4 3 3 4 3 3 ...
$ population : Factor w/ 6 levels "a","c","n","s",..: 4 3 3 4 1 3 3 4 5 4 ...
$ habitat : Factor w/ 7 levels "d","g","l","m",..: 6 2 4 6 2 2 4 4 2 4 ...

# number of rows with missing values
nrow(mushrooms) - sum(complete.cases(mushrooms))

# deleting redundant variable `veil.type`
mushrooms$veil.type <- NULL

# analyzing the odor variable
table(mushrooms$class,mushrooms$odor)
a&nbsp; &nbsp; &nbsp;&nbsp;c&nbsp; &nbsp; &nbsp; f&nbsp; &nbsp; &nbsp; &nbsp;l&nbsp; &nbsp; &nbsp; &nbsp;m&nbsp; &nbsp; &nbsp; &nbsp;n&nbsp; &nbsp; &nbsp; &nbsp;p&nbsp; &nbsp; &nbsp; &nbsp; s&nbsp; &nbsp; &nbsp; y
e&nbsp; &nbsp;400&nbsp; &nbsp; &nbsp;0&nbsp; &nbsp; &nbsp; 0&nbsp; &nbsp; &nbsp;400&nbsp; &nbsp; &nbsp;0&nbsp; &nbsp; 3408&nbsp; &nbsp; 0&nbsp; &nbsp; &nbsp; &nbsp;0&nbsp; &nbsp; &nbsp; 0
p&nbsp; &nbsp;0&nbsp; &nbsp; &nbsp; 192&nbsp; &nbsp;2160&nbsp; 0&nbsp; &nbsp; &nbsp; 36&nbsp; &nbsp; &nbsp;120&nbsp; &nbsp; 256&nbsp; 576&nbsp; 576

number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})

# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")

#data splicing
set.seed(12345)
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]
# penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)
# building the classification tree with rpart
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")

# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)
#Testing the model
pred <- predict(object=tree,mushrooms_test[-1],type="class")

#Calculating accuracy
t <- table(mushrooms_test$class,pred) > confusionMatrix(t)
Confusion Matrix and Statistics

pred
e&nbsp; &nbsp; &nbsp; p
e&nbsp; 839&nbsp; &nbsp; 0
p&nbsp; 0&nbsp; &nbsp; &nbsp;785

Accuracy : 1
95% CI : (0.9977, 1)
No Information Rate : 0.5166
P-Value [Acc > NIR] : < 2.2e-16

Kappa : 1
Mcnemar's Test P-Value : NA

Sensitivity : 1.0000
Specificity : 1.0000
Pos Pred Value : 1.0000
Neg Pred Value : 1.0000
Prevalence : 0.5166
Detection Rate : 0.5166
Detection Prevalence : 0.5166
Balanced Accuracy : 1.0000

'Positive' Class : e


