#Tree Model
#1 Cart Model(Classification and regresion tree)[gini Index]<- Random Forest
#2 ID3 Model(Iterative dictomiser 3)[Entropy & Information gain]<- Decesion tree

#Decision tree - is used to prepare a model to predict dependent variable
#using independent.
#Here Dependent will be binary and independent variable which have 
#only two values (can be any value (Numeric or categorical)

#Steps of performing decesion tree
#1.load package rpart, rattle, rcolorbrewer
#2.load data
#3.split data into train (75%) and test(25%)
#4.build model using trian data

#validate model using test data
#As my dependent variable us classification, so we have to use confusion matrix method to find accuracy of model.

#Decision tree using rpart package
install.packages("rattle")
install.packages("rpart.plot")

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

readingSkills<-read.csv("C:/Users/Mohit/Desktop/Nikhil Analytics Material/R Material/Advance R/R-class Machine Learning/Decision Tree/readingskills.csv")
str(readingSkills)
View(readingSkills)
readingskills1<-readingSkills[-1]

train<-readingskills1[1:150,];train
test<-readingskills1[151:200,];test

sapply(readingskills1,function(x) sum(is.na(x)))

tree<- rpart(nativeSpeaker ~ age + shoeSize + score,
             data=train,
             method="class"
)
tree

fancyRpartPlot(tree)
printcp(tree)
# Predict the values of the test set: pred
pred <- predict(tree, test, type = "class")
pred
# Construct the confusion matrix: conf
conf <- table(test$nativeSpeaker, pred)
conf
# Print out the accuracy
print(sum(diag(conf)) / sum(conf))

# Prune the tree: pruned --#pruning is a method to remove less significant decision node to improve accuracy of model.
pruned<-prune(tree,cp=0.001)
# Draw pruned
fancyRpartPlot(pruned)

# Predict probability values using the model: all_probs
all_probs<-predict(tree,test,type="prob")
all_probs
#ROC curve

# Load the ROCR library
library(ROCR)
probs <- predict(tree, test, type = "prob")[,2]

# Make a prediction object: pred

pred<-prediction(probs,test$nativeSpeaker)

# Make a performance object: perf
perf<-performance(pred,"tpr","fpr")

# Plot this curve
plot(perf)

# Make a performance object: perf
perf<-performance(pred,"auc")

# Print out the AUC
perf@y.values[[1]]

#to get the table --Method 1
Pred<-predict(tree,newdata = test,type = "class")
table(test$nativeSpeaker,pred)

#Method 2
#prediction() Compare test result and predicted results
#performance() tpr = 28/31 , fpr = 19/19
# tpr(senstivity), fpr(1-specificity)
#if the line is higher then it is a 
#best fit.(auc>0.85)
#good fit(middle line)(auc>70.7)
#poor fit(poor fit)(auc<0.5)

