#Random Forest
setwd("F:/R Class/Predictive Modeling Using R/Random Forest/bank")
## Read data
bankdata<-read.csv(file="bank.csv",header = T,sep=";")
## Explore data frame
View(bankdata)
names(bankdata)
str(bankdata)

#Dividing data into two groups
#for (i in 1:5)
#{
#  sample.ind <- sample(2, nrow(bankdata), replace = T, prob = c(0.75,0.25))
  
#  Traindata&i <- bankdata[sample.ind==1,]  
#}

sample.ind <- sample(2, nrow(bankdata), replace = T, prob = c(0.75,0.25))

Traindata <- bankdata[sample.ind==1,]
Testdata <- bankdata[sample.ind==2,]

table(Traindata$y)/nrow(Traindata)
## 
##  no         yes 
## 0.8864103  0.1135897

table(Testdata$y)/nrow(Testdata)
## 
#no          yes 
#0.8797127  0.1202873 

varNames <- names(Traindata)
# Exclude ID or Response variable
response_var <- varNames[!varNames %in% c("y")]

# add + sign between exploratory variables
response_var1 <- paste(response_var, collapse = "+")

# Add response variable and convert to a formula object
random_for.form <- as.formula(paste("y", response_var1, sep = " ~ "))

#Random Forest using R
install.packages("randomForest")
library(randomForest)
random_forest_eq <- randomForest(random_for.form, Traindata,
                              ntree=500,importance=T)

plot(random_forest_eq)
 
# Variable Importance Plot
varImpPlot(random_forest_eq, sort = T, main="Variable Importance", n.var=5)


#Model perfromance
library(ROCR)
probs <- predict(random_forest_eq, Testdata, type = "prob")[,2]

# Make a prediction object: pred

pred<-prediction(probs,Testdata$y)

# Make a performance object: perf
perf<-performance(pred,"tpr","fpr")

# Plot this curve
plot(perf)

# Make a performance object: perf
perf<-performance(pred,"auc")

# Print out the AUC
perf@y.values[[1]]
