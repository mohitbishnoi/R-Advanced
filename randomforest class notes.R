#Random Forest--Parellal Method--used for credit card fraud detaction
getwd()
## Read data
bankdata<-read.csv("C:/Users/Mohit/Desktop/Nikhil Analytics Material/R Material/Advance R/R-class Machine Learning/Random Forest/bank/bank.csv",header = T,sep=";")
## Explore data frame
View(bankdata)
names(bankdata)
str(bankdata)

#Dividing data into two groups
#for (i in 1:5)
#{
#  sample.ind <- sample(2, nrow(bankdata), replace = T, prob = c(0.75,0.25))

 # Traindata&i <- bankdata[sample.ind==1,]  
#}

sample.ind <- sample(2, nrow(bankdata), replace = T, prob = c(0.75,0.25))

Traindata <- bankdata[sample.ind==1,]
Testdata <- bankdata[sample.ind==2,]
str(Traindata)
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

response_var
# add + sign between exploratory variables
response_var1 <- paste(response_var, collapse = "+")
response_var1
# Add response variable and convert to a formula object
random_for.form <- as.formula(paste("y", response_var1, sep = " ~ "))
random_for.form

#Random Forest using R
install.packages("randomForest")
library(randomForest)
random_forest_eq <- randomForest(random_for.form, Traindata,
                                 ntree=130,importance=T)
?randomForest
plot(random_forest_eq)
random_forest_eq
# Variable Importance Plot
varImpPlot(random_forest_eq, sort = T, main="Variable Importance", n.var=5)

#Model perfromance
library(ROCR)
probs <- predict(random_forest_eq, Testdata, type = "prob")[,2]
probs
# Make a prediction object: pred

pred<-prediction(probs,Testdata$y)
pred
# Make a performance object: perf
perf<-performance(pred,"tpr","fpr")
perf
# Plot this curve
plot(perf)

# Make a performance object: perf
perf<-performance(pred,"auc")
perf
# Print out the AUC
perf@y.values[[1]]
