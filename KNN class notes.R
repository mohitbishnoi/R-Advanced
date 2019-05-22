#Market basket analysis - bigbasket is using knn for similar products suggestion.
#reading data into R
data<-read.csv("C:/Users/Mohit/Desktop/Nikhil Analytics Material/R Material/Advance R/R-class Machine Learning/K Nearest Neighbors/US Presidential Data.csv")
str(data)
View(data)

library(caret)
library(e1071)

# Transforming the dependent variable to a factor
data$Win.Loss = as.factor(data$Win.Loss)

#Partitioning the data into training and validation data
set.seed(101)
index = createDataPartition(data$Win.Loss, p = 0.7, list = F )
train = data[index,]
test = data[-index,]

str(train)
str(test)

# Setting levels for both training and validation data
levels(train$Win.Loss) <- make.names(levels(factor(train$Win.Loss)))
levels(test$Win.Loss) <- make.names(levels(factor(test$Win.Loss)))

#At least one of the class levels is not a valid R variable name; 
#This will cause errors when class probabilities are generated because the 
#variables names will be converted to  X0, X1 . 
#Please use factor levels that can be used as valid R variable names  (see ?make.names for help)

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = "repeatedcv",
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

model1 <- train(Win.Loss~. , data = train, method = "knn", # we  are using . here to get all coloumns
                preProcess = c("center","scale"), #Preprocessing for scalling the data{incase data have unequal number of digits in independent variable}
                trControl = x,
                metric = "ROC", #ROC is a curve
                tuneLength = tunel)
?train
# Summary of model
model1
plot(model1)

# Validation
valid_pred <- predict(model1,test, type = "prob")

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(valid_pred[,2],test$Win.Loss)

# Calculating Area under Curve (AUC)
perf_val <- performance(pred_val,"auc")
auc<-perf_val@y.values[[1]]
auc

# Plot AUC
perf_val <- performance(pred_val, "tpr", "fpr")
plot(perf_val, col = "green", lwd = 1.5)
