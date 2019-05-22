#Hyperplan drawn to differnciate different class of dependent(target variable)
#-> it should differnciate all values correctly
#-> it should have maximum margin from each class
##Tuneing Parameter
#1. Gamma - Explain angle of support vector(Hyperplan)
#2. Cost - Explain soft margin of support vector(Hyperplan)
heart_df<-read.csv("C:/Users/Mohit/Desktop/Nikhil Analytics Material/R Material/Advance R/R-class Machine Learning/Support Vector Machine SVM/heart_tidy.csv",header=FALSE)
dir()

View(heart_df)
str(heart_df)
library(e1071)
library(caret)
#creating test and train datasets
set.seed(3)
#'seed ' - is used for random sampling. it is used to reduce result
intrain <- createDataPartition(y = heart_df$V14, p= 0.7, list = FALSE)
training <- heart_df[intrain,]
testing <- heart_df[-intrain,]

dim(training);str(training)
dim(testing);str(testing)

anyNA(training)#check your missing values
anyNA(testing)
summary(training)#descriptive statistics for each numeric variable

#converting dependent variable as factor
training$V14<-factor(training$V14)
testing$V14<-factor(testing$V14)

#creating svm model using e1071 package
model_svm <- svm(V14 ~ . , training)
?svm
summary(model_svm)
test_pred<-predict(model_svm,newdata=testing)
test_pred
confusionMatrix(test_pred,testing$V14)


#creating svm model using caret package
trctrl <- trainControl(method = "repeatedcv", number = 50, repeats = 3)
?trainControl
svm_Radial <- train(V14 ~., data = training, method = "svmRadial",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial
plot(svm_Radial)

test_pred <- predict(svm_Radial, newdata = testing)
test_pred
confusionMatrix(test_pred, testing$V14 )

#tunning svm model
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svm_Linear_Grid <- train(V14 ~., data = training, method = "svmRadial",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)
svm_Linear_Grid
plot(svm_Linear_Grid)
test_pred <- predict(svm_Linear_Grid, newdata = testing)
test_pred
confusionMatrix(test_pred, testing$V14 )



