
setwd("F:/my work/R Material/Logistic Regression")
dir()
Titanic<-read.csv("Titanic.csv")
str(Titanic)
View(Titanic)

#Data Cleaning
#replace all blank value by NA
Titanic[Titanic==""]<-NA

Titanic <- subset(Titanic,select=c(2,3,5,6,7,8,10,12))
View(Titanic)

#find number of missing value under each column
miss_count<-sapply(Titanic,function(x) sum(is.na(x)))
(miss_count/891)*100

#If percentage of missing values is more than 40%, 
#then drop that column

#FIND NUMBER OF unique VALUES
sapply(Titanic, function(x) length(unique(x)))

is.na(Titanic$Age)

Titanic[is.na(Titanic$Age),c("Age")]<-median(Titanic$Age,na.rm=T)
View(Titanic)

#Drop all rows when EMbarked is missing
data <- Titanic[!is.na(Titanic$Embarked),]
View(data)
#Removing Row names
rownames(data) <- NULL

#Machine Learning Concepts - Dividing data into two parts 
#Train Data - 75% and Test Data - 25% 
#(Train data - 70%-80% Test data - 30%-20%)

train <- data[1:800,]
test  <- data[801:889,]

model <- glm(Survived ~.,family=binomial,data=train)

#By using function summary() we obtain the results of our model:
  test$Survived
summary(model)
anova(model, test="Chisq")
confint(model) # In what confidence Interval we should increase
#independent variable to improve predicitability of derived equation
#by one unit.

#Model Validation - will be performed by ROC curve
#this ROC curve will be derived between TPR and FPR
#TPR - True Positive Rate, FPR - False Positive Rate

library(ROCR)
p <- predict(model, newdata=test, type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results 
table(test$Survived,fitted.results)
(50+24)/(50+6+9+24)

misClasificError <- mean(fitted.results != test$Survived,na.rm=T)
print(paste('Accuracy',1-misClasificError))


