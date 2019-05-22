#We use gbm if correlation is very less. for high corelation we use lm(logistic regression).
library(gbm)
library(MASS)
data("Boston")
View(Boston)
str(Boston)

#separating training and test data
#train - 75%, test - 25%
train=sample(1:nrow(Boston),size=round(0.75*nrow(Boston)))

train_data=Boston[train,]
test_data=Boston[-train,]
str(train_data)
#distribution = if it is numeric = gussian
#                         factor 2 = Bernolli
#                         More the 2 = Adaboosting, Xgboosting
Boston.boost=gbm(medv ~ . ,data = train_data,
                 distribution = "gaussian",
                 n.trees = 1000,
                 shrinkage = 0.01, interaction.depth = 4)
Boston.boost

summary(Boston.boost)
#Summary gives a table of Variable Importance and a plot of Variable Importance
#n.trees Integer specifying the total number of trees to fit. This is equivalent
#to the number of iterations and the number of basis functions in the additive expansion.
#Default is 100.
#and the shrinkage parameter lambda=0.01 which is also known as learning rate.
#interaction depth- which is the total splits we want to do.Here each tree is a 
#small tree with only 4 splits.
#The summary of the Model gives a feature importance plot.
#In this list we can see the top most important variable and the least important 
#variable.
#And the 2 most important features which explain the maximum variance in the Data set
#is lstat i.e lower status of the population (percent) and 
#rm which is average number of rooms per dwelling.


#Plotting the Partial Dependence Plot
#The partial Dependence Plots will tell us the relationship and dependence of the
#variables Xi with the Response variable Y

#Plot of Response variable with lstat variable
plot(Boston.boost,i="lstat") 
#Inverse relation with lstat variable

plot(Boston.boost,i="rm") 
#as the average number of rooms increases the the price increases

#Prediction on Test Set
#We will compute the Test Error as a function of number of Trees.

n.trees = seq(from=1 ,to=1000, by=1) #no of trees-a vector of 100 values 

#Generating a Prediction matrix for each Tree
predmatrix<-predict(Boston.boost,test_data,n.trees = 368)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(test_data,apply( (predmatrix-medv)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees

plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)

best.iter <- gbm.perf(Boston.boost, method = "OOB")
print(best.iter)
