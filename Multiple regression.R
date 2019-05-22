
setwd("C:/Users/Mohit/Desktop/Nikhil Analytics Material/R Material/Advance R/R-class Machine Learning/Multiple Regression")
marketing<- read.csv("marketing.csv")
View(marketing)
marketing2<-marketing[,2:5]
cor(marketing2)

#predict sales using youtube

lm.out<-lm(sales~youtube,marketing2)
summary(lm.out)

# R-squared:  0.6119, as r squared is below 0.7, so we go for multiple regression

#predict sales using youtube and facebook

lm.out2<-lm(sales~youtube+facebook,marketing2)
summary(lm.out2)

#R-squared:  0.8972 as r squared is more than 0.7, so we accept this regression result.

lm.out3<-lm(sales~youtube+facebook+newspaper,marketing2)
summary(lm.out3)

#R-squared:  0.8972, as r squared is not changing significantly by adding newspaper, so 
#we will keep newspaer is regression model.

#our best equation is  lm.out2




