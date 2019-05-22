#Singificance Test 
# - is used to identify group formation is based on numeric variable or not
# - statistical significance in mean values under different groups
# - is used to compare group mean values - if there is difference in mean that means
# groups are different from each other.

#One Sample test  -
#Null hypothesis - mean=value (default= zero)
#alternative - case 1 - mean not equal to value  - two tail test
#              case 2 - means less than value    - one tail test - left tail test
#              case 3 - mean greater than value  - one tail test - right tail test

#all decision will be based on p-value
#if p-value > 0.05 then accept null hypothesis
#else reject null hypothesis - rejecting null hypothesis,
#we have to accept alternative hypothesis

#0.05 - is called as significance level - aplha
#if you take higher alpha ( more than 0.05 ) - it will cause TYPE 1 error
#if you take lower alpha ( less than 0.05 ) - it will cause TYPE 2 error

#TYPE 1 - you are rejecting null hypohtesis when it should be accepted
#TYPE 2 - you are accepting null hypothesis when it should be rejected

#confidence level = 1- aplha (significance level)

#Two Sample test  -   
 # Type 1 - Unpaired test - Null: mean1=mean2 , alternative - mean1 not equal to mean2, mean1 > mean2 , mean1 < mean2
 # Type 2 - paired test   - Null: mean1 - mean2 = 0 , alternative - mean1 - mean2 not equal to 0 , mean1 - mean2 > 0 , mean1 - mean2 < 0


# one sample t-test
data(mtcars)
View(mtcars)
summary(mtcars)

mpg<-mtcars$mpg

t.test(mpg,mu=15) # Ho: mu=15

#conclusion
#As p-value = p-value = 4.054e-05 < 0.05 that means we have to reject null hypothesis and
#accept alternative hypothesis - means is not equal to 15.

t.test(mpg,mu=25,alternative = "greater")
#As p-value = 1 > 0.05 that means we have to accept null hypothesis and
#mean is not greater than 25, mean is less than or equal to 25.

#Two-sample test
View(mtcars)
mpg.cyl6 <- subset(mtcars,cyl==6,mpg)
mpg.cyl8 <- subset(mtcars,cyl==8,mpg)
mpg.cyl8<-mpg.cyl8[1:7,]

t.test(mpg.cyl6,mpg.cyl8,conf.level = .90) # where y1 and y2 are numeric
#Conclusion:
#p-value = 0.005036 - is less than 0.10 so we reject null hypothesis and accept
#alternative. Mean1 is not equal to mean2.

qt(.95,df=6)  # finding t-value from t-table


# paired t-test
#in case of 100-meter race
before_training = c(12.9, 13.5, 12.8, 15.6, 17.2, 19.2, 12.6, 15.3, 14.4, 11.3)
after_training = c(12.7, 13.6, 12.0, 15.2, 16.8, 20.0, 12.0, 15.9, 16.0, 11.1)

t.test(before_training,after_training, paired=TRUE)
#NUll:        before_training-after_training=0
#alternative: before_training-after_training not eqault 0
#Conclusion:
#As p-value = 0.8358 >0.05 that there is no significance difference in their timing

after_training2 = c(12.0, 12.6, 12.0, 14.2, 16.6, 18.0, 11.0, 15.2, 16.0, 11.7)

t.test(before_training,after_training2, paired=TRUE, alt="greater")

#conclusion:
#As p-value = 0.05164 is more than 0.05 that there is no significance difference
#their timing


t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric
#You can use the var.equal = TRUE option to specify equal variances and a pooled variance estimate. You can use the alternative="less" or alternative="greater" option to specify a one tailed test.

#Two Sample Test   - test to compare variance of two groups
#null: variance is equal
#alte: variance is not equal
#Unpaired Test

a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)

var.test(a,b)

#conclusion
#As p-value = 0.2834 is more than 0.05 that means variance is equal.

#f-value = variance of var1 / variance of var2
# 1-0.25 = .975

qf(0.975, 9, 9)

setwd("F:/R Class/R-class Machine Learning/Significance testing")
dir()
cancer<-read.table("Cancer Data.txt",skip=12)
View(cancer)
#aov - is used to compare group mean value when there is more
# than 2 groups
#aov - analysis of variance (ANOVA)
aov.out<-aov(V1~V2,data=cancer)
summary(aov.out)

#Conclusion
#As p-value = 0.000229 <0.05, so we reject null hypothesis,
#that means mean of different group is not same
#average number of death from different cancer is not equal

aggregate(cancer$V1,list(cancer$V2),mean)



