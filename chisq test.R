#Chi-Square test - is used to test association between two character (Categorial) variables

##### H0: The The two variables are independent
##### H1: The two variables are related.

#or 
# H0: There is no association between given variables
# H1: There is association between given variables

#if p-value is greater than 0.05 then you are going to accept null hypothesis,
#but if p-value is less than 0.05 then we reject null hypothesis, that we have to
#accept alternative hypothesis; and your test is significant.

# Load the library.
library("MASS")

# Create a data frame from the main data set.
attach(Cars93)
View(Cars93)
car_data <-data.frame(AirBags, Type)

# Create a table with the needed variables.
car_Table = table(car_data$AirBags, car_data$Type) 
print(car_Table)

# Xsq = summation((obs-exp)^2/exp)
#exp = RT*CT/Total
#df  = (r-1)*(c-1)

# Perform the Chi-Square test.
chisq.test(car_Table)

####CONCLUTION.......
#Pearson's Chi-squared test
#data:  car_data
#X-squared = 33.001, df = 10, p-value = 0.0002723

#Here p value is less than 0.05 so we reject hull hypothesis and conclude that 
#AirBags and Type have significant relationship.


###### EXAMPLE 2 FOR CHISQR TEST####

NEW_DATA<- data.frame(aspirin=c("no","yes","no","yes"),
		      MI=c("yes","yes","no","no"),
		      Count=c(189,104,158,133))
View(NEW_DATA)

#Now we create the crosstabulation and compute the chi-squared:
#The xtabs() function creates the cross-tabulation. 
#The two factors (aspirin and MI) define the table. Count countains the counts for each cell of the table.

Aspirin<-xtabs(Count~aspirin+MI,data=NEW_DATA)
Aspirin


CHSQR<-chisq.test(Aspirin,correct=F)
CHSQR

#Pearson's Chi-squared test
#data:  Aspirin
#X-squared = 6.3112, df = 1, p-value = 0.012
#The chi-squared statistic is 25.01 with one degree of freedom.
#The p-value is desired probability; if it less than .05 we infer dependence. 
#we are failed to null hypothesis so we have to accept alternative hypothesis
#that we there is association between Aspirin and MI.

#another method to conclude result is using x-square and df
#if x-square is more than x-square from table, then reject H0 and accept H1.
#here x-square from table is 3.841 which is less than 6.3112 x-square, so we
#reject h0 and accept H1. that is association.




#Goodness of fit
library(MASS)
data(survey)
str(survey)
View(survey)
smoke.freq<-table(survey$Smoke)
smoke.freq/nrow(survey)
smoke.prob<-c(0.045,0.795,0.085,0.075)
sum(smoke.prob)

chisq.test(smoke.freq,p=smoke.prob)
#X-squared = 0.10744, df = 3, p-value = 0.9909
#conclusion - as p>0.05, so we accept null hypothesis,
#that means sample is consistent with expected distribution.

