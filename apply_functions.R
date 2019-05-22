#examples of apply functions
student<-matrix(c(70,60,65,50,55,72,55,85,60,50,70,55,50,55,60,65),4,4,dimnames=list(c("PHY","CHEM","MATH","BIO"),c("RAM","RADHA","RAVI","RAKESH")))
student

#apply
#sum of column
#calculate total marks of each student 

total_marks<-apply(student,2,sum)
total_marks

#mean of row
#calculate average marks of each subject 

subject_avg<-apply(student,1,mean)
subject_avg

#applying a function on all values of matrix
div_result<-apply(student,1:2,function(x) x/10)
div_result

div_result2<-apply(student,1:2,sqrt)
div_result2

#----------------------------------------------------------------

#lapply
# create a list with 3 elements
list1<-list(a=1:10,b=3:30,c=6:60)
list1

#calculate mean of the value in each element
list1_avg<-lapply(list1,mean)
list1_avg
class(list1_avg)
list1_avg[["a"]]

#----------------------------------------------------------------
#sapply
#create a list with 3 elements
list1<-list(a=1:10,b=3:30,c=6:60)
list1

#mean of values using sapply
list1.mean <- sapply(list1, mean)
list1.mean

#type of object returned
class(list1.mean)

# it's a numeric vector, so we can get element "c" like this
list1.mean['c']

list1.mean2 <- sapply(list1, mean,simplify=F)
list1.mean2
class(list1.mean2)
#note - simplify=T - return vector result, and it is default
#       simplify=F - return list result

list1.mean3 <- sapply(list1, mean,simplify=T,USE.NAMES=F)
list1.mean3
class(list1.mean3)

list3<-list(A=matrix(1:9,3,3),B=matrix(2:21,5,4),C=matrix(4:9,3,2))
list3
sapply(list3,"[",,1)




#----------------------------------------------------------------
#vapply

#creating a list 
l<-list(a = 1:10, b = c("SAS","R","VBA","Excel"))
l

#is numeric using vapply
l.logical<-vapply(l,is.numeric,logical(1))
l.logical
vapply(list1,max,numeric(1))

l.fivenum <- vapply(list1, fivenum, c(Min=0, "1st Qu"=0, Median=0, "3rd Qu"=0, Max=0))
l.fivenum
class(l.fivenum)

#----------------------------------------------------------------
help(rapply)
#rapply
#create a list
l1 <- list(a = 1:10, b = 40:50)
l1

#mean of each element
result_rapply<-rapply(l1,mean)
result_rapply

# log2 of each value in the list
log2_rapply1<-rapply(l1,log2)
log2_rapply1

sapply(l1,log2) # return a list

# log2 of each value in each list
log2_rapply2<-rapply(l1,log2,how="list")
log2_rapply2
