
#This gives you the current directory. Here is where you need to place your 
#Csv file, or you can set your current directory to a different folder using
#setwd('path to your folder...')

getwd()



#Read the given CSV file from Moodle 'Test.csv
#header = TRUE as first row in csv file has the name of the columns
dataset1 = read.csv("Test.csv",header=TRUE, sep=',')

#Access each column
dataset1$Age
dataset1$Yearsofstudy


summary(dataset1)

#plot columns against each other
#par(mar=c(1,1,1,1)) # (in case of error from below, try uncommenting and running this line as well)
plot(dataset1$Age, dataset1$Yearsofstudy, col="blue",pch=19, xlab = 'Age' ,ylab='Years of study')


#Examples from the first R class
print("Hello world!")

s="hello world"

a=5.6
is.numeric(a)
is.character(a)
b=as.integer(a)
b

averageAge=30

myFunc <- function(name){
  message = sprintf("My name is %s and my age is %.2f",name,averageAge)
  print(message)
}
myFunc("Sandra")



firstNames = c("jeff","roger","andrew","brian")
firstNames

for(item in firstNames){
  print(item)
}


name="george"
substring(name,2,2)

mixedVector = c("jeff",2)
mixedVector

class(mixedVector)

mixedVector = c(2,3)
mixedVector
mixedVector  + 5

class(mixedVector)
names(dataset1)
dataset1[3,4]

library(help = "dataset1")

plot.ts(ToothGrowth)
summary(ToothGrowth)
install.packages(ggplot2)