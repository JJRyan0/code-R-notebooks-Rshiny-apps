
#Classical one sample test for normality in R

#John Ryan created on 07/12/2017

#read the data to a table for inspection
das <- read.table( "C:/das.txt", header=T )
par(mfrow=c(2,2))
plot( das$y)

#creates a boxplot to investigate outliers
boxplot( das$y)
#creates a histogram
hist( das$y, main="")

#create a mistake in the data to demonstrate the effect of outliers
y2 <- das$y
y2[52]<-21.75
plot(y2)
#view the summary stats
#This gives us six pieces of information about the vector called y. 
#The smallest value is 1.904 (labeled Min. for minimum) 
#and the largest value is 2.984 (labeled Max. for maximum). 
#There are two measures of central tendency: the median is 2.414 
#and the arithmetic mean in 2.419.

summary( das$y)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.904   2.241   2.414   2.419   2.568   2.984
#creates a quantile–quantile plot, which shows a slight S-shape, 
#but there is no compelling evidence of non-normality.
qqnorm( das$y )
qqline( das$y, lty=2)

#One test we might use is the Shapiro-Wilks normality test (shapiro.test  in R) 
#for testing whether the data in a vector comes from a normal distribution. 
#Let’s generate some data that is lognormally distributed:
dasX <-exp(rnorm(30))
#What does this look like?
hist( dasX )
#Does the plot look normal?
qqnorm( dasX )
qqline( dasX, lty=2)

#try the Shapiro-Wilks normality test and remember we should want our 
#non-normal data to fail the normality test

shapiro.test( dasX )

        Shapiro-Wilk normality test
data:  dasX 
W = 0.7485, p-value = 8.614e-06

#Hang on this is a very low p-value. Is this distribution normal or not? 
#This p-value tells you what the chances are that the 
#sample comes from a normal distribution. 
#The lower this value, the smaller the chance.


#check the test with a ‘real’ normal distribution?
#Try this (you won’t get the exact same results as shown here):

checkShapiroData <- rnorm(n=10)

shapiro.test( checkShapiroData)
        Shapiro-Wilk normality test
data:  checkShapiroData 
W = 0.9594, p-value = 0.7787

checkShapiroData <- rnorm(n=50)
checkShapiroData <- rnorm(n=100)
checkShapiroData <- rnorm(n=250)

shapiro.test( checkShapiroData)
        Shapiro-Wilk normality test
data:  checkShapiroData 
W = 0.9941, p-value = 0.4408


