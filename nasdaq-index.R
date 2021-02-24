#NASDAQ PRICE INDEX ANALYSIS - 2016 @JJRyan0

install.packages("reshape2")
install.packages("ellipse")
install.packages("ggplot2")
install.packages("plyr")
install.packages("rggobi")
install.packages("VIM")
install.packages("RGtk2")
library("RGtk2")
library("reshape2")
library("ellipse")
library("ggplot2")
library("plyr")
library("rggobi")


#set directory
setwd("D:/Rstudiodata")
#Import the data via csv file from directory
nasdaqdata = read.csv("D:/Rstudiodata/NASDAQ Price Data.csv")
View(nasdaqdata)
head(nasdaqdata)
View(nasdaqdata)
#Summerise Data
summary(nasdaqdata)
#Cleaning the data for analysis clearing out unwanted characters

clean_numeric <- function(s){
  s = gsub("%|\\$|,|\\)|\\(", "", s)
  s = as.numeric(s)
}
nasdaqdata <- cbind(nasdaqdata[,1:6],apply(nasdaqdata[,2:25], 2, clean_numeric))

#Histogram of Price Distribution
hist(nasdaqdata$Current.Price, breaks=100, main="Nasdaq Price Distribution for Period",
     xlab="Price")
hist(nasdaqdata$Current.Price[nasdaqdata$Current.Price<150], breaks=100, main="Final_Price", xlab="Price")

Company_Price_Averages = aggregate(Current.Price~Company.Name,data=nasdaqdata,FUN="mean")
colnames(Company_Price_Averages)[2] = "Company_Price_Average"
install.packages("ggplot")
library(ggplot)
ggplot(Company_Price_Averages, aes(x=Company.Name, y=Company_Price_Average, fill=Company.Name)) 
geom_bar(stat="identity") + ggtitle("Company Average Prices")theme(axis.text.x = element_text(angle = 90, hjust = 1))

require(DMwR)
attach(nasdaqdata)

#fill missing data
co=knnImputation(data_norm, k = 10, scale = T, meth = "weighAv", distData = NULL)
library(package="ellipse")
d= c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25)
plotcorr(cor(nasdaqdata[d,]),col=col=clo <- c("orange","green", "purple"))

company_drilldown = subset(nasdaqdata,Company.Name=="Corp")
View(company_drilldown)


ggplot(company_drilldown, aes(x=Company.Name, y=Current.Price, fill=Company.Name)) 
geom_bar(stat="identity") + theme(legend.position="none") 
ggtitle("Company Avg Prices")  
theme(axis.text.x = element_text(angle = 90, hjust = 1))

Corporation_avg = melt(nasdaqdata, id="Corporation")

Corporation_avg = subset(Corporation_avg,variable%in%c("Current.Price","Price.to.Earnings..TTM.","Price.to.Book..ttm.","Current.Ratio",
"Earnings.Per.Share"))

library(arules, quietly=TRUE)
transactiondataset = as(nasdaqdata[datsetsample,c(2:3, 5, 14, 21, 25)], "Frequencytransactions")
itemFrequencyPlot(transactiondataset, support=0.1, cex=0.9)









