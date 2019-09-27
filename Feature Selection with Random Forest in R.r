#Feature selection with a Random Forest Algorithm

library(mlbench)
library(caret)

newspop <- read.csv("C:/Clusters/OnlineNewsPopularityUCI.csv", header = TRUE, stringsAsFactors = FALSE)
head(newspop)

library(ggplot2)
qplot(x,y,data=newspop,colour=color,size=4) + 
  scale_colour_gradient(low="black", high="white")

#Feature Selection - removing unimportant features
correlationMatrix <- cor(newspop[,1:60])
print(correlationMatrix)
library(corrplot)
corrplot(correlationMatrix, method="shade")
highlyCorrelation <- findCorrelation(correlationMatrix, cutoff=0.5)
print(highlyCorrelation)

#feature selection 2
# ensure the results are repeatable
set.seed(123)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(newspop[,1:60], newspop[,61], sizes=c(1:60), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

pairs(newspop[c("age", "bmi", "children", "expenses")])
pairs.panels(insurance[c("age", "bmi", "children", "expenses")])