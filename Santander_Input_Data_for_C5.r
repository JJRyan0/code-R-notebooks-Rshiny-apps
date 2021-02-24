#Data Source https://www.kaggle.com/c/santander-customer-satisfaction


train <- read.csv("C:/Big Data Assignment/trainsantan.csv",sep=",",header=TRUE)

#####Finding redundant columns

count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)


##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
  }
}

str (train)

####install c50 package
install.packages("C50")
library(C50)

###randomise
set.seed(1234)

####TARGET VARIABLE READING AS NUMERIC - CONVERT TO A FACTOR

train$TARGET<-as.factor(train$TARGET)
str(train$TARGET)

###Rename TARGET factor

train$TARGET <- revalue(train$Target, c("1"="Satisfied", "0"="DisSatisifed"))

###Reduce observations to improve run time
santander_rand <- train[order(runif(2000)), ]

###split into training set and test set for 70-30 split
santander_train <- santander_rand[1:1400, ]
santander_test <- santander_rand[1401:2000, ]

####Include table of proportions

prop.table(table(santander_train$TARGET))
prop.table(table(santander_test$TARGET))

###Build the Model

santander_model <- C5.0(TARGET ~ ., data = santander_train)
santander_model

summary(santander_model)

###Predict on Test Data

santander_pred <- predict(santander_model, santander_test)

library(gmodels)
CrossTable(santander_test$TARGET,
             santander_pred,prop.chisq = FALSE, prop.c = FALSE,
             prop.r = FALSE,dnn = c('actual default',
                                    'predicted default'))

###Bagging 

santander_boost10 <- C5.0(santander_train[-338],
                         santander_train$TARGET, trials = 10)
santander_boost10

####Bagging 2

santander_boost_pred10 <- predict(santander_boost10,
                               santander_test)

CrossTable(santander_test$TARGET, santander_boost_pred10,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))
