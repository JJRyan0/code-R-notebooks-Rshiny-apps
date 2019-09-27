#Text Mining - Naïve Bayes & Hierarchical clustering

#Problem: Short Message Service (SMS) text messages are sent to potential targeted consumers with detials unwanted advertising known as SMS spam.
#Solution: Develop a Naïve Bayes classification algorithm to filter SMS spam would providing a useful enhancement for smart phone manufactors.

#John Ryan - 25/10/17

#Read in the spam dataset from a csv file
library(readr)
sms_spam <- read_csv("~/sms_spam.csv")
str(sms_raw)
View(sms_spam)# contains two columns 1. type = ham or spam 2. text = contains of the text of the sms.

#variable currently a char vector need to convert to a factor.
sms_spam$type <- factor(sms_spam$type)
str(sms_spam$type)
table(sms_spam$type)

#text processing - removing numbers, punctuations & stop words.

#1. create a corpus which is an R object to store the text, 
#using function VectorSource as the format of the data.

library(tm)
sms_corp<- Corpus(VectorSource(sms_spam$text))
inspect(sms_corp[1:5])

# remove stop words
corpus_clean <- tm_map(sms_corp, tolower)#drop the text to a lower case
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())#remove stop words
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
#search for synonyms - any words with same meaning
library("wordnet")
synonyms("competition")# can be used to see what types of words will show up for spam competitions for example

# Create a machine Learning Model - Naïve Bayes Classifier

#1.Tokenize the corpus and create a sparse matrix 
sms_doctm <- DocumentTermMatrix(corpus_clean)

#Train Test split - splits data into a training and test set
sms_train <- sms_spam[1:4169, ]
sms_test <- sms_spam[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

#compare the subsets are to the correct proportion
prop.table(table(sms_train$type))
prop.table(table(sms_test$type))
prop.table(table(sms_corpus_train$type))
prop.table(table(sms_corpus_test$type))

#reduce the number of features used by the algorithm, remove any words that will apear less than 5 times 
sms_dict <- findFreqTerms(sms_corpus_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary - sms_dict))

#The sparse matrix contaions counts of how many times words appear in the text, we need 
#to convert counts to factors using the following function
conv_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = C(0, 1), labels = c("No", "Yes"))
  return (x) }

#specify training and test sets
sms_train <- apply(sms_train, MARGIN = 2, conv_counts)
sms_test <- apply(sms_test), MARGIN = 2, conv_counts)

#Build the clissification model
sms_model <- naiveBayes(sms_train, sms_train$type)
sms_test_pred <- predict(sms_model, sms_test)

#model evaluation

library(gmodels)
CrossTable(sms_test_pred, sms_classifier, sms_test$type, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

