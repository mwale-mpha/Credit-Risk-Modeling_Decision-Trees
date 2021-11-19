#DECISION TREES - identifying risky bank loans using C5.0 decision trees
credit<- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/credit.csv", stringsAsFactors = TRUE)
str(credit)

# nominal features
table(credit$checking_balance)
table(credit$savings_balance)

# numeric features
summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

# converting default into a factor
credit$default <- factor(credit$default, levels = c("1","2"), labels = c("no","yes"))
table(credit$default)

# DATA PREPARATION - CREATING RANDOM TRAINING & TEST DATASETS
set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]

summary(credit$amount)
summary(credit_rand$amount)

head(credit$amount)
head(credit_rand$amount)

# splitting 90% into training data set and 10% test data set
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

# TRAINING THE MODEL ON THE DATA
install.packages("C50")
library(C50)

?C5.0Control

credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

summary(credit_model)

# EVALUATING MODEL PERFORMANCE
credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Boosting accuracy of decision trees
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10
summary(credit_boost10)
summary(credit_boost10)#end of code

# Let's see if there's also an improvement on the test data
credit_boost_pred10 <- predict(credit_boost10, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

