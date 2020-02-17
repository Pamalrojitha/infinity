#Importing Libraries

library(caret)
library(e1071)
library(kernlab)
library(rpart.plot)
library(readxl)

#importing data sets and prepring data according to requirements of processing

credit_card_default_train <- read.csv("credit_card_default_train.csv")
View(credit_card_default_train)
str(credit_card_default_train)
credit_card_default_train[]<-data.matrix(credit_card_default_train)
credit_card_default_train[, 1:25] <- sapply(credit_card_default_train[, 1:25], as.numeric)
credit_card_default_test <- read.csv("credit_card_default_test.csv")
View(credit_card_default_test)
credit_card_default_test[]<-data.matrix(credit_card_default_test)
credit_card_default_test[, 1:24] <- sapply(credit_card_default_test[, 1:24], as.numeric)

#exploratory data analysis

summary(credit_card_default_train)
introduce(credit_card_default_train)
count(credit_card_default_train, vars =  EDUCATION_STATUS)
count(credit_card_default_train, vars =  Balance_Limit_V1)
count(credit_card_default_train, vars =  Gender  )
count(credit_card_default_train, vars =  MARITAL_STATUS)
count(credit_card_default_train, vars =  AGE  )
#according to analysis there were no missing values as well as outliers

#Feature Engineering
#deleting columns with no correlation to next_month_defualt
credit_card_new <- select(credit_card_default_train, -one_of('Client_ID','DUE_AMT_JULY', 'DUE_AMT_AUG',
                                                             'DUE_AMT_SEP','DUE_AMT_OCT','DUE_AMT_NOV','DUE_AMT_DEC'))

#data splitting

set.seed(123)
cl_id<-sample(2,nrow(credit_card_new),prob =c(0.8,0.2),replace = TRUE )
cdefault_train<-credit_card_new[cl_id==1,]
cdefault_test<-credit_card_new[cl_id==2,]
cdefault_train$NEXT_MONTH_DEFAULT = as.factor(cdefault_train$NEXT_MONTH_DEFAULT)

#Model developing

tree <- rpart(NEXT_MONTH_DEFAULT~.,data=cdefault_train)

#predicting and accuracy checking
tree.defualt.predicted<-predict(tree,cdefault_test,type='class')

cdefault_test$NEXT_MONTH_DEFAULT<-as.factor(cdefault_test$NEXT_MONTH_DEFAULT)
confusionMatrix(tree.defualt.predicted,cdefault_test$NEXT_MONTH_DEFAULT)

tree.newdefualt.predicted<-predict(tree,credit_card_default_test,type='class')
View(tree.newdefualt.predicted)
write.table(tree.newdefualt.predicted,file = "Predictions8.csv",sep=",")