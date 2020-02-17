#Importing Libraries

library(knitr)
library(tidyverse)
library(ggplot2)
library(mice)
library(lattice)
library(DataExplorer)
library(dplyr)
library(caret)
library(e1071)
library(randomForest)
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

#data splitting

set.seed(3)
cl_id<-sample(2,nrow(credit_card_default_train),prob =c(0.8,0.2),replace = TRUE )
cdefault_train<-credit_card_default_train[cl_id==1,]
cdefault_test<-credit_card_default_train[cl_id==2,]

#Model developing

bestmtry<-tuneRF(cdefault_train,cdefault_train$NEXT_MONTH_DEFAULT,stepFactor=1.2,improve=0.01,trace =T,plot=T)
default_forest<-randomForest(NEXT_MONTH_DEFAULT ~.,data = cdefault_train)
default_forest
default_forest$importance
varImpPlot(default_forest)

#predicting and accuracy checking

predic<-predict(default_forest,newdata = cdefault_test,type = "class")
predic
predic.correct <- ifelse(predic > 0.5, 1, 0)
predic.correct 

confusionMatrix(table(predic.correct,cdefault_test$NEXT_MONTH_DEFAULT))

finalpredic<-predict(default_forest,newdata = credit_card_default_test,type = "class")
finalpredic
finalpredic.correct <- ifelse(finalpredic > 0.5, 1, 0)
finalpredic.correct 
write.table(finalpredic.correct,file = "Predictions6.csv",sep=",")


