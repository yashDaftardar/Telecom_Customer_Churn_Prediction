##########################################################################################################

####################################
#Telecom Customer Churn Prediction #
####################################

#CS 513
#Knowledge Dis & Data Mining
#Team:
#     Manas Kulkarni
#     Sai Goutham Gaddam
#     Yash Daftardar
#     Sujay Salvi


########################################### IMPORTS ######################################################

#install.packages('randomForest')
#install.packages('dplyr')
#install.packages('tidyverse')
#install.packages('e1071', dependencies = TRUE)
#install.packages("rpart")
#install.packages("rpart.plot")     
#install.packages("rattle")         
#install.packages("RColorBrewer") 
#install.packages("C50")
#install.packages("neuralnet")
#install.packages("h20")
#install.packages("ggplot2")
#install.packages("data.table")

library(h2o)
library(ggplot2)
library(data.table)
library("neuralnet") 
library('C50')
library(rpart)
library(rpart.plot)  		
library(rattle)         
library(RColorBrewer)   
library("tidyverse")
library("randomForest")
library("dplyr")
library(e1071)

######################################## DATA CLEANING AND PREPROCESSING #################################

rm(list=ls())

original<-read.csv('/home/manas/Stevens/CS 513 KDD/Project/Telecom/Telecom_customer churn.csv')


dat<-original
dat[dat==""]<-NA
numeric_raw<-select_if(dat, is.numeric)
numeric_raw[] <- lapply(numeric_raw, function(x) { 
  x[is.na(x)] <- floor(mean(x, na.rm = TRUE))
  x
})
non_numeric_raw<-select_if(dat, negate(is.numeric))
colnames(non_numeric_raw)[colSums(is.na(non_numeric_raw)) > 0]

a1<-c(dat$prizm_social_one)
x1<-factor(a1)
most_freq1<-tail(names(sort(table(x1))), 1)
x1[is.na(x1)] <- most_freq1
drops1 <- c("prizm_social_one")
dat[ , !(names(dat) %in% drops1)]
dat$prizm_social_one <- x1

a2<-c(dat$area)
x2<-factor(a2)
most_freq2<-tail(names(sort(table(x2))), 1)
x2[is.na(x2)] <- most_freq2
drops2 <- c("area")
dat[ , !(names(dat) %in% drops2)]
dat$area <- x2

a3<-c(dat$dualband)
x3<-factor(a3)
most_freq3<-tail(names(sort(table(x3))), 1)
x3[is.na(x3)] <- most_freq3
drops3 <- c("dualband")
dat[ , !(names(dat) %in% drops3)]
dat$dualband <- x3

a4<-c(dat$refurb_new)
x4<-factor(a4)
most_freq4<-tail(names(sort(table(x4))), 1)
x4[is.na(x4)] <- most_freq4
drops4 <- c("refurb_new")
dat[ , !(names(dat) %in% drops4)]
dat$refurb_new <- x4

a5<-c(dat$hnd_webcap)
x5<-factor(a5)
most_freq5<-tail(names(sort(table(x5))), 1)
x5[is.na(x5)] <- most_freq5
drops5 <- c("hnd_webcap")
dat[ , !(names(dat) %in% drops5)]
dat$hnd_webcap <- x5

a6<-c(dat$ownrent)
x6<-factor(a6)
most_freq6<-tail(names(sort(table(x6))), 1)
x6[is.na(x6)] <- most_freq6
drops6 <- c("ownrent")
dat[ , !(names(dat) %in% drops6)]
dat$ownrent <- x6

a7<-c(dat$dwlltype)
x7<-factor(a7)
most_freq7<-tail(names(sort(table(x7))), 1)
x7[is.na(x7)] <- most_freq7
drops7 <- c("dwlltype")
dat[ , !(names(dat) %in% drops7)]
dat$dwlltype <- x7

a8<-c(dat$marital)
x8<-factor(a8)
most_freq8<-tail(names(sort(table(x8))), 1)
x8[is.na(x8)] <- most_freq8
drops8 <- c("marital")
dat[ , !(names(dat) %in% drops8)]
dat$marital <- x8

a9<-c(dat$infobase)
x9<-factor(a9)
most_freq9<-tail(names(sort(table(x9))), 1)
x9[is.na(x9)] <- most_freq9
drops9 <- c("infobase")
dat[ , !(names(dat) %in% drops9)]
dat$infobase <- x9

a10<-c(dat$HHstatin)
x10<-factor(a10)
most_freq10<-tail(names(sort(table(x10))), 1)
x10[is.na(x10)] <- most_freq10
drops10 <- c("HHstatin")
dat[ , !(names(dat) %in% drops10)]
dat$HHstatin <- x10

a11<-c(dat$dwllsize)
x11<-factor(a11)
most_freq11<-tail(names(sort(table(x11))), 1)
x11[is.na(x11)] <- most_freq11
drops11 <- c("dwllsize")
dat[ , !(names(dat) %in% drops11)]
dat$dwllsize <- x11

a12<-c(dat$ethnic)
x12<-factor(a12)
most_freq12<-tail(names(sort(table(x12))), 1)
x12[is.na(x12)] <- most_freq12
drops12 <- c("ethnic")
dat[ , !(names(dat) %in% drops12)]
dat$ethnic <- x12

a13<-c(dat$kid0_2)
x13<-factor(a13)
most_freq13<-tail(names(sort(table(x13))), 1)
x13[is.na(x13)] <- most_freq13
drops13 <- c("kid0_2")
dat[ , !(names(dat) %in% drops13)]
dat$kid0_2 <- x13

a14<-c(dat$kid3_5)
x14<-factor(a14)
most_freq14<-tail(names(sort(table(x14))), 1)
x14[is.na(x14)] <- most_freq14
drops14 <- c("kid3_5")
dat[ , !(names(dat) %in% drops14)]
dat$kid3_5 <- x14

a15<-c(dat$kid6_10)
x15<-factor(a15)
most_freq15<-tail(names(sort(table(x15))), 1)
x15[is.na(x15)] <- most_freq15
drops15 <- c("kid6_10")
dat[ , !(names(dat) %in% drops15)]
dat$kid6_10 <- x15

a16<-c(dat$kid11_15)
x16<-factor(a16)
most_freq16<-tail(names(sort(table(x16))), 1)
x16[is.na(x16)] <- most_freq16
drops16 <- c("kid11_15")
dat[ , !(names(dat) %in% drops16)]
dat$kid11_15 <- x16

a17<-c(dat$kid16_17)
x17<-factor(a17)
most_freq17<-tail(names(sort(table(x17))), 1)
x17[is.na(x17)] <- most_freq17
drops17 <- c("kid16_17")
dat[ , !(names(dat) %in% drops17)]
dat$kid16_17<- x17

a18<-c(dat$creditcd)
x18<-factor(a18)
most_freq18<-tail(names(sort(table(x18))), 1)
x18[is.na(x18)] <- most_freq18
drops18 <- c("creditcd")
dat[ , !(names(dat) %in% drops18)]
dat$creditcd <- x18
dat[] <- lapply(dat, function(x) { 
  x[is.na(x)] <- floor(mean(x, na.rm = TRUE))
  x
})

which(is.na(dat), arr.ind=TRUE)

############################################ CORRELATION ##################################################

CorrMatrix <- cor(numeric_raw)
CorrMatrix[upper.tri(CorrMatrix)] <- 0
diag(CorrMatrix) <- 0
dat <- dat[,!apply(CorrMatrix,2,function(x) any(x > 0.55))]
head(dat)
dat$CHURN <- numeric_raw$churn
dat<-select(dat, -c(churn))

######################################## TRAIN-TEST DATA SPLIT ############################################

index<-sort(sample(nrow(dat),round(.30*nrow(dat))))
training<-dat [-index,]
test<-dat[index,]
training$CHURN <- as.factor(training$CHURN)
test$CHURN <- as.factor(test$CHURN)

######################################## EXPLORATORY DATA ANALYSIS #########################################

###Churn vs Area
counts <- table(dat$CHURN, non_numeric_raw$area)
barplot(counts, main="CHURN VS AREA",las=2, border = 1, cex.lab=1, cex.axis=1, font=1, col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)
title(xlab="Area", line=10, cex.lab=1)

###Churn vs Dualband
co<-table(dat$CHURN,non_numeric_raw$dualband)
barplot(co, main="CHURN VS dualband",xlab="Dualband", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)

### Churn vs Active User Subscription
c<-table(training$CHURN,training$actvsubs)
barplot(c, main="CHURN VS Active Sub",xlab="Active Sub", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)


### Churn vs Total number of months in service
cd<-table(dat$CHURN,original$months)
barplot(cd, main="CHURN VS Months",xlab="months", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)

### Churn vs Annual Income
icome<-table(training$CHURN,training$income)
barplot(icome, main="CHURN VS Income",xlab="months", col=c("darkblue","red"),legend = rownames(counts), beside=TRUE)

########################################## NAIVE BAYES ###################################################

nBayes <- naiveBayes(factor(CHURN)~., data =training[,-1])
category_all<-predict(nBayes,test[,-1]  )
table(NBayes=category_all,Survived=test$CHURN)
NB_wrong<-sum(category_all!=test$CHURN )
NB_error_rate<-NB_wrong/length(category_all)
NB_error_rate

############################################## RANDOM FOREST ##############################################

fit <- randomForest( CHURN~., data=training[,-1], importance=TRUE, ntree=500)
importance(fit)
varImpPlot(fit)
dev.off()
Prediction <- predict(fit, test[,-1])
table(actual=test$CHURN,Prediction)
wrong<- (test$CHURN!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

################################################# CART ###################################################

CART_class<-rpart( CHURN~.,data=training[,-1])
rpart.plot(CART_class)
CART_predict2<-predict(CART_class,test, type="class")
df<-as.data.frame(cbind(test,CART_predict2))
table(Actual=test[,"CHURN"],CART=CART_predict2)
CART_wrong<-sum(test[,"CHURN"]!=CART_predict2)
error_rate=CART_wrong/length(test$CHURN)
error_rate
dev.off()

################################################### C.50 ##################################################

C50_class <- C5.0( CHURN~.,data=training[,-1] )
summary(C50_class )
#plot(C50_class)
#dev.off()
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,"CHURN"],C50=C50_predict)
wrong<- (test[,"CHURN"]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,"CHURN"])
c50_rate

##########################################################################################################







