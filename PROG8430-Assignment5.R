##################################################
### PROG8430                                    ##
### Classification                              ## 
##################################################
#                                               ##
##################################################
# Written by Vidhya Venugopal
# ID: 8902970
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("E:/Canada/Conestoga College/Data Analysis-Math-Algor - PROG8430-23S-Sec2/R Studio Tool")

options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

#For ROC Curves

if(!require(pROC)){install.packages("pROC")}
library(pROC)


#For N-B Analysis

if(!require(klaR)){install.packages("klaR")}
library("klaR")

# For LDA

if(!require(MASS)){install.packages("MASS")}
library("MASS")

##############################
## Read in Data             ##
##############################

load("Tumor_21F.Rdata")

head(Tumor_21F)

###################################################
## Find Outliers                                 ##
###################################################

summary(Tumor_21F)

# there are no outliers in the provided tumor dataset

#########################################
## Checking Correlations               ##
#########################################

######################
##Brain    ####
######################

TumCorr <- cor(Tumor_21F, method="spearman")
round(TumCorr, 2) 

Tbl_Rct <- table(Tumor_21F$Out, Tumor_21F$Brain, dnn=list("Tumor","Brain"))
Tbl_Rct
prop.table(Tbl_Rct, 2) # col percentages

#Check the Chi Squared Test - NOTE Removal of Yate's Continuity Correction

chisqRct <- chisq.test(Tumor_21F$Out, Tumor_21F$Brain, correct=FALSE)      
chisqRct

chisqRct$observed   # What we observed
chisqRct$expected   # If there were no relationship


######################
##Marrow    ####
######################

Tbl_Rct <- table(Tumor_21F$Out, Tumor_21F$Marrow, dnn=list("Tumor","Brain"))
Tbl_Rct
prop.table(Tbl_Rct, 2) # col percentages

#Check the Chi Squared Test - NOTE Removal of Yate's Continuity Correction

chisqRct <- chisq.test(Tumor_21F$Out, Tumor_21F$Marrow, correct=FALSE)      
chisqRct

chisqRct$observed   # What we observed
chisqRct$expected   # If there were no relationship


##################################################
### Building the Model                          ##
##################################################

#Full Model = User Model 1

tum_glm = glm(Out ~ Age + Sex + Bone + Marrow + Lung + Pleura + Liver + Brain + Skin + Neck + Supra + Axil + Media,
              family="binomial", data=Tumor_21F, na.action=na.omit)

tum_glm
summary(tum_glm)

#Manual Adjusted Model (Dropping Bone, Pleura, Liver and Axil) = User Model 2

tum_Man = glm(Out ~ Age + Sex + Marrow + Lung + Brain + Neck + Supra + Media
              ,
              family="binomial", data=Tumor_21F, na.action=na.omit)

summary(tum_Man)

#A Criteria Selection Model

stp_tum_glm <- step(tum_glm)

summary(stp_tum_glm)

#############################################
## Check the Full Model = User Model 1
#############################################

resp_FM <- predict(tum_glm, type="response")   # creates probabilities
head(resp_FM,20)
Class_FM <- ifelse(resp_FM > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely to get Tumor)
head(Class_FM)
True_log <- Tumor_21F$Out                        #Creates a vector of the true outcomes
T1 <- table(True_log, Class_FM, dnn=list("Act Tumor","Predicted") )  # Creates a Contingency Table
#Note - Could have just used Tumor_21F$Out
T1

#Create True Positive, Negatives, etc.

TP <- T1[2,2]
TN <- T1[1,1]
FP <- T1[1,2]
FN <- T1[2,1]

# Calculates Accuracy

Acc <- (TP + TN)/sum(T1)
Acc

# Calculates Specificity 

Spec <- TN/(TN +FP)
Spec

# Calculates Precision 

Prec <- TP/(TP + FP)
Prec

# Calculates Sensitivity

Sens <- TP/sum(T1[2,])
Sens


##################################################################
## Check the Manual Adjusted Model (Dropping Skin) = User Model 2
##################################################################

resp_MAM <- predict(tum_Man, type="response")   # creates probabilities
head(resp_MAM,20)
Class_MAM <- ifelse(resp_MAM > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely to get Tumor)
head(Class_MAM)
True_log <- Tumor_21F$Out                        #Creates a vector of the true outcomes
T1 <- table(True_log, Class_MAM, dnn=list("Act Tumor","Predicted") )  # Creates a Contingency Table
#Note - Could have just used Tumor_21F$Out
T1

#Create True Positive, Negatives, etc.

TP <- T1[2,2]
TN <- T1[1,1]
FP <- T1[1,2]
FN <- T1[2,1]

# Calculates Accuracy

Acc <- (TP + TN)/sum(T1)
Acc

# Calculates Specificity 

Spec <- TN/(TN +FP)
Spec

# Calculates Precision 

Prec <- TP/(TP + FP)
Prec

# Calculates Sensitivity

Sens <- TP/sum(T1[2,])
Sens

#ROC Curve (and Area Under the Curve) - Full Model = User Model 1

plot(roc(Tumor_21F$Out,resp_FM, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Tumor Prediction')

auc(Tumor_21F$Out, resp_FM)

#ROC Curve (and Area Under the Curve) - Manual Adjusted Model (Dropping Skin) = User Model 2

plot(roc(Tumor_21F$Out,resp_MAM, direction="<"),
     col="red", lwd=2, main='ROC Curve for Logistic, Tumor Prediction')

auc(Tumor_21F$Out, resp_MAM)

##################################################
## Part B 1. Logistic Regression – Stepwise       ####
### Building a Stepwise Model                   ##
##################################################
  
#stepwise - A Criteria Selection Model

start_time <- Sys.time()

tum_glm = glm(Out ~ Age + Sex + Bone + Marrow + Lung + Pleura + Liver + Brain + Skin + Neck + Supra + Axil + Media,
              family="binomial", data=Tumor_21F, na.action=na.omit)

stp_tum_glm <- step(tum_glm)

summary(stp_tum_glm)

## Check the stepwise Model

resp_SW <- predict(stp_tum_glm, type="response")   # creates probabilities
head(resp_SW,20)
Class_SW <- ifelse(resp_SW > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely to donate)
head(Class_SW)
True_log <- Tumor_21F$Out                        #Creates a vector of the true outcomes
T1 <- table(True_log, Class_SW, dnn=list("Act Tumor","Predicted") )  # Creates a Contingency Table
#Note - Could have just used Blood$Donate
T1

# the time (in seconds) it took to fit the model

end_time <- Sys.time()

SW_Time <- end_time - start_time

SW_Time

##################################################
### NAIVE BAYES                                 ##
##################################################

str(Tumor_21F)

Tumor_21F$Out <- as.factor(Tumor_21F$Out)

start_time <- Sys.time()

Tumor_Naive <- NaiveBayes(Out ~ Age + Sex + Bone + Marrow + Lung + Pleura + Liver + Brain + Skin + Neck + Supra + Axil + Media
                          , 
                          data = Tumor_21F, na.action=na.omit)

#Classifies
pred_bay <- predict(Tumor_Naive,Tumor_21F)

#Creates Confusion Matrix
CF_NB <- table(Actual=Tumor_21F$Out, Predicted=pred_bay$class)
CF_NB

#calculate the time (in seconds) it took to fit the model

end_time <- Sys.time()

NB_Time <- end_time - start_time

NB_Time

##################################################
### LDA                                         ##
##################################################

str(Tumor_21F)

Tumor_21F$Out <- as.factor(Tumor_21F$Out)

Tumor_21F$Out

start_time <- Sys.time()

Tumor_Discrim <- lda(Out ~ Age + Sex + Bone + Marrow + Lung + Pleura + Liver + Brain + Skin + Neck + Supra + Axil + Media,
                     data = Tumor_21F, na.action=na.omit)

#Classifies
pred_dis <- predict(Tumor_Discrim, data=Tumor_21F)

#Confusion Matrix
CF_LDA <- table(Actual=Tumor_21F$Out, Predicted=pred_dis$class)
CF_LDA

end_time <- Sys.time()

LDA_Time <- end_time - start_time
LDA_Time


#Comparing all three
# A Criteria Selection Model
#Create True Positive, Negatives, etc.

TP <- T1[2,2]
TN <- T1[1,1]
FP <- T1[1,2]
FN <- T1[2,1]

# Calculates Accuracy

Acc_T1 <- (TP + TN)/sum(T1)

# Calculates Specificity 

Spec_T1 <- TN/(TN +FP)

# Calculates Precision 

Prec_T1 <- TP/(TP + FP)

# Calculates Sensitivity

Sens_T1 <- TP/sum(T1[2,])

T1
Acc_T1
Spec_T1
Prec_T1
Sens_T1
SW_Time

# Naïve-Bayes Classification
#Create True Positive, Negatives, etc.

TP <- CF_NB[2,2]
TN <- CF_NB[1,1]
FP <- CF_NB[1,2]
FN <- CF_NB[2,1]

# Calculates Accuracy

Acc_NB <- (TP + TN)/sum(T1)

# Calculates Specificity 

Spec_NB <- TN/(TN +FP)

# Calculates Precision 

Prec_NB <- TP/(TP + FP)

# Calculates Sensitivity

Sens_NB <- TP/sum(T1[2,])

CF_NB
Acc_NB
Spec_NB
Prec_NB
Sens_NB
NB_Time

# Linear Discriminant Analysis

#Create True Positive, Negatives, etc.

TP <- CF_LDA[2,2]
TN <- CF_LDA[1,1]
FP <- CF_LDA[1,2]
FN <- CF_LDA[2,1]

# Calculates Accuracy

Acc_LDA <- (TP + TN)/sum(T1)

# Calculates Specificity 

Spec_LDA <- TN/(TN +FP)

# Calculates Precision 

Prec_LDA <- TP/(TP + FP)

# Calculates Sensitivity

Sens_LDA <- TP/sum(T1[2,])

CF_LDA
Acc_LDA
Spec_LDA
Prec_LDA
Sens_LDA
LDA_Time
