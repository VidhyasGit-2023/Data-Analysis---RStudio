##################################################
### PROG8430                                    ##
### Data Reduction Demonstration                ##
### Demonstration                               ##  
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 8643279
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

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

##############################
## Read in Data             ##
##############################

options(digits=5)

Master <- read.csv("PROG8430_Dimension_Data_Demo.csv", header = TRUE, sep = ",")

head(Master)

##############################
## Reduce Number of Rows    ##
##############################

# Find missing values
# Identify cols > 99% missing

summary(Master)

#Look like X02 is likely!

Master <- Master[-c(3)]

head(Master,8)

# Identify Low Variance

stat.desc(Master)  #Consider coef of var
summary(Master)

#Based on the above X04 seem likely. Let's check!

table(Master$X04)

Master <- Master[-c(4)]

head(Master,7)

#Identify High Correlation

cor(Master,method="spearman")

#X05 and X06 seem highly correlated
#Don't need to keep both, I'll drop the second one.

Master <- Master[-c(5)]

head(Master,3)

#########################
#Test for Normality   ###
#########################

shapiro.test(Master$X01)
qqnorm(Master$X01, pch=46)
qqline(Master$X01)
densityplot( ~X01, dat=Master, pch=46)

shapiro.test(Master$X03)
qqnorm(Master$X03, pch=46)
qqline(Master$X03)
densityplot( ~X03, dat=Master, pch=46)

shapiro.test(Master$X05)
qqnorm(Master$X05, pch=46)
qqline(Master$X05)
densityplot( ~X05, dat=Master, pch=46)

shapiro.test(Master$X07)
qqnorm(Master$X07, pch=46)
qqline(Master$X07)
densityplot( ~X07, dat=Master, pch=46)

shapiro.test(Master$X08)
qqnorm(Master$X08, pch=46)
qqline(Master$X08)
densityplot( ~X08, dat=Master, pch=46)

#################################
## Let's Try Tukey's Ladder   ###
## For Non-Normal Data        ###
#################################

#X03

for(i in -3:3) 
{ Master$Tmp <- Master$X03^(i)
if(i==0) Master$Tmp <- log(Master$X03)
message("i=",i)
print(shapiro.test(Master$Tmp))
}

# Looks like nothing on Tukey's ladder will help. :(

#X07

for(i in -3:3) 
{ Master$Tmp <- Master$X07^(i)
if(i==0) Master$Tmp <- log(Master$X07)
message("i=",i)
print(shapiro.test(Master$Tmp))
}

# Again, looks like nothing on Tukey's ladder will help. :(

#X08

for(i in -3:3) 
{ Master$Tmp <- Master$X08^(i)
if(i==0) Master$Tmp <- log(Master$X08)
message("i=",i)
print(shapiro.test(Master$Tmp))
}

# Looks like a log tranform!

Master$X08T <- log(Master$X08)

# Test to make sure it worked

shapiro.test(Master$X08T)
qqnorm(Master$X08T, pch=46)
qqline(Master$X08T)
densityplot( ~X08T, dat=Master, pch=46)

###################################
## TEST AND TRAINING SAMPLES     ##
###################################

#Now Let's split in to test and training samples
#75% Sample

Inter = sort(sample(nrow(Master), nrow(Master)*.75))

#creating training data set by selecting the output row values
train<-Master[Inter,]

#creating test data set by not selecting the output row values
test<-Master[-Inter,]

#Compare - Do they look similar?
#Look at Q1, Q2, Q3

summary(train)
stat.desc(train)

summary(test)
stat.desc(test)

# They will be different as it is different set of data s







