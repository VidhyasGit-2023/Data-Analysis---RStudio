##################################################
### PROG8430                                    ##
### K-Means Clustering                          ## 
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
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

if(!require(cluster)){install.packages("cluster")}
library("cluster")

if(!require(factoextra)){install.packages("factoextra")}
library("factoextra")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read R Data file (".Rdata")
# represents the percentage of income devoted each expense category

load("PROG8430_Clst_21F.Rdata")

head(PROG8430_Clst_21F,10)

###################################################
## Standardize Data                              ##
###################################################

#Create a quick standardization function
norm01 <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#Standardizing Food

PROG8430_Clst_21F$Food_MinMax <- norm01(PROG8430_Clst_21F$Food)

#Standardizing Entr

PROG8430_Clst_21F$Entr_MinMax <- norm01(PROG8430_Clst_21F$Entr)

#Standardizing Education

PROG8430_Clst_21F$Educ_MinMax <- norm01(PROG8430_Clst_21F$Educ)

#Standardizing Transport

PROG8430_Clst_21F$Trans_MinMax <- norm01(PROG8430_Clst_21F$Tran)

#Standardizing Work

PROG8430_Clst_21F$Work_MinMax <- norm01(PROG8430_Clst_21F$Work)

#Standardizing Housing

PROG8430_Clst_21F$Hous_MinMax <- norm01(PROG8430_Clst_21F$Hous)

#Standardizing Other

PROG8430_Clst_21F$Other_MinMax <- norm01(PROG8430_Clst_21F$Othr)

head(PROG8430_Clst_21F)
summary(PROG8430_Clst_21F)
stat.desc(PROG8430_Clst_21F)
str(PROG8430_Clst_21F)

ExpClstrData <- PROG8430_Clst_21F[c(8,12)]   
str(ExpClstrData)

# Create graphical summaries of the data (as demonstrated in class: boxplots or histograms) and comment on any observations you make.

#Box Plot of Food

boxplot(PROG8430_Clst_21F$Food, data=PROG8430_Clst_21F, main="Distribution of Expenses against Food Category",
        xlab="Percent of Exp spent on Food", horizontal=TRUE, pch=20)

#Box Plot of Entr

boxplot(PROG8430_Clst_21F$Entr, data=PROG8430_Clst_21F, main="Distribution of Expenses against Entr Category",
        xlab="Percent of Exp spent on Entr", horizontal=TRUE, pch=20)

#Box Plot of Educ

boxplot(PROG8430_Clst_21F$Educ, data=PROG8430_Clst_21F, main="Distribution of Expenses against Educ Category",
        xlab="Percent of Exp spent on Educ", horizontal=TRUE, pch=20)

#Box Plot of Tran

boxplot(PROG8430_Clst_21F$Tran, data=PROG8430_Clst_21F, main="Distribution of Expenses against Tran Category",
        xlab="Percent of Exp spent on Tran", horizontal=TRUE, pch=20)

#Box Plot of Work

boxplot(PROG8430_Clst_21F$Work, data=PROG8430_Clst_21F, main="Distribution of Expenses against Work Category",
        xlab="Percent of Exp spent on Work", horizontal=TRUE, pch=20)

#Box Plot of Hous

boxplot(PROG8430_Clst_21F$Hous, data=PROG8430_Clst_21F, main="Distribution of Expenses against Hous Category",
        xlab="Percent of Exp spent on Hous", horizontal=TRUE, pch=20)

#Box Plot of Othr

boxplot(PROG8430_Clst_21F$Othr, data=PROG8430_Clst_21F, main="Distribution of Expenses against Othr Category",
        xlab="Percent of Exp spent on Othr", horizontal=TRUE, pch=20)


# Create segmentation/cluster schemes for k=3,4,5,6,7

###################################################
## Create the Clusters                          ##
###################################################

#K = 3

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=3, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:3), ClstrExp$centers)

#K = 4

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=4, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:4), ClstrExp$centers)

#K = 5

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=5, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:5), ClstrExp$centers)


#K = 6

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=6, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:6), ClstrExp$centers)

#K = 7

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=7, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:7), ClstrExp$centers)

#Create the WSS plots as demonstrated in class and select a suitable k value based on the “elbow”.

fviz_nbclust(ExpClstrData, kmeans, method = "wss")

# Based on the “k” chosen above, create a scatter plot showing the clusters and colour-coded datapoints for each of “k-1”, “k”, “k+1”.

#K = 2

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=2, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:2), ClstrExp$centers)

ggplot(data=PROG8430_Clst_21F, aes(x=Food_MinMax, y=Work_MinMax, color=cluster)) + geom_point()

#K = 3

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=3, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:3), ClstrExp$centers)

ggplot(data=PROG8430_Clst_21F, aes(x=Food_MinMax, y=Work_MinMax, color=cluster)) + geom_point()

#K = 4

ClstrExp <- kmeans(ExpClstrData, iter.max=10, centers=4, nstart=10)
ClstrExp

PROG8430_Clst_21F$cluster <- factor(ClstrExp$cluster)   # Adding Cluster tags to variables
head(PROG8430_Clst_21F)

centers <- data.frame(cluster=factor(1:4), ClstrExp$centers)

ggplot(data=PROG8430_Clst_21F, aes(x=Food_MinMax, y=Work_MinMax, color=cluster)) + geom_point()

# Create summary tables for the segmentation/clustering scheme

ExpSUM <- PROG8430_Clst_21F %>% 
  group_by(cluster) %>% 
  summarise(Food = mean(Food), Entr = mean(Entr), Educ=mean(Educ), Tran=mean(Tran), Work=mean(Work),
            Hous=mean(Hous), Othr=mean(Othr), N=n())

ExpSUM

