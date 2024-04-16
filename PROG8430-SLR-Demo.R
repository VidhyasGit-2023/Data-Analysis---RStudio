##################################################
### PROG8430                                    ##
### Simple Linear Regression - Demo             ## 
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

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

##################################################
### Read in Data                                ##
##################################################

# Read "comma separated value" files (".csv")
# Systolic Blood Pressure Dataset
Systolic <- read.csv("Systolic1.csv", header = TRUE, sep = ",")

# Read "comma separated value" files (".csv")
# Thunder Basin Dataset
Thunder <- read.csv("ThunderBasin1.csv", header = TRUE, sep = ",")

##################################################
### Rename and Clean Variables                  ##
##################################################

#Rename Variables to something meaningful

str(Systolic)

names(Systolic) <- c("BP", "Age", "Wgt")

str(Systolic)

str(Thunder)

names(Thunder) <- c("Fwn", "Adt", "Prc", "Sev")

str(Thunder)

##################################################
### Description of Data                         ##
##################################################

#Systolic

SysSum <-stat.desc(Systolic)
format(SysSum,digits=2)

histogram( ~ BP, dat=Systolic, breaks=4, type="count", main="Distribution of BP")
densityplot( ~BP, dat=Systolic, main="Distribution of BP")

histogram( ~ Age, dat=Systolic, breaks=4, type="count", main="Distribution of Age")
densityplot( ~ Age, dat=Systolic, main="Distribution of Age")

histogram( ~ Wgt, dat=Systolic, breaks=4, type="count", main="Distribution of Weight")
densityplot( ~ Wgt, dat=Systolic)

#Thunder Basin

TdrSum <-stat.desc(Thunder)
format(TdrSum,digits=2)

histogram( ~ Fwn, dat=Thunder, breaks=4, type="count", main="Dist of Fawns")
densityplot( ~Fwn, dat=Thunder, main="Dist of Fawns")

histogram( ~ Adt, dat=Thunder, breaks=4, type="count", main="Dist of Adults")
densityplot( ~Adt, dat=Thunder,  main="Dist of Adults")

#histogram( ~ Prc, dat=Thunder, breaks=4, type="count", main="Dist of Precipitation")
#densityplot( ~Prc, dat=Thunder)

#histogram( ~ Sev, dat=Thunder, breaks=4, type="count", main="Dist of Severity")
#densityplot( ~Sev, dat=Thunder)

#Check for Normality (Just for Fun!)

shapiro.test(Systolic$BP)
shapiro.test(Systolic$Age)
#shapiro.test(Systolic$Wgt)

qqnorm(Systolic$BP, main="Is BP Normal?")
qqline(Systolic$BP)

qqnorm(Systolic$Age, main="Is Age Normal?")
qqline(Systolic$Age)

#qqnorm(Systolic$Wgt, main="Is Wgt Normal?")
#qqline(Systolic$Wgt)

#Now for the Thunder Basin Data

shapiro.test(Thunder$Adt)
shapiro.test(Thunder$Fwn)

qqnorm(Thunder$Adt, main="Is Adult Distribution Normal?")
qqline(Thunder$Adt)

qqnorm(Thunder$Fwn, main="Is Fawn Count Normal?")
qqline(Thunder$Fwn)

#Scatter plots of Systolic and Thunder (and correlation)

xyplot(BP ~ Age, data=Systolic, main="Comparing BP to Age")

cor(Systolic$BP, Systolic$Age)
cor.test(Systolic$BP, Systolic$Age) #added details

xyplot(Fwn ~ Adt, data=Thunder, main="Comparing Spring Births")

cor(Thunder$Fwn, Thunder$Adt)
cor.test(Thunder$Fwn, Thunder$Adt)

##################################################
### Create a Model                              ##
##################################################

#Create a Simple Linear Model for each dataset and print specifications

Sysmodel <- lm(BP ~ Age, data=Systolic)

Sysmodel

xyplot(BP ~ Age, data=Systolic, panel = function(x,y) {
  panel.xyplot(x, y)
  panel.abline(Sysmodel)
}, main=list(label="BP by Age (with Regression Line)",
             cex=0.85))

#Now the same for Thunder Basin data

Tdrmodel <- lm(Fwn ~ Adt, data=Thunder)

Tdrmodel

xyplot(Fwn ~ Adt, data=Thunder, panel = function(x,y) {
  panel.xyplot(x, y)
  panel.abline(Tdrmodel)
}, main=list(label="Spring Fawns by Adult Population (with Regression Line)",
             cex=0.85))

#Provide some total measures of fitness

summary(Sysmodel)

summary(Tdrmodel)

#Plot diagnostics for each model

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid

plot(Sysmodel)  # Plot the model information
plot(Tdrmodel)  # Plot the model information

par(mfrow = c(1, 1))  # Return plotting panel to 1 section


