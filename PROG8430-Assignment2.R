##################################################
### PROG8430                                    ##
### ASSIGNMENT - 1                              ## 
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

if(!require(tseries)){install.packages("tseries")}
library("tseries")

if(!require(TTR)){install.packages("TTR")}
library("TTR")

if(!require(smooth)){install.packages("smooth")}
library("smooth")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

#R dataset

load("PROG8430_Assign_Explore-23S.Rdata")

head(PROG8430_Assign_Explore,10)

#Create a QQ Normal plot of the Political Awareness Test Score.

qqnorm(PROG8430_Assign_Explore$score, pch = 1, frame = FALSE, main="Is Political Awarness Test Score Normal")
qqline(PROG8430_Assign_Explore$score)

shapiro.test(PROG8430_Assign_Explore$score)

#Compare Political Awareness Test Scores between the treatment and control group using a suitable hypothesis test

#Check for Variance Treat & Control Groups

res.ftest <- var.test(PROG8430_Assign_Explore$score ~ PROG8430_Assign_Explore$group, data = PROG8430_Assign_Explore, var.equal = TRUE)

res.ftest

res <- t.test(PROG8430_Assign_Explore$score ~ PROG8430_Assign_Explore$group, data = PROG8430_Assign_Explore, var.equal = TRUE)

res

# Determine if the Score on the Political Awareness Test varies by Region using ANOVA (statistical) and a sequence of boxplots (graphical)

Score <- summary(aov(PROG8430_Assign_Explore$score ~ PROG8430_Assign_Explore$nation), data = PROG8430_Assign_Explore)

Score

#Comparing by Region using boxplots

boxplot(PROG8430_Assign_Explore$score ~ PROG8430_Assign_Explore$nation, data=PROG8430_Assign_Explore,
        main="Score on the Political Awareness Test varies by Region",
        xlab="Nation", ylab = "Score on the Political Awareness Test", range=0)


# Determine if the Measure of Political Involvement (Pol) varies by Political Affiliation using ANOVA and a sequence of boxplots 

Pol <- summary(aov(PROG8430_Assign_Explore$Pol ~ PROG8430_Assign_Explore$nation), data = PROG8430_Assign_Explore)

Pol

#Comparing by Region using boxplots

boxplot(PROG8430_Assign_Explore$Pol ~ PROG8430_Assign_Explore$nation, data=PROG8430_Assign_Explore,
        main="Measure of Political Involvement varies by Region",
        xlab="Nation", ylab = "Measure of Political Involvement", range=0)

####################################################
## TIME SERIES                                    ##
####################################################

#Load Temperature R data

load("Ayr_21F.Rdata")
head(Ayr_21F)

#Convert to a Time Series datatype
TempStudy <- ts(Ayr_21F, frequency=2, start=1968, end=2003)
head(TempStudy)

#Summarize the information (mean, std dev, etc.)
stat.desc(TempStudy)

#Plot the time series data

plot.ts(TempStudy, main="Average Temperature in Canada")

#Decompose the times series data in to the constituent components

decompTemp <- decompose(TempStudy)

decompTemp

plot(decompTemp)

adf.test(TempStudy)

#Deseasonalize the information and plot the result

TempStudy_Seasonal <- TempStudy - decompTemp$seasonal
  
plot.ts(TempStudy_Seasonal,
        main="Deseasonalized - Average Temperature in Canada")


TempStudy_trend <- TempStudy - decompTemp$trend

plot.ts(TempStudy_trend,
        main="Deseasonalized - Average Temperature in Canada")

TempStudy_random <- TempStudy - decompTemp$random

plot.ts(TempStudy_random,
        main="Deseasonalized - Average Temperature in Canada")

#Smooth the temperature chart using a moving average.Try 3 different values for the moving average

# n=5
TempStudy_Smoothing_Avg5 <- SMA(TempStudy, n=5)

plot.ts(TempStudy_Smoothing_Avg5)

# n=8

TempStudy_Smoothing_Avg8 <- SMA(TempStudy,n=8)

plot.ts(TempStudy_Smoothing_Avg8)

# n=15

TempStudy_Smoothing_Avg15 <- SMA(TempStudy,n=15)

plot.ts(TempStudy_Smoothing_Avg15)

# Determine if the time series is stationary

adf.test(TempStudy) # p-value < 0.05 indicated series iscstationary

# Create an autocorrelation chart (using acf) and comment on which lags are significant. Do previous values seem to influence current values?

acf(TempStudy)

# Create a simple moving average forecast of temperature in Ayr for five years beyond the data provided. Graph your results along with a 75% prediction interval.

TempStudy_move_avg <- sma(TempStudy)
TempStudy_move_avg

TempStudy_move_avg <- forecast(TempStudy_move_avg, h=8,level=0.75)
TempStudy_move_avg
plot(TempStudy_move_avg)

# Create an exponentially smoothed forecast of temperature in Ayr for five years beyond the data provided. Graph your results along with a 75% prediction interval

TempStud_ES_avg <- es(TempStudy)
TempStud_ES_avg

TempStud_ES_avg <- forecast(TempStud_ES_avg, h=8,level=0.75)
TempStud_ES_avg
plot(TempStud_ES_avg)