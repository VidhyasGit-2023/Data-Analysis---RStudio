##################################################
### PROG8430                                    ##
### Time Series Demonstration                   ## 
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
#setwd("C:/Users/Marsh/Documents/Data")
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

####################################################
## TIME SERIES  - Quarterly Data                  ##
####################################################

Temperature <- read.csv("Temperature.csv", header=TRUE)
head(Temperature)

#Convert to a Time Series datatype
TempStudy <- ts(Temperature, frequency = 4, start=c(2014,1))
head(TempStudy)
summary(TempStudy)
TempStudy

###PLOT THE TIME SERIES ####

plot.ts(TempStudy, main="Average Temperature - Waterloo",  ylim = c(-15, 25)) 

### Decompose and check for Autocorrelation

decompTemp <- decompose(TempStudy, type="additive")  #Could be mult for multiplicative
decompTemp
plot(decompTemp)

#boxplot(TempStudy~cycle(TempStudy))

#acf(TempStudy)

adf.test(TempStudy) # p-value < 0.05  indicated series is stationary: note - can set the lags, k=n

#Deseasonalize

TempStudy_Seas_Adj <- TempStudy - decompTemp$seasonal

plot.ts(TempStudy_Seas_Adj, main="Deseasonalized - Average Temperature - Waterloo", ylim = c(-15, 25))

#####################################################
## TIME SERIES  - Annual                           ##
#####################################################

#Source: BCCAQv2(Annual)
Temperature <- read.csv("Waterloo_Climate_Annual.csv", header=TRUE)
head(Temperature)
Temperature <- Temperature[c(-1)]
head(Temperature)

TempStudy <- ts(Temperature, frequency = 1, start=c(1951))  #Converts to Time Series
head(TempStudy)  
stat.desc(TempStudy)

###PLOT THE TIME SERIES ####

plot.ts(TempStudy, main="Average Temperature - Waterloo")

#Spot trends by smoothing

TempStudySMA10 <- SMA(TempStudy,n=10)
plot.ts(TempStudySMA10)

adf.test(TempStudy) # p-value < 0.05  indicated series is stationary

acf(TempStudy)   #Autocorrelations

#Moving Average Forecast- Let's build a forecast

move_avg <- sma(TempStudy)
move_avg
move_avg <- forecast(move_avg, h=5,level=0.80)   #h - periods to forecast; level=Prediction Level
move_avg
plot(move_avg)

#Exponential Smoothing Forecast

ES_avg <- es(TempStudy)
ES_avg
ES_avg <- forecast(ES_avg, h=5,level=0.80)
ES_avg
plot(ES_avg)

