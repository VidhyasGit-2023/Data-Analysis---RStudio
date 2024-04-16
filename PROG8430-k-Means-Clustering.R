##################################################
### PROG8430                                    ##
### K-Means Clustering                          ## 
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

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read text file (".txt")
# Denver Crime data sheet
Denver <- read.delim("DenverCrime2.txt", header=TRUE)
quantile(Denver$Crime,.95)
Denver$Crime <- with(Denver, ifelse(Crime>700, 105 , Crime))   #Adjusting for bad data
str(Denver)

###################################################
## Preliminary data transformation               ##
###################################################

#Year - Convert to a Date
Denver$Year <- as.Date(Denver$Year,format="%d/%m/%Y")
head(Denver)
#Converts to Days Since Jan 1, 1970
Denver$Year <- julian(Denver$Year)+10000
head(Denver)

#Convert Income into Numeric
Denver$Income <- as.numeric(gsub('[$,]', '', Denver$Income))

str(Denver)   #Check Results

###################################################
## Standardize Data                              ##
###################################################

#Create a quick standardization function
norm01 <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#A different standardization function
normn <- function(x) {
  return ((x-mean(x))/sd(x))
}

#Standardizing Population

Denver$Pop_MinMax <- norm01(Denver$Pop)
Denver$Pop_Normx <- normn(Denver$Pop)

#Standardizing Crime

Denver$Crime_MinMax <- norm01(Denver$Crime)
Denver$Crime_Normx <- normn(Denver$Crime)
head(Denver)
summary(Denver)
stat.desc(Denver)
str(Denver)

DenverClstrData <- Denver[c(12,14)]   
str(DenverClstrData)


###################################################
## Create the Clusters                          ##
###################################################

ClstrDen <- kmeans(DenverClstrData, iter.max=10, centers=3, nstart=10)
ClstrDen

Denver$cluster <- factor(ClstrDen$cluster)   # Adding Cluster tags to variables
head(Denver)

centers <- data.frame(cluster=factor(1:3), ClstrDen$centers)

###################################################
## Plotting the Clusters                        ##
###################################################


ggplot(data=Denver, aes(x=Pop_Normx, y=Crime_Normx, color=cluster)) + geom_point()

ggplot(data=Denver, aes(x=Pop_Normx, y=Crime_Normx, color=cluster, shape=cluster)) + 
  geom_point(alpha=.8) +
  geom_point(data=centers, aes(x=Pop_Normx, y=Crime_Normx), size=3, stroke=2)


DenverSUM <- Denver %>% 
  group_by(cluster) %>% 
  summarise(Pop = mean(Pop), PopChg = mean(Change), Child=mean(Child), Lunch=mean(Lunch), IncChg=mean(ChgInc),
            Income=mean(Income), Crime=mean(Crime), N=n() )

DenverSUM
