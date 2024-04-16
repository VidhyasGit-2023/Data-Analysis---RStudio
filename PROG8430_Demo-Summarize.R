##################################################
### PROG8430                                    ##
### Demonstrates some summarization             ## 
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 123456789
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

#Set work directory to an appropriate location
setwd("E:/Canada/Conestoga College/Data Analysis-Math-Algor - PROG8430-23S-Sec2/R Studio Tool")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

##################################################
### Read in Data                                ##
##################################################

#Two Ways to read in Data

#Comma Seperated Values

Elect2008 <- read.csv("Elect2008.csv", header = TRUE, sep = ",")

#R dataset

load("Elect08.Rdata")

str(Elect08)
str(Elect2008)

head(Elect08,10)

##################################################
### Measures of Central Tendency                ##
##################################################

mean(Elect08$Obama)

mean(Elect08$Obama, trim=0.1)

median(Elect08$Obama)

weighted.mean(Elect08$Obama, w=Elect08$Population)

##################################################
### Measures of Dispersion                      ##
##################################################

var(Elect08$Obama)

sd(Elect08$Obama)

mad(Elect08$Obama)

IQR(Elect08$Obama)

range(Elect08$Obama)

quantile(Elect08$Obama) 

quantile(Elect08$Obama, c(.32, .57, .98)) 

summary(Elect08$Obama)

##################################################
### Create Tables                               ##
##################################################

#Create a Variable to see if Obama won a State

Elect08$Win <- "M"
Elect08$Win <- with(Elect08, ifelse(Obama > McCain, "O", Win))
str(Elect08)
Elect08$Win <- as.factor(Elect08$Win)
str(Elect08)

#Frequency Table

Table2 <- table(Elect08$Region, Elect08$Win)
Table2
margin.table(Table2, 1) # A frequencies (summed over B)
margin.table(Table2, 2) # B frequencies (summed over A)

prop.table(Table2) # cell percentages
prop.table(Table2, 1) # row percentages
prop.table(Table2, 2) # column percentages

#States Obama won above a certain amount

Elect08[Elect08$Obama > 65, c("State", "Unemployment", "Income", "Obama")]

#Aggregate Percentage Won by Region

Table1 <- aggregate(Elect08[,3:4], by=list(Elect08$Region), FUN=mean, na.rm=TRUE)
Table1

Table1 <- aggregate(Elect08[,3:5], by=list(Elect08$Region), FUN=mean, na.rm=TRUE)
Table1

##################################################
### Pie Charts                                  ##
##################################################

str(Elect08)

PopSum <- aggregate(Elect08$Population, by=list(Elect08$Region), FUN=sum, na.rm=TRUE)
PopSum
Pop <- PopSum$x
names(Pop) <- PopSum$Group.1 
pie(Pop, main="Total Population in Each Region")

##################################################
### Bar Charts                                  ##
##################################################

#Alaska and Hawaii lack detailed info so remove them

Elect <- Elect08[!(Elect08$State %in% c("Alaska", "Hawaii")), ]

str(Elect)

#Percent Catholic by State

#Calculate the summaries
Cath <- tapply(Elect$Catholic, Elect$State, mean)

#Create the bar plot
barplot(Elect$Catholic, 
        main="Pct Catholic by State", 
        ylab="Pct Catholic")

#Religious Affliation by Region

str(Elect)

Relig <- as.matrix(aggregate(Elect[,9:12], by=list(Elect$Region), FUN=mean))
head(Relig)
rownames(Relig) <- Relig[,1] 
Relig <- Relig[,-1]
head(Relig)

barplot(t(Relig),
        legend=TRUE,
        ylim=c(0,130),
        main="Religious Affiliation by Region", 
        ylab="Pct of each Religious Affiliation")

##################################################
### Histograms                                  ##
##################################################

#Histogram of Percent Support 

hist(Elect$Obama, main="Obama Support")

#Forcing Breaks

hist(Elect08$Obama, breaks=10, main="Obama Support")

#Style type = (count, density, percent)

hist(Elect08$Obama, breaks=12, prob=TRUE,
            main="Obama Percent of Support")

##################################################
### Box Plots                                   ##
##################################################

#Box Plot of McCain Support 

boxplot(Elect08$McCain, data=Elect08, main="Distribution of McCain Support",
       xlab="Percent of McCain Support", horizontal=TRUE, pch=20)

#Comparing by Regions

boxplot(McCain ~ Region, data=Elect08, 
               main="Distribution of McCain Support by Region",
              xlab="Regions",  pch = 20)

# Correlation Coefficient

cor.test(Elect$Turnout, Elect$Income)
#Pearson defaults, but assumes normality

cor.test(Elect$Turnout, Elect$Income, method="spearman")

##################################################
### Scatter Plots                               ##
##################################################

# Basic chart
plot(Turnout ~ Income, data=Elect08, main="Turnout by Income, 2008")

#You can change some formating

plot(Turnout ~ Income, data=Elect08, col=2, pch=20,
       main="Turnout by Income, 2008")

