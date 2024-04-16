##################################################
### PROG8430                                    ##
### Inference                                   ##
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

#Set work directory
#setwd("C:/Users/David/Documents/Data")
setwd("E:/Canada/Conestoga College/Data Analysis-Math-Algor - PROG8430-23S-Sec2/R Studio Tool")

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(HSAUR)){install.packages("HSAUR")}
library("HSAUR")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")

if(!require(vcd)){install.packages("vcd")}
library("vcd")

##################################################
### Read in Data                                ##
##################################################

#Read in Water Data

head(water)

#SUMMARY STATS ON DATA

DescWater <- stat.desc(water[3:4])
round(DescWater,2)

summary(water)

# SOME GRAPHICAL SUMMARY
# Notice - This uses lattice. 
# The commented out code is base R

histogram( ~ mortality, dat=water, breaks=12, type="density", xlab="Mortality", main="Distribution of Mortality")
densityplot( ~mortality, dat=water)

histogram( ~ hardness, dat=water, breaks=12, type="density", xlab="Hardness", main="Distribution of Water Hardness")
densityplot( ~ hardness, dat=water)

#hist(water$hardness, breaks=12, xlab="Water Hardness", freq=FALSE, main="Water Hardness")
#lines(density(water$hardness), lwd=3, col="blue")

#hist(water$mortality, breaks=12, xlab="Mortality", main="Mortality", freq=FALSE)
#lines(density(water$mortality), lwd=3, col="blue")

#ARE THEY NORMAL?

shapiro.test(water$hardness)
shapiro.test(water$mortality)

qqnorm(water$hardness, main="Is Hardness Normal?")
qqline(water$hardness)

qqnorm(water$mortality, main="Is Mortality Normal?")
qqline(water$mortality)

# SO THERE IS A CONNECTION BETWEEN MORTALITY AND HARDNESS

# IS THERE A CONNECTION BETWEEN MORTALITY OR HARDNESS AND LOCATION (I.E.)
# NORTH AND SOUTH?

#xyplot(hardness ~ mortality, data=water, groups= location, col = c("blue", "red"), main="Comparing Water Hardness to Mortality")

bwplot(hardness ~ location, data=water, 
       main="Hardness by Location",
       xlab="Regions",  pch = '|')

bwplot(mortality ~ location, data=water, 
       main="Mortality by Location",
       xlab="Regions",  pch = '|')

#Can this be quantified?

# Assumptions for non-paired t-test:
# 1. Independant
# 2. Normally Distributed - Shapiro-Wilks - as above
# 3. Equal Variance - F-test

#Mortality is normal so let's use a t-test

#Check for Variance North and South

res.ftest <- var.test(mortality ~ location, data = water)
res.ftest

# p-value > 0.05, therefore no significant difference in variances.

# T-Test 

res <- t.test(mortality ~ location, data = water, var.equal = TRUE)
res

# Strong Evidence that the differences are significant

#For Hardness, we need non-parametric tests

res <- wilcox.test(hardness ~ location, data = water,
                   exact = FALSE)
res

# Strong evidence that it is different.

#ARE THEY CORRELATED?

ggplot(data = water, aes(x = hardness, y = mortality, color  = location)) + 
  geom_point() + labs(y = "averaged annual mortality per 100,000 males", 
                      x = "calcium concentration (in parts per million)")

xyplot(hardness ~ mortality, data=water, main="Comparing Water Hardness to Mortality")


cor.test(water$hardness, water$mortality)
#Pearson defaults, but assumes normalacy

cor.test(water$hardness, water$mortality, method="spearman")
#Speaman is non-parametric and therefore makes no normalacy assumption

################################
## Comparing Categorical Data ##
################################

data(pistonrings)

pistonrings

#Total ChiSq Test
chisq.test(pistonrings)

#Calculating the Residuals
chisq.test(pistonrings)$residuals

#Graphing the Residuals

assoc(pistonrings)











