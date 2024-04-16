##################################################
### PROG8430                                    ##
### ANOVA Demonstration                         ##
##################################################
#                                               ##
##################################################
# Written by David Marsh
# ID: 123456
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

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#### Create Data

set.seed(179)

Unit <- c(1:200)
Page <- rep(1:4,each=50)
User <- rep(1:5, times=40)
df <- data.frame(cbind(Unit,Page,User))
df
set.seed(177)
df$Time <- ifelse(Page==1,rnorm(50,165,3),1)
df$Time <- ifelse(Page==2,rnorm(50,175,3),df$Time)
df$Time <- ifelse(Page==3,rnorm(50,170,3),df$Time)
df$Time <- ifelse(Page==4,rnorm(50,160,3),df$Time)
df

df$Page <- as.factor(df$Page)
df$User <- as.factor(df$User)
df

#Comparing Pages

boxplot(Time ~ Page, data=df,
        main="Time spent on each page (secs)",
        range=0)

#Comparing Users

boxplot(Time ~ User, data=df,
        main="Time spent on each page (secs)",
        range=0)

#One-Way ANOVA

summary(aov(Time ~ Page, data=df))
summary(aov(Time ~ User, data=df))

ANOVA_page <- aov(Time~Page, data=df)
summary(ANOVA_page)

TukeyHSD(ANOVA_page)

#Two-Way ANOVA

anova_two_page <- aov(Time~Page + User, data = df)
summary(anova_two_page)