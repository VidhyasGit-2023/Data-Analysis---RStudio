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

#R dataset

load("PROG8430_Assign_Explore-23S.Rdata")

head(PROG8430_Assign_Explore,10)

# Table to display the total income by each category of marital status

#Aggregate total income by each category

Income_Table <- aggregate(PROG8430_Assign_Explore$income, by=list(PROG8430_Assign_Explore$m.status), FUN=sum, a.rm=TRUE)

Income_Table

#Calculate the mean age of respondents born in Asia
options(digits = 4)

mean(PROG8430_Assign_Explore[PROG8430_Assign_Explore$nation == 'Asia', 'age'], trim=0.1)

#Calculate the mean age of respondents born in Asia weighted by the number of children they have
library(dplyr)

Explore_summary <- 
  PROG8430_Assign_Explore %>%
  group_by(nation) %>% 
  summarise(weighted_income = weighted.mean(age, n.child))

Explore_summary$nation
round(Explore_summary$weighted_income, digits = 2)

#Create a table to show the mean score on the political awareness test for males compared to females.

#Aggregate political test by each gender

Political_Table <- aggregate(PROG8430_Assign_Explore$score, by=list(PROG8430_Assign_Explore$gender), FUN=mean, a.rm=TRUE)

Political_Table

#Calculate the 34th and 63rd percentiles of percentage of time taken on the test.

quantile(PROG8430_Assign_Explore$time1, probs=c(.34, .63))

##################################################
### Pie Charts                                  ##
##################################################

str(PROG8430_Assign_Explore)

PopSum <- aggregate(PROG8430_Assign_Explore$nation, by=list(PROG8430_Assign_Explore$political), FUN=length)
PopSum
Pop <- PopSum$x
names(Pop) <- PopSum$Group.1 
pie(Pop, main="No of Respondents against Each Political Affliation")

#Create a table that shows the percentage of respondents from each Region that are in the Treatment group

#Create a Variable to see if group

PROG8430_Assign_Explore$Tgroup <- "t"
PROG8430_Assign_Explore$Tgroup <- with(PROG8430_Assign_Explore, ifelse(PROG8430_Assign_Explore$group == "control", "c", Tgroup))
str(PROG8430_Assign_Explore)
PROG8430_Assign_Explore$Tgroup <- as.factor(PROG8430_Assign_Explore$Tgroup)
str(PROG8430_Assign_Explore)

#Treatment Group Table

TGroup_Table <- table(PROG8430_Assign_Explore$nation, PROG8430_Assign_Explore$Tgroup)
TGroup_Table
margin.table(TGroup_Table, 1) # A frequencies (summed over B)
margin.table(TGroup_Table, 2) # B frequencies (summed over A)

prop.table(TGroup_Table) # cell percentages

##################################################
### Bar Charts                                  ##
##################################################

#the mean Score on the Political Awareness Test for each Region

Test1_Score <- as.matrix(aggregate(PROG8430_Assign_Explore[,14], by=list(PROG8430_Assign_Explore$nation), FUN=mean))
head(Test1_Score)
rownames(Test1_Score) <- Test1_Score[,1] 
Test1_Score <- Test1_Score[,-1]
head(Test1_Score)

barplot(t(Test1_Score),
        legend=TRUE,
        ylim=c(-0.05,0.02),
        main="mean Score on the Political Awareness Test for each Region", 
        ylab="Political Awareness Test Score")


#the mean Score on the Standardized Score Test for each Region

Test2_Score <- as.matrix(aggregate(PROG8430_Assign_Explore[,15], by=list(PROG8430_Assign_Explore$nation), FUN=mean))
head(Test2_Score)
rownames(Test2_Score) <- Test2_Score[,1] 
Test2_Score <- Test2_Score[,-1]
head(Test2_Score)

barplot(t(Test2_Score),
        legend=TRUE,
        ylim=c(0,1.5),
        main="mean Score on the Standardized Score Test for each Region", 
        ylab="Standardized Score Test Score")


##################################################
### Histograms                                  ##
##################################################

#Create a histogram with 5 bins showing the distribution of the percentage of household income going to food

hist(PROG8430_Assign_Explore$food, breaks = 5, main="The distribution of the percentage of household income going to food",xlab = "Pct of Income to Food")

#BoxPlot - Create a sequence of box plots showing the distribution of income separated by marital status

boxplot(PROG8430_Assign_Explore$income ~ PROG8430_Assign_Explore$m.status, 
        main="Distribution of income separated by marital status",
        xlab="Marital status", ylab = "Income",  pch = 20, horizontal = FALSE)



##################################################
### Scatter Plots                               ##
##################################################

#Histogram for income

hist(PROG8430_Assign_Explore$income, breaks = 'fd', main="Histogram for income",xlab = "Income")

#Histogram for standardized score

hist(PROG8430_Assign_Explore$scr, breaks = 'fd', main="Histogram for standardized score",xlab = "Standardized Score")

##################################################
### Scatter Plots                               ##
##################################################

# Relationship between the income and standardized score

plot(PROG8430_Assign_Explore$scr ~ PROG8430_Assign_Explore$income, main="Relationship between the income and standardized score",xlab = "Income", ylab = "Score")
abline(lm(PROG8430_Assign_Explore$scr ~ PROG8430_Assign_Explore$income, data = PROG8430_Assign_Explore), col = "blue")

# correlation coefficient between these two variables - Income & Scr

cor(PROG8430_Assign_Explore$scr,PROG8430_Assign_Explore$income,use = "complete.obs")

