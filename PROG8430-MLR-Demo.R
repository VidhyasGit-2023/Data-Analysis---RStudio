##################################################
### PROG8430                                    ##
### Multivariate Linear Regression              ## 
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

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read text file (".txt")
# Denver Crime data sheet
Denver <- read.delim("DenverCrime.txt", header=TRUE)

Denver[5:15,]     #Prints data 5 to 15 to make sure it looks correct

ls(Denver)      #Lists all objects for Denver Crime Data
names(Denver)   #List the variables for Denver Crime Data
str(Denver)     #List structure of Denver Crime Data

#Rename for easier interpretation

names(Denver) <- c("Pop", "PopChg", "Child", "Lunch", "IncChg", 
                    "Income","Crime","CrmChg","Year","Counsellor")

#Retain Only Pop > 2 because of political reasons

Denver <- subset(Denver, Pop>2)

str(Denver)      #Check changes

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
str(Denver)

#Counsellor - Convert to index (Dummy) Variables -1 is to delete the intercept column
Cnc_Dummies <- model.matrix(~Counsellor -1, data=Denver)
head(Cnc_Dummies)

#Combine the Datasets again
Denver <- cbind(Denver, Cnc_Dummies)
head(Denver)

str(Denver)   #Check Results

#Denver[11:14]

#Denver <- Denver[-c(10)]   #Drop unneeded column
head(Denver)               #Check Results

#Adjust Names Again
names(Denver) <- c("Pop", "PopChg", "Child", "Lunch", "IncChg", 
                    "Income","Crime","CrmChg","Year","Counsel","John",
                    "Jone","Lang","Will")
head(Denver)               #Check Results

###################################################
## Univariate Descriptive Analysis               ##
###################################################

summary(Denver)

Denver <- Denver[-c(10)]  #Drop un-needed column

par(mfrow=c(3,3))    #Fit more graphs in!

# Histogram for all variables
# loop over column *names* instead of actual columns
sapply(names(Denver), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Denver[[cname]]))
    # use the `main` param to put column name as plot title
    print(hist(Denver[[cname]], main=cname))
})

par(mfrow=c(1,1))

###################################################
## Find Outliers                                 ##
###################################################

par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(Denver), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Denver[[cname]]))
    # use the `main` param to put column name as plot title
    print(boxplot(Denver[[cname]], main=cname))
})

par(mfrow=c(1,1))

#########################################
## Test Data for Normality             ##
#########################################

### NOTE - Instead of doing these one at a time, do them all together.

DenNrm <- lapply(Denver, shapiro.test)
DenNrm
str(DenNrm[[4]])

DenRes <- sapply(DenNrm, `[`, c("statistic","p.value"))
DenRes

DenRest <- t(DenRes)
DenRest

#Graphical Tests

par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(Denver), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Denver[[cname]]))
    # use the `main` param to put column name as plot title
    qqnorm(Denver[[cname]], main=cname)
    qqline(Denver[[cname]])
})

par(mfrow=c(1,1))


#########################################
## Checking Correlations               ##
#########################################

corrgram(Denver, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Denver Crime Stats")

res <- cor(Denver, method="spearman")
round(res, 2)

cor(Denver$IncChg, Denver$PopChg, method="spearman")

#########################################
## Creating Baseline Model             ##
#########################################

Den_lm = lm(Crime ~ Pop + PopChg + Child + Lunch + IncChg + CrmChg +
            Year + John + Jone + Lang ,
             data=Denver, na.action=na.omit)
Den_lm
summary(Den_lm)

#########################################
## Creating Backward Selection Model   ##
#########################################

Bck_Den_lm = step(Den_lm, direction="backward", details=TRUE)

Bck_Den_lm
summary(Bck_Den_lm)

#########################################
## Creating Forward  Selection Model   ##
#########################################

min_model <- lm(Crime ~ 1, data=Denver, na.action=na.omit)
Fwd_Den_lm = step(min_model, direction="forward", scope =(
             ~ Pop + PopChg +  Lunch + IncChg +Income + CrmChg +
            Year + John + Jone + Lang), details=TRUE)

Fwd_Den_lm
summary(Fwd_Den_lm)


#########################################
## Creating Criteria Selection Model   ##
#########################################

Den_lm = lm(Crime ~ Pop + PopChg + Child + Lunch + IncChg + CrmChg +
            Year + John + Jone + Lang ,
             data=Denver, na.action=na.omit)
stp_den_lm <- step(Den_lm)
stp_den_lm
summary(stp_den_lm)


#########################################
## Evaluating the Models               ##
#########################################

###########################################
## Creating Model and Residual vectors    #
###########################################

DenFit <- predict(Den_lm)
DenRes <- residuals(Den_lm)

BckDenFit <- predict(Bck_Den_lm)
BckDenRes <- residuals(Bck_Den_lm)

FwdDenFit <- predict(Fwd_Den_lm)
FwdDenRes <- residuals(Fwd_Den_lm)

StpDenFit <- predict(stp_den_lm)
StpDenRes <- residuals(stp_den_lm)


#Numerically

shapiro.test(DenRes)
shapiro.test(BckDenRes)
shapiro.test(FwdDenRes)
shapiro.test(StpDenRes)

#Graphically

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Den_lm)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Bck_Den_lm)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Fwd_Den_lm)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(stp_den_lm)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


#Denver <- cbind(Denver, DenRes)







