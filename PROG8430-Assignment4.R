##################################################
### PROG8430                                    ##
### Multivariate Linear Regression              ## 
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
library("pastecs")

if(!require(pastecs)){install.packages("pastecs")}
library("corrgram")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

##################################################
### Read in Data                                ##
##################################################


##############################
## Read in Data             ##
##############################

options(digits=5)

load("PROG8430_Assign_MLR_21F.Rdata")

head(PROG8430_Assign_MLR_21F)

# Consider only numeric data to reduce Dimensional

numeric_columns <- sapply(PROG8430_Assign_MLR_21F, is.numeric)

numeric_GroupData <- PROG8430_Assign_MLR_21F[, numeric_columns]

##############################
## Reduce Number of Rows    ##
##############################

# Find missing values
# Identify cols > 99% missing

summary(numeric_GroupData)

#Look like time2 is likely!

Group_Data <- numeric_GroupData[-c(11)]

head(Group_Data,12)

# Identify Low Variance

stat.desc(Group_Data)  #Consider coef of var
summary(Group_Data)

#Based on the above age seem likely. Let's check!

table(Group_Data$age)

Group_Data <- Group_Data[-c(2)]

head(Group_Data,11)

#Identify High Correlation

cor(Group_Data,method="spearman")

#time1 and time3 seem highly correlated
#Don't need to keep both, I'll drop the second one.

Group_Data <- Group_Data[-c(10)]

head(Group_Data,10)

###################################################
## Preliminary data transformation               ##
###################################################

#group, hs.grad, nation, gender, m.status, political - Convert to index (Dummy) Variables -1 is to delete the intercept column
grp_Dummies <- model.matrix(~PROG8430_Assign_MLR_21F$group+PROG8430_Assign_MLR_21F$hs.grad+PROG8430_Assign_MLR_21F$nation+PROG8430_Assign_MLR_21F$gender+PROG8430_Assign_MLR_21F$m.status+PROG8430_Assign_MLR_21F$political -1, data=Group_Data)
head(grp_Dummies)

Group_Data <- Group_Data[-c(1)] 
str(Group_Data)   #Check Results

#Combine the Datasets again
Group_Data_Tfm <- cbind(Group_Data, grp_Dummies)
head(Group_Data_Tfm)

head(Group_Data_Tfm)               #Check Results

#Adjust Names Again
names(Group_Data_Tfm) <- c("No Of Child", "Income", "Food", "Housing", "Other Income", 
                   "score","Pol","Time1","scr","Control-Group","Treat-Group","HS.Gradyes","Europe","North America","Southern","Male","Undis","Married","Never","Widowed","Liberal","New_Democrat","Political-Other")

head(Group_Data_Tfm) #Check Results

summary(Group_Data_Tfm)

###################################################
## Univariate Descriptive Analysis               ##
###################################################

Outlier_Group_Data <- Group_Data_Tfm[, 1:9]

par(mfrow=c(3,3))    #Fit more graphs in!

# Histogram for all variables
# loop over column *names* instead of actual columns
sapply(names(Outlier_Group_Data), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Outlier_Group_Data[[cname]]))
    # use the `main` param to put column name as plot title
    print(hist(Outlier_Group_Data[[cname]], main=cname))
})

par(mfrow=c(1,1))

###################################################
## Find Outliers                                 ##
###################################################

par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(Outlier_Group_Data), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Outlier_Group_Data[[cname]]))
    # use the `main` param to put column name as plot title
    print(boxplot(Outlier_Group_Data[[cname]], main=cname))
})

par(mfrow=c(1,1))

#########################################
## Test Data for Normality             ##
#########################################

### NOTE - Instead of doing these one at a time, do them all together.

GrpNrm <- lapply(Group_Data_Tfm, shapiro.test)
GrpNrm
str(GrpNrm[[4]])

GrpRes <- sapply(GrpNrm, `[`, c("statistic","p.value"))
GrpRes

GrpRest <- t(GrpRes)
GrpRest

#Graphical Tests

par(mfrow=c(3,3))

# BoxPlot for all variables
# loop over column *names* instead of actual columns
sapply(names(Group_Data), function(cname){
  # (make sure we only plot the numeric columns)
  if (is.numeric(Group_Data[[cname]]))
    # use the `main` param to put column name as plot title
    qqnorm(Group_Data[[cname]], main=cname)
  qqline(Group_Data[[cname]])
})

par(mfrow=c(1,1))


#########################################
## Checking Correlations               ##
#########################################

corrgram(Group_Data_Tfm, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Political Engagement Stats")

res <- cor(Group_Data_Tfm, method="spearman")
round(res, 2)

cor(Group_Data_Tfm$scr, Group_Data_Tfm$Income, method="spearman")



##################################################
### Create a Model                              ##
##################################################

#Create a Simple Linear Model for each dataset and print specifications
#Political Involvement against Score on Political awareness test

Sysmodel <- lm(Group_Data_Tfm$Pol ~ Group_Data_Tfm$score, data=Group_Data_Tfm)

Sysmodel

xyplot(Group_Data_Tfm$Pol ~ Group_Data_Tfm$score, data=Group_Data_Tfm, panel = function(x,y) {
  panel.xyplot(x, y)
  panel.abline(Sysmodel)
}, main=list(label="Political Involvement to Score on Political Awareness Test (with Regression Line)",
             cex=0.85))

#Now the same for Political Involvement against Standardized Score Test

Tdrmodel <- lm(Group_Data_Tfm$Pol ~ Group_Data_Tfm$scr, data=Group_Data_Tfm)

Tdrmodel

xyplot(Group_Data_Tfm$Pol ~ Group_Data_Tfm$scr, data=Group_Data_Tfm, panel = function(x,y) {
  panel.xyplot(x, y)
  panel.abline(Tdrmodel)
}, main=list(label="Political Involvement against Standardized Score Test (with Regression Line)",
             cex=0.85))

#Provide some total measures of fitness

summary(Sysmodel)

summary(Tdrmodel)

#Plot diagnostics for each model

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid

plot(Sysmodel)  # Plot the model information
plot(Tdrmodel)  # Plot the model information

par(mfrow = c(1, 1))  # Return plotting panel to 1 section


#########################################
## Creating Baseline Model             ##
#########################################

Pol_lm = lm(Group_Data_Tfm$Pol ~ Group_Data_Tfm$`No Of Child` + Group_Data_Tfm$Income + Group_Data_Tfm$Food + Group_Data_Tfm$Housing + Group_Data_Tfm$`Other Income` + Group_Data_Tfm$score
            + Group_Data_Tfm$Time1 + Group_Data_Tfm$scr + Group_Data_Tfm$`Control-Group` + Group_Data_Tfm$`Treat-Group` + Group_Data_Tfm$HS.Gradyes + Group_Data_Tfm$Europe + Group_Data_Tfm$`North America`
            + Group_Data_Tfm$Southern + Group_Data_Tfm$Male + Group_Data_Tfm$Undis + Group_Data_Tfm$Married + Group_Data_Tfm$Never + Group_Data_Tfm$Widowed + Group_Data_Tfm$Liberal + Group_Data_Tfm$New_Democrat
            + Group_Data_Tfm$`Political-Other`,
            data=Group_Data_Tfm, na.action=na.omit)
Pol_lm
summary(Pol_lm)

#########################################
## Creating Backward Selection Model   ##
#########################################

Bck_Pol_lm = step(Pol_lm, direction="backward", details=TRUE)

Bck_Pol_lm
summary(Bck_Pol_lm)

#########################################
## Evaluating the Models               ##
#########################################

###########################################
## Creating Model and Residual vectors    #
###########################################

PolFit <- predict(Pol_lm)
PolRes <- residuals(Pol_lm)

BckPolFit <- predict(Bck_Pol_lm)
BckPolRes <- residuals(Bck_Pol_lm)


#Numerically

shapiro.test(PolRes)
shapiro.test(BckPolRes)

#Graphically

par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Pol_lm)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section


par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(Bck_Pol_lm)  # Plot the model information
par(mfrow = c(1, 1))  # Return plotting panel to 1 section

