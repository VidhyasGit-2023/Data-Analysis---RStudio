##################################################
### PROG8430                                    ##
### Demonstration of Reading in data            ## 
### And summarization                           ##
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
setwd("C:/Users/David/Documents/Data")

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

#if(!require(dplyr)){install.packages("dplyr")}
#library("dplyr")

##################################################
### Read in Data                                ##
##################################################

# Read "comma separated value" files (".csv")
# Record of Car Sales
Cars <- read.csv("PROG8430_Car_Sale.csv", header = TRUE, sep = ",")
names(Cars) <- c("Dlr","Model","Sold")
str(Cars)              # Gives structure of dataframe
ls(Cars)               # Lists variables in dataframe
head(Cars,10)          # Prints first 10 rows of dataframe
View(head(Cars,10))    # Lets you manipulate it 

# Read "tab delimited" files (".txt")
# Dealership Details 
Dlr <- read.delim("PROG8430_Car_Dlr.txt", header = TRUE, sep = "\t")
names(Dlr) <- c("Dlr","Emp","Year","Bldg","Mgrs")
str(Dlr)
colnames(Dlr)[5] <- "Mgr"        #Rename just one column
Dlr <- Dlr[c(1:3,5)]   # keep column 1-3 and 5
str(Dlr)
Dlr <- Dlr[-c(4)]      # drop column 4
str(Dlr)
head(Dlr,7)

# Read "comma separated value" files (".csv")
# Record of Customer Satisfaction
Cust <- read.csv("PROG8430_Car_Cust.csv", header = TRUE, sep = ",")
str(Cust)
colnames(Cust)[1] <- "Dlr"
str(Cust)

#Set -99 to NULL
Cust$Time[Cust$Time == -99] <- NA
str(Cust)

mean(Dlr$Emp)
median(Dlr$Emp)

