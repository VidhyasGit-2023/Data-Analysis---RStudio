  ##################################################
  ### PROG8430                                    ##
  ### Logistic Demonstration                      ## 
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
  ### Install Libraries                           ##
  ##################################################
  
  #If the library is not already downloaded, download it
  
  if(!require(pROC)){install.packages("pROC")}
  library(pROC)
  
  ##################################################
  ### Read data and do preliminary data checks    ##
  ##################################################
  
  # Read "comma separated value" files (".csv")
  # Blood Donation data set
  Blood <- read.csv("BloodDonate_Class.csv", header = TRUE, sep = ",")
 
  head(Blood,5)  #Print a Few Observations to Verify
  
  #Rename for easier interpretation
  
  names(Blood) <- c("Gender", "Donate", "School", "Recent", "Nbr", "Vol", "Tim")
  
  str(Blood)
  
  summary(Blood)
  
  Blood$Gender <- as.factor(Blood$Gender)
  Blood$Gender <- as.numeric(Blood$Gender)  #Create numeric variable
  Blood$Gender <- Blood$Gender - 1   #Adjust for 0 or 1
  
  Blood$Donate <- as.factor(Blood$Donate)
  Blood$Donate <- as.numeric(Blood$Donate)  #Create numeric variable
  Blood$Donate <- Blood$Donate - 1   #Adjust for 0 or 1
  
  Cnc_Dummies <- model.matrix(~School -1 , data=Blood)
  head(Cnc_Dummies)
  Blood <- cbind(Blood, Cnc_Dummies)
  head(Blood)
  
  names(Blood) <- c("Gender", "Donate", "School", "Recent", "Nbr", "Vol", "Tim", "Sch01", "Sch02", "Sch03")
  
  head(Blood,5)  #Print a Few Observations to Verify
  
  #Adjust unusual data - Need to give reason when we make changes.
  
  Blood$Recent <- ifelse(Blood$Recent < 0, 0, Blood$Recent)
  
  ##################################################
  ### Descriptive Analysis                        ##
  ##################################################
  
  summary(Blood)
  
  #Pie Chart for School
  
  tmptable <- table(Blood$School)
  labels <- paste(names(tmptable), "\n", tmptable, sep="")
  pie(tmptable, labels = labels,
      main="Pie Chart of School\n (with sizes)")
  
  ##################################################
  ### Exploratory Analysis                        ##
  ##################################################
  
  BloodCorr <- Filter(is.numeric, Blood)  # Only take numeric
  str(BloodCorr)
  
  BldCorr <- cor(BloodCorr, method="spearman")
  round(BldCorr, 2) 
   
  Tbl_Rct <- table(Blood$Donate, Blood$Recent, dnn=list("Donate","Recent"))
  Tbl_Rct
  prop.table(Tbl_Rct, 2) # col percentages
  
  #Check the Chi Squared Test - NOTE Removal of Yate's Continuity Correction
  
  chisqRct <- chisq.test(Blood$Donate, Blood$Recent, correct=FALSE)      
  chisqRct
  
  chisqRct$observed   # What we observed
  chisqRct$expected   # If there were no relationship
  
  #Vertical Bar Chart
  
  barplot(prop.table(Tbl_Rct,2), xlab='Rcent',ylab='Pct',main="Donation by Recent",
          col=c("darkblue","darkred")
          ,legend=rownames(Tbl_Rct), args.legend = list(x = "topleft"))
  
  
  ##################################################
  ### Building the Model                          ##
  ##################################################
  
  #stepwise
  
  Bld_glm = glm(Donate ~ Gender + Recent + Nbr + Vol + Tim + Sch01 + Sch02
                                 ,
                family="binomial", data=Blood, na.action=na.omit)
  
  stp_Bld_glm <- step(Bld_glm)
  
  summary(stp_Bld_glm)
  
  #Manual (Dropping Vol)
  
  Bld_Man = glm(Donate ~ Gender + Recent + Nbr + Tim + Sch01 
                ,
                family="binomial", data=Blood, na.action=na.omit)
  
  summary(Bld_Man)
  
  
  ## Check the stepwise Model
  
  resp_SW <- predict(stp_Bld_glm, type="response")   # creates probabilities
  head(resp_SW,20)
  Class_SW <- ifelse(resp_SW > 0.5,1,0)           # Classifies probablities (i.e. >50% then likely to donate)
  head(Class_SW)
  True_log <- Blood$Donate                        #Creates a vector of the true outcomes
  T1 <- table(True_log, Class_SW, dnn=list("Act Donate","Predicted") )  # Creates a Contingency Table
                                                                        #Note - Could have just used Blood$Donate
  T1
  
  #ROC Curve (and Area Under the Curve)
  
  plot(roc(Blood$Donate,resp_SW, direction="<"),
       col="red", lwd=2, main='ROC Curve for Logistic, Blood Donation')
  
  auc(Blood$Donate, resp_SW)
  