#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/R6 Project")
#turn on library
library(base)
#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
################## Clean data #########################

#remove participants with no data
Physical <- Physical[complete.cases(Physical$BMI), ]
PCIAT <- PCIAT[-c(958, 1132, 1491), ]
C3SR <- C3SR[-c(653, 1163), ]
FTND <- FTND[-c(58), ]
MFQ_P <- MFQ_P[-c(918, 1296), ]
MFQ_SR <- MFQ_SR[-c(918), ]
SCARED_SR <- SCARED_SR[-c(727), ]
YFAS_C <- YFAS_C[-c(425,450, 1346), ]

#turn NAs into zeros only for IAT and PCIAT because of scoring errors.
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0

#rescore PCIAT and IAT
IAT$IAT_Total <- rowSums(IAT[2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[2:21])

#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
Categorical_IAT <- data.frame(ID = IAT$URSI, Problematic = ifelse (IAT$IAT_Total >=40, 1, 0))
Categorical_PCIAT <- data.frame(ID = PCIAT$URSI, Problematic = ifelse (PCIAT$PCIAT_Total >=40, 1, 0))

#pull out covariates of interest
Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total)

#dataframe with dxs and IAT/PCIAT
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
#IAT_Dx <- Reduce(Merge, list(Demos_of_interest, Barratt_of_interest, Categorical_IAT, Dx_of_interest))
#PCIAT_Dx <- Reduce(Merge, list(Demos_of_interest, Barratt_of_interest, Categorical_PCIAT, Dx_of_interest))

Threshold_df <- Reduce(Merge, list(Categorical_IAT, Categorical_PCIAT,
                                   Dx_of_interest, Demos_of_interest, Barratt_of_interest))

#calculate individual logistic regressions for each Dx for SR adjusted only for covariates -> exponentiate for odds ratios
#ASD
Log_Reg_ASD <- glm(Problematic ~ ASD + 
                     Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ASD)
exp(Log_Reg_ASD$coefficients)
exp(confint(Log_Reg_ASD))
#Learning Disorder
Log_Reg_Learning_Disorder <- glm(Problematic ~ Learning_Disorder + 
                     Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Learning_Disorder)
exp(Log_Reg_Learning_Disorder$coefficients)
exp(confint(Log_Reg_Learning_Disorder))
#Anxiety
Log_Reg_Anxiety <- glm(Problematic ~ Anxiety + 
                                   Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Anxiety)
exp(Log_Reg_Anxiety$coefficients)
exp(confint(Log_Reg_Anxiety))
#Depression
Log_Reg_Depression <- glm(Problematic ~ Depression +  
                         Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Depression)
exp(Log_Reg_Depression$coefficients)
exp(confint(Log_Reg_Depression))
#ADHD-C
Log_Reg_ADHD_Combined <- glm(Problematic ~ ADHD_Combined + 
                            Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ADHD_Combined)
exp(Log_Reg_ADHD_Combined$coefficients)
exp(confint(Log_Reg_ADHD_Combined))
#ADHD-I
Log_Reg_ADHD_Inattentive <- glm(Problematic ~ ADHD_Inattentive +  
                               Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ADHD_Inattentive)
exp(Log_Reg_ADHD_Inattentive$coefficients)
exp(confint(Log_Reg_ADHD_Inattentive))
#ADHD-H
Log_Reg_ADHD_Hyperactive <- glm(Problematic ~ ADHD_Hyperactive + 
                                  Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ADHD_Hyperactive)
exp(Log_Reg_ADHD_Hyperactive$coefficients)
exp(confint(Log_Reg_ADHD_Hyperactive))
#Social Anxiety
Log_Reg_Social_Anxiety <- glm(Problematic ~ Social_Anxiety +
                                  Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Social_Anxiety)
exp(Log_Reg_Social_Anxiety$coefficients)
exp(confint(Log_Reg_Social_Anxiety))

#calculate individual logistic regressions for each Dx for Parent Report adjusted for only covariates
#ASD
Log_Reg_ASD <- glm(Problematic ~ ASD + 
                     Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ASD)
exp(Log_Reg_ASD$coefficients)
exp(confint(Log_Reg_ASD))
#Learning Disorder
Log_Reg_Learning_Disorder <- glm(Problematic ~ Learning_Disorder + 
                                   Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Learning_Disorder)
exp(Log_Reg_Learning_Disorder$coefficients)
exp(confint(Log_Reg_Learning_Disorder))
#Anxiety
Log_Reg_Anxiety <- glm(Problematic ~ Anxiety + 
                         Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Anxiety)
exp(Log_Reg_Anxiety$coefficients)
exp(confint(Log_Reg_Anxiety))
#Depression
Log_Reg_Depression <- glm(Problematic ~ Depression +
                            Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Depression)
exp(Log_Reg_Depression$coefficients)
exp(confint(Log_Reg_Depression))
#ADHD-C
Log_Reg_ADHD_Combined <- glm(Problematic ~ ADHD_Combined + 
                               Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ADHD_Combined)
exp(Log_Reg_ADHD_Combined$coefficients)
exp(confint(Log_Reg_ADHD_Combined))
#ADHD-I
Log_Reg_ADHD_Inattentive <- glm(Problematic ~ ADHD_Inattentive + 
                                  Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ADHD_Inattentive)
exp(Log_Reg_ADHD_Inattentive$coefficients)
exp(confint(Log_Reg_ADHD_Inattentive))
#ADHD-H
Log_Reg_ADHD_Hyperactive <- glm(Problematic ~ ADHD_Hyperactive + 
                                  Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ADHD_Hyperactive)
exp(Log_Reg_ADHD_Hyperactive$coefficients)
exp(confint(Log_Reg_ADHD_Hyperactive))
#Social Anxiety
Log_Reg_Social_Anxiety <- glm(Problematic ~ Social_Anxiety + 
                                Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Social_Anxiety)
exp(Log_Reg_Social_Anxiety$coefficients)
exp(confint(Log_Reg_Social_Anxiety))

#calculate individual logistic regressions for each Dx for SR adjusted for covariates and significant dxs -> exponentiate both estimates and confidence intervals to get to odds ratios
#ASD
Log_Reg_ASD <- glm(Problematic ~ ASD + Depression + ADHD_Combined + 
                     Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ASD)
exp(Log_Reg_ASD$coefficients)
exp(confint(Log_Reg_ASD))
#Learning Disorder
Log_Reg_Learning_Disorder <- glm(Problematic ~ Learning_Disorder + Depression + ADHD_Combined + 
                     Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Learning_Disorder)
exp(Log_Reg_Learning_Disorder$coefficients)
exp(confint(Log_Reg_Learning_Disorder))
#Anxiety
Log_Reg_Anxiety <- glm(Problematic ~ Anxiety + Depression + ADHD_Combined + 
                                   Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Anxiety)
exp(Log_Reg_Anxiety$coefficients)
exp(confint(Log_Reg_Anxiety))
#Depression
Log_Reg_Depression <- glm(Problematic ~ Depression + ADHD_Combined + 
                         Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Depression)
exp(Log_Reg_Depression$coefficients)
exp(confint(Log_Reg_Depression))
#ADHD-C
Log_Reg_ADHD_Combined <- glm(Problematic ~ Depression + ADHD_Combined + 
                            Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ADHD_Combined)
exp(Log_Reg_ADHD_Combined$coefficients)
exp(confint(Log_Reg_ADHD_Combined))
#ADHD-I
Log_Reg_ADHD_Inattentive <- glm(Problematic ~ ADHD_Inattentive + Depression + ADHD_Combined + 
                               Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ADHD_Inattentive)
exp(Log_Reg_ADHD_Inattentive$coefficients)
exp(confint(Log_Reg_ADHD_Inattentive))
#ADHD-H
Log_Reg_ADHD_Hyperactive <- glm(Problematic ~ ADHD_Hyperactive + Depression + ADHD_Combined + 
                                  Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_ADHD_Hyperactive)
exp(Log_Reg_ADHD_Hyperactive$coefficients)
exp(confint(Log_Reg_ADHD_Hyperactive))
#Social Anxiety
Log_Reg_Social_Anxiety <- glm(Problematic ~ Social_Anxiety + Depression + ADHD_Combined + 
                                  Sex + Age + SES + Site, family = binomial, data = IAT_Dx)
summary(Log_Reg_Social_Anxiety)
exp(Log_Reg_Social_Anxiety$coefficients)
exp(confint(Log_Reg_Social_Anxiety))

#calculate individual logistic regressions for each Dx for Parent Report adjusted for both covariates and significant dxs
#ASD
Log_Reg_ASD <- glm(Problematic ~ ASD + Anxiety + Depression + ADHD_Inattentive + 
                     Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ASD)
exp(Log_Reg_ASD$coefficients)
exp(confint(Log_Reg_ASD))
#Learning Disorder
Log_Reg_Learning_Disorder <- glm(Problematic ~ Learning_Disorder + ASD + Anxiety + Depression + ADHD_Inattentive + 
                                   Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Learning_Disorder)
exp(Log_Reg_Learning_Disorder$coefficients)
exp(confint(Log_Reg_Learning_Disorder))
#Anxiety
Log_Reg_Anxiety <- glm(Problematic ~ Anxiety + ASD + Depression + ADHD_Inattentive + 
                         Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Anxiety)
exp(Log_Reg_Anxiety$coefficients)
exp(confint(Log_Reg_Anxiety))
#Depression
Log_Reg_Depression <- glm(Problematic ~ ASD + Anxiety + Depression + ADHD_Inattentive + 
                            Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Depression)
exp(Log_Reg_Depression$coefficients)
exp(confint(Log_Reg_Depression))
#ADHD-C
Log_Reg_ADHD_Combined <- glm(Problematic ~ ADHD_Combined + ASD + Anxiety + Depression + ADHD_Inattentive + 
                               Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ADHD_Combined)
exp(Log_Reg_ADHD_Combined$coefficients)
exp(confint(Log_Reg_ADHD_Combined))
#ADHD-I
Log_Reg_ADHD_Inattentive <- glm(Problematic ~ ADHD_Inattentive + ASD + Anxiety + Depression + 
                                  Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ADHD_Inattentive)
exp(Log_Reg_ADHD_Inattentive$coefficients)
exp(confint(Log_Reg_ADHD_Inattentive))
#ADHD-H
Log_Reg_ADHD_Hyperactive <- glm(Problematic ~ ADHD_Hyperactive + ASD + Anxiety + Depression + 
                                  Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_ADHD_Hyperactive)
exp(Log_Reg_ADHD_Hyperactive$coefficients)
exp(confint(Log_Reg_ADHD_Hyperactive))
#Social Anxiety
Log_Reg_Social_Anxiety <- glm(Problematic ~ Social_Anxiety + ASD + Anxiety + Depression + 
                                Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx)
summary(Log_Reg_Social_Anxiety)
exp(Log_Reg_Social_Anxiety$coefficients)
exp(confint(Log_Reg_Social_Anxiety))

# SR: logistic regression including covariates + significant predictors
summary(glm(Problematic ~ Social_Anxiety + Learning_Disorder + Depression + ADHD_Combined
            + Sex + Age + SES + Site, family = binomial, data = IAT_Dx))

# P: logistic regression including covariates + significant predictors
summary(glm(Problematic ~ Social_Anxiety + ASD + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive 
            + Sex + Age + SES + Site, family = binomial, data = PCIAT_Dx))
