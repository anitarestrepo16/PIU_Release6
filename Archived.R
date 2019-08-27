#### for table 1 (All SR)
########old categories using absence as reference
#create age categories with dummy variables
Standard_Sample_SR$Ages5to8 = ifelse(Standard_Sample_SR$Age <9.00, 1, 0)
Standard_Sample_SR$Ages9to12 = ifelse(Standard_Sample_SR$Age >=9.00 & Standard_Sample_SR$Age<13.00, 1, 0)
Standard_Sample_SR$Ages13to16 = ifelse(Standard_Sample_SR$Age >=13.00 & Standard_Sample_SR$Age < 17.00, 1, 0)
Standard_Sample_SR$Ages17to21 = ifelse(Standard_Sample_SR$Age >= 17.00, 1, 0)
#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
Standard_Sample_SR$Problematic_SR = ifelse (Standard_Sample_SR$IAT_SR >=40, 1, 0)
#create SES breakdowns using dummy variables
Standard_Sample_SR$Low_SES = ifelse (Standard_Sample_SR$SES < 28, 1, 0)
Standard_Sample_SR$Middle_SES = ifelse(Standard_Sample_SR$SES >= 28 & Standard_Sample_SR$SES < 47, 1, 0)
Standard_Sample_SR$High_SES = ifelse(Standard_Sample_SR$SES >= 47, 1, 0)
#create Race breakdowns using dummy variables
Standard_Sample_SR$Caucasian = ifelse (Standard_Sample_SR$Race == 0, 1, 0)
Standard_Sample_SR$Black = ifelse (Standard_Sample_SR$Race == 1, 1, 0)
Standard_Sample_SR$Hispanic = ifelse(Standard_Sample_SR$Race == 2, 1, 0)
Standard_Sample_SR$Asian = ifelse(Standard_Sample_SR$Race == 3, 1, 0)
Standard_Sample_SR$Other_Race = ifelse(Standard_Sample_SR$Race != 0 & Standard_Sample_SR$Race != 1 & 
                                         Standard_Sample_SR$Race != 2 & Standard_Sample_SR$Race != 0, 1, 0)
#Dummy variable for site
Standard_Sample_SR$SI = ifelse(Standard_Sample_SR$Site == 1, 1, 0)
Standard_Sample_SR$MRV = ifelse(Standard_Sample_SR$Site == 2, 1, 0)
Standard_Sample_SR$Midtown = ifelse (Standard_Sample_SR$Site == 3, 1, 0)

library(epiR)
#run odds ratios
#sex
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Sex)
data_table
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ages 5-8
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Ages5to8)
x <- epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Ages5to8 <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#ages 9-12
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Ages9to12)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Ages9to12 <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#ages 13-16
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Ages13to16)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Ages13to16 <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#ages 17-21 - none in this category as of 5.7.19
#data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Ages17to21)
#data_table
#x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#df_Ages17to21 <- data.frame(
#  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2))
#Caucasian
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Caucasian)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Caucasian <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#Black
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Black)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Black <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#Hispanic
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Hispanic)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Hispanic <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#Asian
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Asian)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Asian <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#other race
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Other_Race)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Other_Race <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#Low SES
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Low_SES)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Low_SES <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#Middle SES
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Middle_SES)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Middle_SES <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#High SES
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$High_SES)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_High_SES <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#SI
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$SI)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_SI <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#MRV - finally no one in this category with standard sample (5.7.19)
#data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$MRV)
#data_table
#library(epiR)
#x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#df_MRV <- data.frame(
#  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2))
#Midtown
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Midtown)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Midtown <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#ASD
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$ASD)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_ASD <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#Learning Disorder - not using as of 5.7.19
#data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Learning_Disorder)
#data_table
#library(epiR)
#x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#df_Learning_Disorder <- data.frame(
#  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2))
#Anxiety
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Anxiety)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Anxiety <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#Depression
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Depression)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Depression <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#ADHD-C
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$ADHD_Combined)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_ADHD_Combined <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#ADHD-I
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$ADHD_Inattentive)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_ADHD_Inattentive <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))
#ADHD-H - not using as of 5.7.19
#data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$ADHD_Hyperactive)
#data_table
#library(epiR)
#x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#df_ADHD_Hyperactive <- data.frame(
#  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2))
#Social Anxiety
data_table <- table(Standard_Sample_SR$Problematic_SR, Standard_Sample_SR$Social_Anxiety)
x <-epi.2by2(data_table, method="cohort.count", conf.level=0.95)
df_Social_Anxiety <- data.frame(
  No_PIU = data_table[1, 2], Yes_PIU = data_table[2, 2], round(x$res$OR.strata.score, digits = 2), 
  round(x$res$chisq.strata[ , 3], digits = 2))

#bind dataframes into one for export
All_Demos_ORs <- rbind(df_Ages5to8, df_Ages9to12, df_Ages13to16, 
                       df_Caucasian, df_Black, df_Hispanic, df_Asian, df_Other_Race, 
                       df_Low_SES, df_Middle_SES, df_High_SES, df_SI, df_Midtown,
                       df_ASD, df_Anxiety, df_Depression, 
                       df_ADHD_Combined, df_ADHD_Inattentive, 
                       df_Social_Anxiety)
row.names(All_Demos_ORs) <- c("Ages5to8", "Ages9to12", "Ages13to16", 
                              "Caucasian", "Black", "Hispanic", "Asian", "Other_Race", 
                              "Low_SES", "Middle_SES", "High_SES", "SI", "Midtown",
                              "ASD", "Anxiety", "Depression", 
                              "ADHD_Combined", "ADHD_Inattentive", 
                              "Social_Anxiety")
colnames(All_Demos_ORs) <- c("No_PIU", "Yes_PIU", "OR", "2.5% CI", "97.5% CI", "p")
#get percentages
All_Demos_ORs$Percent_No = round(All_Demos_ORs$No_PIU/568*100, digits = 2)
All_Demos_ORs$Percent_Yes = round(All_Demos_ORs$Yes_PIU/568*100, digits = 2)
#export as csv file
write.csv(All_Demos_ORs, file = "All_Demos_ORs.csv")



############## SR
#CIS - unadjusted
Reg_CIS_unadjusted_SR <- glm(PIU ~ CIS_SR_Score, family = binomial, data = Standard_Sample_SR)
#CIS - adjusted for covariates
Reg_CIS_adj_cov_SR <- glm(PIU ~ CIS_SR_Score + Age + Sex + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
#CIS - adjusted for covariates + dxs
Reg_CIS_adjusted_SR <- glm(PIU ~ CIS_SR_Score + ASD + Anxiety + Depression +
                             ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                             Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)

#PAQ - unadjusted
Reg_PAQ_unadjusted_SR <- glm(PIU ~ PAQ_Total, family = binomial, data = Standard_Sample_SR)
#PAQ - adjusted for covariates
Reg_PAQ_adj_cov_SR <- glm(PIU ~ PAQ_Total + Sex + Age + SES + Site + Single_Caregiver, family = binomial, 
                          data = Standard_Sample_SR)
#PAQ - adjusted for covariates + dxs
Reg_PAQ_adjusted_SR <- glm(PIU ~ PAQ_Total + ASD + Anxiety + Depression +
                             ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                             Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)

#BMI - unadjusted
Reg_BMI_unadjusted_SR <- glm(PIU ~ BMI, family = binomial, data = Standard_Sample_SR)
#BMI - adjusted for covariates
Reg_BMI_adj_cov_SR <- glm(PIU ~ BMI + Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
#BMI - adjusted for covariates + dxs
Reg_BMI_adjusted_SR <- glm(PIU ~ BMI + ASD + Anxiety + Depression +
                             ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                             Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)

#FMI - unadjusted
Reg_FMI_unadjusted_SR <- glm(PIU ~ FMI, family = binomial, data = Standard_Sample_SR)
#FMI - adjusted for covariates
Reg_FMI_adj_cov_SR <- glm(PIU ~ FMI + Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
#FMI - adjusted for covariates + dxs
Reg_FMI_adjusted_SR <- glm(PIU ~ FMI + ASD + Anxiety + Depression +
                             ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                             Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)

#SDS - unadjusted
Reg_SDS_unadjusted_SR <- glm(PIU ~ SDS_Total, family = binomial, data = Standard_Sample_SR)
#SDS - adjusted fr covariates
Reg_SDS_adj_cov_SR <- glm(PIU ~ SDS_Total + Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)
#SDS - adjusted for covariates + dxs
Reg_SDS_adjusted_SR <- glm(PIU ~ SDS_Total + ASD + Anxiety + Depression +
                             ADHD_Combined + ADHD_Inattentive +  Social_Anxiety +
                             Sex + Age + SES + Site + Single_Caregiver, family = binomial, data = Standard_Sample_SR)

#make DFs for unadjusted with data of interest to combine
DF_CIS <- data.frame(
  round(Reg_CIS_unadjusted_SR$coefficients[2], digits = 2), round(confint(Reg_CIS_unadjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_CIS_unadjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_CIS_unadjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_CIS) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_PAQ <- data.frame(
  round(Reg_PAQ_unadjusted_SR$coefficients[2], digits = 2), round(confint(Reg_PAQ_unadjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_PAQ_unadjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_PAQ_unadjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_PAQ) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_BMI <- data.frame(
  round(Reg_BMI_unadjusted_SR$coefficients[2], digits = 2), round(confint(Reg_BMI_unadjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_BMI_unadjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_BMI_unadjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_BMI) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_FMI <- data.frame(
  round(Reg_FMI_unadjusted_SR$coefficients[2], digits = 2), round(confint(Reg_FMI_unadjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_FMI_unadjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_FMI_unadjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_FMI) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SDS <- data.frame(
  round(Reg_SDS_unadjusted_SR$coefficients[2], digits = 2), round(confint(Reg_SDS_unadjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_SDS_unadjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_SDS_unadjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_SDS) <- c("B", "2.5% CI", "97.5% CI", "p value")
#combine into unadjusted df for export
Neg_Outcomes_unadjusted_SR <- rbind(DF_CIS, DF_PAQ, DF_BMI, DF_FMI, DF_SDS)
#export as csv file
write.csv(Neg_Outcomes_unadjusted_SR, file = "Neg_Outcomes_unadjusted_SR.csv")

#make DFs for djusted for covariates with data of interest to combine
DF_CIS <- data.frame(
  round(Reg_CIS_adj_cov_SR$coefficients[2], digits = 2), round(confint(Reg_CIS_adj_cov_SR)[2, 1], digits = 2),
  round(confint(Reg_CIS_adj_cov_SR)[2, 2], digits = 2), 
  round(summary(Reg_CIS_adj_cov_SR)$coefficients[2, 4], digits = 2)) 
names(DF_CIS) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_PAQ <- data.frame(
  round(Reg_PAQ_adj_cov_SR$coefficients[2], digits = 2), round(confint(Reg_PAQ_adj_cov_SR)[2, 1], digits = 2),
  round(confint(Reg_PAQ_adj_cov_SR)[2, 2], digits = 2), 
  round(summary(Reg_PAQ_adj_cov_SR)$coefficients[2, 4], digits = 2)) 
names(DF_PAQ) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_BMI <- data.frame(
  round(Reg_BMI_adj_cov_SR$coefficients[2], digits = 2), round(confint(Reg_BMI_adj_cov_SR)[2, 1], digits = 2),
  round(confint(Reg_BMI_adj_cov_SR)[2, 2], digits = 2), 
  round(summary(Reg_BMI_adj_cov_SR)$coefficients[2, 4], digits = 2)) 
names(DF_BMI) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_FMI <- data.frame(
  round(Reg_FMI_adj_cov_SR$coefficients[2], digits = 2), round(confint(Reg_FMI_adj_cov_SR)[2, 1], digits = 2),
  round(confint(Reg_FMI_adj_cov_SR)[2, 2], digits = 2), 
  round(summary(Reg_FMI_adj_cov_SR)$coefficients[2, 4], digits = 2)) 
names(DF_FMI) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SDS <- data.frame(
  round(Reg_SDS_adj_cov_SR$coefficients[2], digits = 2), round(confint(Reg_SDS_adj_cov_SR)[2, 1], digits = 2),
  round(confint(Reg_SDS_adj_cov_SR)[2, 2], digits = 2), 
  round(summary(Reg_SDS_adj_cov_SR)$coefficients[2, 4], digits = 2)) 
names(DF_SDS) <- c("B", "2.5% CI", "97.5% CI", "p value")
#combine into adj_cov df for export
Neg_Outcomes_adj_cov_SR <- rbind(DF_CIS, DF_PAQ, DF_BMI, DF_FMI, DF_SDS)
#export as csv file
write.csv(Neg_Outcomes_adj_cov_SR, file = "Neg_Outcomes_adj_cov_SR.csv")

#make DFs for adjusted for covariates + dxs with data of interest to combine
DF_CIS <- data.frame(
  round(Reg_CIS_adjusted_SR$coefficients[2], digits = 2), round(confint(Reg_CIS_adjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_CIS_adjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_CIS_adjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_CIS) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_PAQ <- data.frame(
  round(Reg_PAQ_adjusted_SR$coefficients[2], digits = 2), round(confint(Reg_PAQ_adjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_PAQ_adjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_PAQ_adjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_PAQ) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_BMI <- data.frame(
  round(Reg_BMI_adjusted_SR$coefficients[2], digits = 2), round(confint(Reg_BMI_adjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_BMI_adjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_BMI_adjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_BMI) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_FMI <- data.frame(
  round(Reg_FMI_adjusted_SR$coefficients[2], digits = 2), round(confint(Reg_FMI_adjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_FMI_adjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_FMI_adjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_FMI) <- c("B", "2.5% CI", "97.5% CI", "p value")
DF_SDS <- data.frame(
  round(Reg_SDS_adjusted_SR$coefficients[2], digits = 2), round(confint(Reg_SDS_adjusted_SR)[2, 1], digits = 2),
  round(confint(Reg_SDS_adjusted_SR)[2, 2], digits = 2), 
  round(summary(Reg_SDS_adjusted_SR)$coefficients[2, 4], digits = 2)) 
names(DF_SDS) <- c("B", "2.5% CI", "97.5% CI", "p value")
#combine into adjusted df for export
Neg_Outcomes_adjusted_SR <- rbind(DF_CIS, DF_PAQ, DF_BMI, DF_FMI, DF_SDS)
#export as csv file
write.csv(Neg_Outcomes_adjusted_SR, file = "Neg_Outcomes_adjusted_SR.csv")

#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/R6 Project")
#turn on library
library(base)
#################### Import all data ##############
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/R6 Project/Data")
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

#create dummy variable for 1 caregiver only based on Barratt responses
Barratt$No_C2 <- ifelse(is.na(Barratt$Barratt_P2_Edu) & 
                          is.na(Barratt$Barratt_P2_Occ), 1, 0)


########### make df with intersection of all samples -> final sample (NOT including dimensional measures of psychoapthology) #########
#pull out data of interest (Dx_of_interest already created above)
IAT_of_interest <- data.frame(
  ID = IAT$URSI,
  IAT_SR = IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$URSI,
  IAT_Parent = PCIAT$PCIAT_Total)

Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

Race <- data.frame(
  ID = PreInt_Demos_Fam$URSI,
  Race = PreInt_Demos_Fam$Child_Race)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total,
  Single_Caregiver = Barratt$No_C2)

Physical_of_interest <- data.frame(
  ID = Physical$URSI,
  BMI = Physical$BMI)

BIA_of_interest <- data.frame(
  ID = BIA$URSI,
  FMI = BIA$FMI)

PAQ_A_of_interest <- data.frame(
  ID = PAQ_A$URSI,
  PAQ_Total = PAQ_A$PAQ_A_Total)

PAQ_C_of_interest <- data.frame(
  ID = PAQ_C$URSI,
  PAQ_Total = PAQ_C$PAQ_C_Total)

SDS_of_interest <- data.frame(
  ID = SDS$URSI,
  SDS_Total = SDS$SDS_Total_T)

CIS_P_of_interest <- data.frame(
  ID = CIS_P$URSI,
  CIS_P_Score = CIS_P$CIS_P_Score)

CIS_SR_of_interest <- data.frame(
  ID = CIS_SR$URSI,
  CIS_SR_Score = CIS_SR$CIS_SR_Total)

#merge PAQ dataframes
PAQ_of_interest <- rbind(PAQ_A_of_interest, PAQ_C_of_interest)

#construct final sample df for both Parent and SR
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Standard_Sample_SR <- Reduce(Merge, list(IAT_of_interest,
                                         Demos_of_interest, Race, Barratt_of_interest, 
                                         Physical_of_interest, 
                                         BIA_of_interest, PAQ_of_interest, SDS_of_interest, 
                                         CIS_SR_of_interest, Dx_of_interest))
Standard_Sample_P <- Reduce(Merge, list(PCIAT_of_interest,
                                        Demos_of_interest, Race, Barratt_of_interest,
                                        Physical_of_interest, BIA_of_interest, PAQ_of_interest, 
                                        SDS_of_interest, CIS_P_of_interest, Dx_of_interest))

#remove participants below 7 and above 15 years old
Standard_Sample_SR <- Standard_Sample_SR [(!(Standard_Sample_SR$Age >15) & !(Standard_Sample_SR$Age<7)),]

#remove all Na's
Standard_Sample_SR <- Standard_Sample_SR[complete.cases(Standard_Sample_SR), ]
Standard_Sample_P <- Standard_Sample_P[complete.cases(Standard_Sample_P), ]

#create symptom count score for YFAS
YFAS_C$Score_1 <- ifelse(YFAS_C$YFAS_C_Score_01 >= 1, 1, 0)
YFAS_C$Score_2 <- ifelse(YFAS_C$YFAS_C_Score_02 >= 1, 1, 0)
YFAS_C$Score_3 <- ifelse(YFAS_C$YFAS_C_Score_03 >= 1, 1, 0)
YFAS_C$Score_4 <- ifelse(YFAS_C$YFAS_C_Score_04 >= 1, 1, 0)
YFAS_C$Score_5 <- ifelse(YFAS_C$YFAS_C_Score_05 >= 1, 1, 0)
YFAS_C$Score_6 <- ifelse(YFAS_C$YFAS_C_Score_06 >= 1, 1, 0)
YFAS_C$Score_7 <- ifelse(YFAS_C$YFAS_C_Score_07 >= 1, 1, 0)

YFAS_C$Symptom_Count <- YFAS_C$Score_1 + YFAS_C$Score_2 + YFAS_C$Score_3 +
  YFAS_C$Score_4 + YFAS_C$Score_5 + YFAS_C$Score_6 + YFAS_C$Score_7

YFAS$Score_1 <- ifelse(YFAS$YFAS_Score_01 >= 1, 1, 0)
YFAS$Score_2 <- ifelse(YFAS$YFAS_Score_02 >= 1, 1, 0)
YFAS$Score_3 <- ifelse(YFAS$YFAS_Score_03 >= 1, 1, 0)
YFAS$Score_4 <- ifelse(YFAS$YFAS_Score_04 >= 1, 1, 0)
YFAS$Score_5 <- ifelse(YFAS$YFAS_Score_05 >= 1, 1, 0)
YFAS$Score_6 <- ifelse(YFAS$YFAS_Score_06 >= 1, 1, 0)
YFAS$Score_7 <- ifelse(YFAS$YFAS_Score_07 >= 1, 1, 0)

YFAS$Symptom_Count <- YFAS$Score_1 + YFAS$Score_2 + YFAS$Score_3 +
  YFAS$Score_4 + YFAS$Score_5 + YFAS$Score_6 + YFAS$Score_7


#pull out all things of interest

IAT_of_interest <- data.frame(
  ID = IAT$URSI,
  IAT_SR = IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$URSI,
  IAT_Parent = PCIAT$PCIAT_Total)

Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total)

AUDIT_of_interest <- data.frame(
  ID = AUDIT$URSI, 
  Alcohol = AUDIT$AUDIT_Total_Score)

ESPAD_of_interest <- data.frame(
  ID = ESPAD$URSI,
  Cannabis = ESPAD$ESPAD_01d)

FTQA_of_interest <- data.frame(
  ID = FTQA$URSI,
  Nicotine = FTQA$FTQA_Total)

FTND_of_interest <- data.frame(
  ID = FTND$URSI,
  Nicotine = FTND$FTND_Total)

YFAS_A_of_interest <- data.frame(
  ID = YFAS$URSI,
  Dysreg._Eating_Impairment = YFAS$YFAS_Score_08,
  Dysreg._Eating_Symptom_Count = YFAS$Symptom_Count)

YFAS_C_of_interest <- data.frame(
  ID = YFAS_C$URSI,
  Dysreg._Eating_Impairment = YFAS_C$YFAS_C_Score_08,
  Dysreg._Eating_Symptom_Count = YFAS_C$Symptom_Count)

CIS_P_of_interest <- data.frame(
  ID = CIS_P$URSI,
  CIS_P_Score = CIS_P$CIS_P_Score)

CIS_SR_of_interest <- data.frame(
  ID = CIS_SR$URSI,
  CIS_SR_Score = CIS_SR$CIS_SR_Total)

ASSQ_of_interest <- data.frame(
  ID = ASSQ$URSI,
  ASSQ_Total = ASSQ$ASSQ_Total)

Conners_of_interest <- data.frame(
  ID = C3SR$URSI,
  Conners_Hyperactivity_Impulsivity = C3SR$C3SR_HY_T,
  Conners_Inattention = C3SR$C3SR_IN_T,
  Conners_Learning_Problems = C3SR$C3SR_LP_T)

SWAN_of_interest <- data.frame(
  ID = SWAN$URSI,
  SWAN_Inattentive = SWAN$SWAN_IN,
  SWAN_Hyperactive = SWAN$SWAN_HY,
  SWAN_Total = SWAN$SWAN_Total)

SCARED_SR_of_interest <- data.frame(
  ID = SCARED_SR$URSI,
  Social_Anxiety_SR = SCARED_SR$SCARED_SR_SC,
  Total_Anxiety_SR = SCARED_SR$SCARED_SR_Total)

SCARED_P_of_interest <- data.frame(
  ID = SCARED_P$URSI,
  Social_Anxiety_P = SCARED_P$SCARED_P_SC,
  Total_Anxiety_P = SCARED_P$SCARED_P_Total)

MFQ_SR_of_interest <- data.frame(
  ID = MFQ_SR$URSI,
  Depression_SR = MFQ_SR$MFQ_SR_Total)

MFQ_P_of_interest <- data.frame(
  ID = MFQ_P$URSI,
  Depression_P = MFQ_P$MFQ_P_Total)

CCSC_of_interest <- data.frame(
  ID = CCSC$URSI,
  Problem_Focused_Coping = CCSC$CCSC_PFC,
  Avoidance_Coping = CCSC$CCSC_AC,
  Positive_Cognitive_Restructuring = CCSC$CCSC_PCR,
  Religion = CCSC$CCSC_REL,
  Support_Seeking = CCSC$CCSC_SS)

Physical_of_interest <- data.frame(
  ID = Physical$URSI,
  BMI = Physical$BMI)

BIA_of_interest <- data.frame(
  ID = BIA$URSI,
  FMI = BIA$FMI)

PAQ_A_of_interest <- data.frame(
  ID = PAQ_A$URSI,
  PAQ_Total = PAQ_A$PAQ_A_Total)

PAQ_C_of_interest <- data.frame(
  ID = PAQ_C$URSI,
  PAQ_Total = PAQ_C$PAQ_C_Total)

SDS_of_interest <- data.frame(
  ID = SDS$URSI,
  SDS_Total = SDS$SDS_Total_T)

#merge dataframes for nicotine, YFAS and PAQ
Nicotine_of_interest = rbind(FTQA_of_interest, FTND_of_interest)

YFAS_of_interest = rbind(YFAS_A_of_interest, YFAS_C_of_interest)

PAQ_of_interest <- rbind(PAQ_A_of_interest, PAQ_C_of_interest)

#create all dataframes for all analyses
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Fitness_Sleep_Reg <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest,
                                        Physical_of_interest, BIA_of_interest, PAQ_of_interest, SDS_of_interest))

Reg_CCSC <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CCSC_of_interest))

linear_regression <- Reduce(Merge, list(IAT_of_interest,
                                        PCIAT_of_interest, ASSQ_of_interest, 
                                        Conners_of_interest, SWAN_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                        MFQ_SR_of_interest, MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest))

Logistic_Regression <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                          PCIAT_of_interest, Demos_of_interest, Barratt_of_interest))

Corr_substance <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, AUDIT_of_interest, ESPAD_of_interest, 
                                     Nicotine_of_interest, YFAS_of_interest, Demos_of_interest, 
                                     Barratt_of_interest))

Logistic_Regression_with_CIS <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                                   PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CIS_P_of_interest, CIS_SR_of_interest))

linear_regression_with_CIS <- Reduce(Merge, list(IAT_of_interest,
                                                 PCIAT_of_interest, ASSQ_of_interest, 
                                                 Conners_of_interest, SWAN_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                                 MFQ_SR_of_interest, MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest, 
                                                 CIS_P_of_interest, CIS_SR_of_interest))

#remove all rows with missing data
Fitness_Sleep_Reg <- Fitness_Sleep_Reg[complete.cases(Fitness_Sleep_Reg), ]
Reg_CCSC <- Reg_CCSC[complete.cases(Reg_CCSC), ]
linear_regression <- linear_regression[complete.cases(linear_regression), ]
Logistic_Regression <- Logistic_Regression[complete.cases(Logistic_Regression), ]
Corr_substance <- Corr_substance[complete.cases(Corr_substance), ]
Logistic_Regression_with_CIS <- Logistic_Regression_with_CIS[complete.cases(Logistic_Regression_with_CIS), ]
linear_regression_with_CIS <- linear_regression_with_CIS[complete.cases(linear_regression_with_CIS), ]


#make new full sample df including variable of existence of second caregiver
#pull out data of interest (Dx_of_interest already created above)
IAT_of_interest <- data.frame(
  ID = IAT$URSI,
  IAT_SR = IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$URSI,
  IAT_Parent = PCIAT$PCIAT_Total)

Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

Race <- data.frame(
  ID = PreInt_Demos_Fam$URSI,
  Race = PreInt_Demos_Fam$Child_Race)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total)

Physical_of_interest <- data.frame(
  ID = Physical$URSI,
  BMI = Physical$BMI)

BIA_of_interest <- data.frame(
  ID = BIA$URSI,
  FMI = BIA$FMI)

PAQ_A_of_interest <- data.frame(
  ID = PAQ_A$URSI,
  PAQ_Total = PAQ_A$PAQ_A_Total)

PAQ_C_of_interest <- data.frame(
  ID = PAQ_C$URSI,
  PAQ_Total = PAQ_C$PAQ_C_Total)

SDS_of_interest <- data.frame(
  ID = SDS$URSI,
  SDS_Total = SDS$SDS_Total_T)

CIS_P_of_interest <- data.frame(
  ID = CIS_P$URSI,
  CIS_P_Score = CIS_P$CIS_P_Score)

CIS_SR_of_interest <- data.frame(
  ID = CIS_SR$URSI,
  CIS_SR_Score = CIS_SR$CIS_SR_Total)

#merge PAQ dataframes
PAQ_of_interest <- rbind(PAQ_A_of_interest, PAQ_C_of_interest)

#construct final sample df for both Parent and SR
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Standard_Sample_SR <- Reduce(Merge, list(IAT_of_interest,
                                         Demos_of_interest, Race, Barratt_of_interest, 
                                         Physical_of_interest, 
                                         BIA_of_interest, PAQ_of_interest, SDS_of_interest, 
                                         CIS_SR_of_interest, Dx_of_interest))
Standard_Sample_P <- Reduce(Merge, list(PCIAT_of_interest,
                                        Demos_of_interest, Race, Barratt_of_interest,
                                        Physical_of_interest, BIA_of_interest, PAQ_of_interest, 
                                        SDS_of_interest, CIS_P_of_interest, Dx_of_interest))
#remove all Na's
Standard_Sample_SR <- Standard_Sample_SR[complete.cases(Standard_Sample_SR), ]
Standard_Sample_P <- Standard_Sample_P[complete.cases(Standard_Sample_P), ]

#add barratt items back in
colnames(Barratt)[1] <- "ID"
Barratt_Standard <- merge(Standard_Sample_SR[,1:2], Barratt[, 1:9], by.y = "ID", all = FALSE)

#create dummy variable for 1 caregiver only based on Barratt responses
Barratt_Standard$No_C2 <- ifelse(is.na(Barratt_Standard$Barratt_P2_Edu) & 
                                   is.na(Barratt_Standard$Barratt_P2_Occ), 1, 0)
