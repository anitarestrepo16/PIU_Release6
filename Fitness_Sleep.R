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

#variables of interest for fitness/sleep regressions
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

#merge PAQ dataframes
PAQ_of_interest <- rbind(PAQ_A_of_interest, PAQ_C_of_interest)


#create dataframe for regressions
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Fitness_Sleep_Reg <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest,
                                        Physical_of_interest, BIA_of_interest, PAQ_of_interest, SDS_of_interest, Dx_of_interest))
#delete all NAs
Fitness_Sleep_Reg <- Fitness_Sleep_Reg[complete.cases(Fitness_Sleep_Reg), ]

#create centered versions of age and fitness variables for interaction terms
Fitness_Sleep_Reg$Age_Centered <- Fitness_Sleep_Reg$Age - mean(Fitness_Sleep_Reg$Age)
Fitness_Sleep_Reg$PAQ_Centered <- Fitness_Sleep_Reg$PAQ_Total - mean(Fitness_Sleep_Reg$PAQ_Total)
Fitness_Sleep_Reg$BMI_Centered <- Fitness_Sleep_Reg$BMI - mean(Fitness_Sleep_Reg$BMI)
Fitness_Sleep_Reg$FMI_Centered <- Fitness_Sleep_Reg$FMI - mean(Fitness_Sleep_Reg$FMI)

#Regression for IAT predicting PAQ scores
#SR
#unadjusted
summary(lm(PAQ_Total ~ IAT_SR, data = Fitness_Sleep_Reg))
confint(lm(PAQ_Total ~ IAT_SR, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(PAQ_Total ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(PAQ_Total ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for covariates + all dxs
summary(lm(PAQ_Total ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(PAQ_Total ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
#P
#unadjusted
summary(lm(PAQ_Total ~ IAT_Parent, data = Fitness_Sleep_Reg))
confint(lm(PAQ_Total ~ IAT_Parent, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(PAQ_Total ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(PAQ_Total ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for covariates + all dxs
summary(lm(PAQ_Total ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(PAQ_Total ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))

#Regression for IAT predicting BMI
#SR
#unadjusted
summary(lm(BMI ~ IAT_SR, data = Fitness_Sleep_Reg))
confint(lm(BMI ~ IAT_SR, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(BMI ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(BMI ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for covariates + all dxs
summary(lm(BMI ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(BMI ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
#P
#unadjusted
summary(lm(BMI ~ IAT_Parent, data = Fitness_Sleep_Reg))
confint(lm(BMI ~ IAT_Parent, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(BMI ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(BMI ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for covariates + all dxs
summary(lm(BMI ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(BMI ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))


#Regression for IAT predicting FMI (Fat Mass Index)
#SR
#unadjusted
summary(lm(FMI ~ IAT_SR, data = Fitness_Sleep_Reg))
confint(lm(FMI ~ IAT_SR, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(FMI ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(FMI ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for covariates + all dxs
summary(lm(FMI ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(FMI ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
#P
#unadjusted
summary(lm(FMI ~ IAT_Parent, data = Fitness_Sleep_Reg))
confint(lm(FMI ~ IAT_Parent, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(FMI ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(FMI ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for convariates + all dxs
summary(lm(FMI ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(FMI ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))

#Regression for IAT predicting SDS
#SR
#unadjusted
summary(lm(SDS_Total ~ IAT_SR, data = Fitness_Sleep_Reg))
confint(lm(SDS_Total ~ IAT_SR, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(SDS_Total ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(SDS_Total ~ IAT_SR + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for covariates + all dxs
summary(lm(SDS_Total ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(SDS_Total ~ IAT_SR + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
#P
#unadjusted
summary(lm(SDS_Total ~ IAT_Parent, data = Fitness_Sleep_Reg))
confint(lm(SDS_Total ~ IAT_Parent, data = Fitness_Sleep_Reg))
#adjusted for covariates
summary(lm(SDS_Total ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
confint(lm(SDS_Total ~ IAT_Parent + Sex + Age + SES + Site, data = Fitness_Sleep_Reg))
#adjusted for covariates + all dxs
summary(lm(SDS_Total ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))
confint(lm(SDS_Total ~ IAT_Parent + Sex + Age + SES + Site + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Fitness_Sleep_Reg))

#Regressions including interaction terms (flipped model w/ IAT as outcome)
#SR: PAQ
summary(lm(IAT_SR ~ PAQ_Total + Sex + Age + SES + Site + PAQ_Centered*Age_Centered, data = Fitness_Sleep_Reg))
#SR: BMI
summary(lm(IAT_SR ~ BMI + Sex + Age + SES + Site + BMI_Centered*Age_Centered, data = Fitness_Sleep_Reg))
#SR: FMI
summary(lm(IAT_SR ~ FMI + Sex + Age + SES + Site + FMI_Centered*Age_Centered, data = Fitness_Sleep_Reg))

#P: PAQ
summary(lm(IAT_Parent ~ PAQ_Total + Sex + Age + SES + Site + PAQ_Centered*Age_Centered, data = Fitness_Sleep_Reg))
#P: BMI
summary(lm(IAT_Parent ~ BMI + Sex + Age + SES + Site + BMI_Centered*Age_Centered, data = Fitness_Sleep_Reg))
#P: FMI
summary(lm(IAT_Parent ~ FMI + Sex + Age + SES + Site + FMI_Centered*Age_Centered, data = Fitness_Sleep_Reg))

#scatterplots for age interactions
#PAQ
summary(lm(Fitness_Sleep_Reg$PAQ_Total ~ Fitness_Sleep_Reg$Age))$adj.r.squared #find R-squared value to input into plot
pdf(file="PAQ_Age_Scatterplot.pdf", width = 25, height = 25)
par(mar=c(12,12,12,2)+0.1,mgp=c(8,3,0))
plot(Fitness_Sleep_Reg$Age, Fitness_Sleep_Reg$PAQ_Total, main="Age Vs. PAQ Scores", 
     xlab="Age", ylab="PAQ Score", cex.main = 4, cex.lab = 4, cex.axis = 4, cex =4)
abline(lm(Fitness_Sleep_Reg$PAQ_Total ~ Fitness_Sleep_Reg$Age), col="red", lwd = 4) # regression line (y~x)
legend("topright", bty="n", cex = 4, legend=expression(paste("R"^"2"*"= 0.134")))
dev.off()

#BMI
summary(lm(Fitness_Sleep_Reg$BMI ~ Fitness_Sleep_Reg$Age))$adj.r.squared #find R-squared value to input into plot
pdf(file="BMI_Age_Scatterplot.pdf", width = 25, height = 25)
par(mar=c(12,12,12,2)+0.1,mgp=c(8,3,0))
plot(Fitness_Sleep_Reg$Age, Fitness_Sleep_Reg$BMI, main="Age Vs. BMI", 
     xlab="Age", ylab="BMI", cex.main = 4, cex.lab = 4, cex.axis = 4, cex =4)
abline(lm(Fitness_Sleep_Reg$BMI ~ Fitness_Sleep_Reg$Age), col="red", lwd = 4) # regression line (y~x)
legend("topright", bty="n", cex = 4, legend=expression(paste("R"^"2"*"= 0.010")))
dev.off()

#FMI
summary(lm(Fitness_Sleep_Reg$FMI ~ Fitness_Sleep_Reg$Age))$adj.r.squared #find R-squared value to input into plot
pdf(file="FMI_Age_Scatterplot.pdf", width = 25, height = 25)
par(mar=c(12,12,12,2)+0.1,mgp=c(8,3,0))
plot(Fitness_Sleep_Reg$Age, Fitness_Sleep_Reg$FMI, main="Age Vs. FMI", 
     xlab="Age", ylab="FMI", cex.main = 4, cex.lab = 4, cex.axis = 4, cex =4)
abline(lm(Fitness_Sleep_Reg$FMI ~ Fitness_Sleep_Reg$Age), col="red", lwd = 4) # regression line (y~x)
legend("topright", bty="n", cex = 4, legend=expression(paste("R"^"2"*"= 0.064")))
dev.off()
