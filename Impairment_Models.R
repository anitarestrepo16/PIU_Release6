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

# make data frames
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

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total)

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

#dataframe for regression -- dxs
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Logistic_Regression_with_CIS <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                          PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CIS_P_of_interest, CIS_SR_of_interest))
Logistic_Regression_with_CIS <- Logistic_Regression_with_CIS[complete.cases(Logistic_Regression_with_CIS), ]
#linear_regression_with_CIS <- Reduce(Merge, list(IAT_of_interest,
#                                        PCIAT_of_interest, ASSQ_of_interest, 
#                                        Conners_of_interest, SWAN_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
#                                        MFQ_SR_of_interest, MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest, 
#                                        CIS_P_of_interest, CIS_SR_of_interest))
#regression with IAT predicting CIS
summary(lm(CIS_SR_Score ~ IAT_SR, data = Logistic_Regression_with_CIS))
confint(lm(CIS_SR_Score ~ IAT_SR, data = Logistic_Regression_with_CIS))
summary(lm(CIS_P_Score ~ IAT_Parent, data = Logistic_Regression_with_CIS))
confint(lm(CIS_P_Score ~ IAT_Parent, data = Logistic_Regression_with_CIS))

#regression with IAT and covariates predicting CIS
summary(lm(CIS_SR_Score ~ IAT_SR + Age + Sex + Site + SES, data = Logistic_Regression_with_CIS))
confint(lm(CIS_SR_Score ~ IAT_SR + Age + Sex + Site + SES, data = Logistic_Regression_with_CIS))
summary(lm(CIS_P_Score ~ IAT_Parent + Age + Sex + Site + SES, data = Logistic_Regression_with_CIS))
confint(lm(CIS_P_Score ~ IAT_Parent + Age + Sex + Site + SES, data = Logistic_Regression_with_CIS))

#regression with CIS predicted by consensus dxs
summary(lm(CIS_SR_Score ~ IAT_SR + Age + Sex + Site + SES + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Logistic_Regression_with_CIS))
confint(lm(CIS_SR_Score ~ IAT_SR + Age + Sex + Site + SES + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Logistic_Regression_with_CIS))
summary(lm(CIS_P_Score ~ IAT_Parent + Age + Sex + Site + SES + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Logistic_Regression_with_CIS))
confint(lm(CIS_P_Score ~ IAT_Parent + Age + Sex + Site + SES + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Logistic_Regression_with_CIS))

#regression with CIS predicted by dimensional symptoms - not using this anymore (as of 3.14.19)
summary(lm(CIS_SR_Score ~ IAT_SR + Conners_Hyperactivity_Impulsivity + Conners_Inattention + Conners_Learning_Problems + 
             Social_Anxiety_SR + Total_Anxiety_SR +
             Depression_SR + Sex + Age + SES + Site, data = linear_regression_with_CIS))
summary(lm(CIS_P_Score ~ IAT_Parent + ASSQ_Total +
             SWAN_Inattentive + SWAN_Hyperactive + SWAN_Total + Social_Anxiety_P + Total_Anxiety_P +
             Depression_P + Sex + Age + SES + Site, data = linear_regression_with_CIS))

#SR: Scatterplot of IAT and CIS scores
library(psych)
pdf(file="IAT_CIS_Scatterplot.pdf", width = 25, height = 25)
par(mar=c(12,12,12,2)+0.1,mgp=c(8,3,0))
plot(Logistic_Regression_with_CIS$IAT_SR, Logistic_Regression_with_CIS$CIS_SR_Score, main="IAT Scores vs. CIS Scores", 
     xlab="Self-Report IAT Total Score ", ylab="Self-Report CIS Total Score", cex.main = 4, cex.lab = 4, cex.axis = 4, cex =4)
abline(lm(CIS_SR_Score ~ IAT_SR, data = Logistic_Regression_with_CIS), col="blue", lwd = 4) # regression line (y~x)
R2 <- format(summary(lm(Logistic_Regression_with_CIS$CIS_SR_Score ~ 
                   Logistic_Regression_with_CIS$IAT_SR))$adj.r.squared, digits=4)
legend("topright", bty="n", cex = 4, legend = expression(paste("R"^"2"*"= 0.065")))
dev.off()

#P: Scatterplot of IAT and CIS scores
library(psych)
pdf(file="PCIAT_CIS_Scatterplot.pdf", width = 25, height = 25)
par(mar=c(12,12,12,2)+0.1,mgp=c(8,3,0))
plot(Logistic_Regression_with_CIS$IAT_Parent, Logistic_Regression_with_CIS$CIS_P_Score, main="IAT Scores vs. CIS Scores", 
     xlab="Parent-Report IAT Total Score ", ylab="Parent-Report CIS Total Score", cex.main = 4, cex.lab = 4, cex.axis = 4, cex =4)
abline(lm(CIS_P_Score ~ IAT_Parent, data = Logistic_Regression_with_CIS), col="blue", lwd = 4) # regression line (y~x)
R2 <- format(summary(lm(CIS_P_Score ~ IAT_Parent, data = Logistic_Regression_with_CIS))$adj.r.squared, digits=4)
legend("topright", bty="n", cex = 4, legend = expression(paste("R"^"2"*"= 0.092")))
dev.off()

#Added Variable Plots
library(car)
#SR:
avPlots(lm(CIS_P_Score ~ IAT_Parent + Age + Sex + Site + SES + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
     Social_Anxiety, data = Logistic_Regression_with_CIS))
avPlots(lm(CIS_SR_Score ~ IAT_SR + Conners_Hyperactivity_Impulsivity + Conners_Inattention + Conners_Learning_Problems + 
             Social_Anxiety_SR + Total_Anxiety_SR +
             Depression_SR + Sex + Age + SES + Site, data = linear_regression_with_CIS))

#P:
avPlots(lm(CIS_P_Score ~ IAT_Parent + Age + Sex + Site + SES + ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
             Social_Anxiety, data = Logistic_Regression_with_CIS))
avPlots(lm(CIS_P_Score ~ IAT_Parent + ASSQ_Total +
             SWAN_Inattentive + SWAN_Hyperactive + SWAN_Total + Social_Anxiety_P + Total_Anxiety_P +
             Depression_P + Sex + Age + SES + Site, data = linear_regression_with_CIS))

