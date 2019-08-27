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

###################### Correlation Matrices for CIS and diagnosis data ##########
#create data frames
#consensus dxs
#pull out data of interest (Dx_of_interest already created)
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

#dataframe for regression -- dxs
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Logistic_Regression_SR <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                             Demos_of_interest, Barratt_of_interest, CIS_SR_of_interest))
Logistic_Regression_P <- Reduce(Merge, list(Dx_of_interest,
                                            PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CIS_P_of_interest))

#dimensional stuff
#pull out data of interest
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

CIS_P_of_interest <- data.frame(
  ID = CIS_P$URSI,
  CIS_P_Score = CIS_P$CIS_P_Score)

CIS_SR_of_interest <- data.frame(
  ID = CIS_SR$URSI,
  CIS_SR_Score = CIS_SR$CIS_SR_Total)

#dataframe for dimensional q's regression
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
linear_regression_SR <- Reduce(Merge, list(IAT_of_interest,
                                        Conners_of_interest, SCARED_SR_of_interest,
                                        MFQ_SR_of_interest, Demos_of_interest, Barratt_of_interest, 
                                       CIS_SR_of_interest))
linear_regression_P <- Reduce(Merge, list(PCIAT_of_interest, ASSQ_of_interest, 
                                          SWAN_of_interest, SCARED_P_of_interest,
                                          MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest, 
                                           CIS_P_of_interest))

#correlation matrices
#consensus dx - SR
library(psych)
correlationvars2 <- Logistic_Regression_SR[,-1]
rvalues2 <- cor(correlationvars2, use="pairwise.complete.obs")
#get P and n values for correlations
significance2 <- corr.test(correlationvars2, adjust="fdr")
pvalues2 <- significance2$p
nvalues2 <- array(significance2$n)
# Set plot parameters, color, and labels
par(mfrow=c(1,1))
col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                           "cyan", "#007FFF","#00007F"))
#PLOT CORRELATION MATRIX
library(corrplot)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
#save correlation matrix
pdf(file="Logistic_Regression_SR.pdf", width = 100, height = 100)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper", tl.cex = 10, cl.cex = 10)
dev.off()

#consensus dx - P
library(psych)
correlationvars2 <- Logistic_Regression_P[,-1]
rvalues2 <- cor(correlationvars2, use="pairwise.complete.obs")
#get P and n values for correlations
significance2 <- corr.test(correlationvars2, adjust="fdr")
pvalues2 <- significance2$p
nvalues2 <- array(significance2$n)
# Set plot parameters, color, and labels
par(mfrow=c(1,1))
col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                           "cyan", "#007FFF","#00007F"))
#PLOT CORRELATION MATRIX
library(corrplot)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
#save correlation matrix
pdf(file="Logistic_Regression_P.pdf", width = 100, height = 100)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper", tl.cex = 10, cl.cex = 10)
dev.off()

#dimensional - SR
library(psych)
correlationvars2 <- linear_regression_SR[,-1]
rvalues2 <- cor(correlationvars2, use="pairwise.complete.obs")
#get P and n values for correlations
significance2 <- corr.test(correlationvars2, adjust="fdr")
pvalues2 <- significance2$p
nvalues2 <- array(significance2$n)
# Set plot parameters, color, and labels
par(mfrow=c(1,1))
col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                           "cyan", "#007FFF","#00007F"))
#PLOT CORRELATION MATRIX
library(corrplot)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
#save correlation matrix
pdf(file="linear_regression_SR.pdf", width = 100, height = 100)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper", tl.cex = 10, cl.cex = 10)
dev.off()

#dimensional - P
library(psych)
correlationvars2 <- linear_regression_P[,-1]
rvalues2 <- cor(correlationvars2, use="pairwise.complete.obs")
#get P and n values for correlations
significance2 <- corr.test(correlationvars2, adjust="fdr")
pvalues2 <- significance2$p
nvalues2 <- array(significance2$n)
# Set plot parameters, color, and labels
par(mfrow=c(1,1))
col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                           "cyan", "#007FFF","#00007F"))
#PLOT CORRELATION MATRIX
library(corrplot)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
#save correlation matrix
pdf(file="linear_regression_P.pdf", width = 100, height = 100)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper", tl.cex = 10, cl.cex = 10)
dev.off()

####################### testing for multicollinearity #############
#make Nas into zeros
Logistic_Regression_SR[is.na(Logistic_Regression_SR)] <- 0
Logistic_Regression_P[is.na(Logistic_Regression_P)] <- 0
linear_regression_SR[is.na(linear_regression_SR)] <- 0
linear_regression_P[is.na(linear_regression_P)] <- 0

####eigenvalue analysis

#consensus dx - SR
eigen(cor(Logistic_Regression_SR[,-1]))$values
kappa(cor(Logistic_Regression_SR[,-1]), exact = TRUE)

#consensus dx - P
eigen(cor(Logistic_Regression_P[,-1]))$values
kappa(cor(Logistic_Regression_P[,-1]), exact = TRUE)

#dimensional - SR
eigen(cor(linear_regression_SR[,-1]))$values
kappa(cor(linear_regression_SR[,-1]), exact = TRUE)

#dimensional - P
eigen(cor(linear_regression_P[,-1]))$values
kappa(cor(linear_regression_P[,-1]), exact = TRUE)

##### VIF
library(car)
#consensus dx - SR
vif(lm(IAT_SR ~ ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
                    Social_Anxiety + Sex + Age + SES + Site + CIS_SR_Score, data = Logistic_Regression))
#consensus dx - P
vif(lm(IAT_Parent ~ ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
         Social_Anxiety + Sex + Age + SES + Site + CIS_P_Score, data = Logistic_Regression))
#dimensional - SR
vif(lm(IAT_SR ~ Conners_Hyperactivity_Impulsivity + Conners_Inattention + Conners_Learning_Problems + 
         Social_Anxiety_SR + Total_Anxiety_SR +
         Depression_SR + Sex + Age + SES + Site + CIS_SR_Score, data = linear_regression))
#dimensional - P
vif(lm(IAT_Parent ~ ASSQ_Total +
         SWAN_Inattentive + SWAN_Hyperactive + SWAN_Total + Social_Anxiety_P + Total_Anxiety_P +
         Depression_P + Sex + Age + SES + Site + CIS_P_Score, data = linear_regression))
