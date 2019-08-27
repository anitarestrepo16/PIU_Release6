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

########### make df with intersection of all samples -> final sample 
#(NOT including dimensional measures of psychoapthology) #########
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

#construct final sample df for both Parent and SR with full IAT and PCIAT indidivual items
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

#turn URSI to ID
colnames(IAT)[1] <- "ID"
colnames(PCIAT)[1] <- "ID"

#put items back into standard sample
IAT_Standard <- merge(Standard_Sample_SR[,1:2], IAT[, 1:21], by.y = "ID", all.y = FALSE, all.x = FALSE)
PCIAT_Standard <- merge(Standard_Sample_P[,1:2], PCIAT[, 1:21], by.y = "ID", all.y = FALSE, all.x = FALSE)


# create different dataframes for each age group
IAT_8_12 <- IAT_age[IAT_age$Age >= 8 & IAT_age$Age < 12,]
IAT_10_14 <- IAT_age[IAT_age$Age >= 10 & IAT_age$Age < 14,]
IAT_12_16 <- IAT_age[IAT_age$Age >= 12 & IAT_age$Age < 16,]
IAT_14_18 <- IAT_age[IAT_age$Age >= 14 & IAT_age$Age < 18,]
IAT_16_20 <- IAT_age[IAT_age$Age >= 16 & IAT_age$Age < 20,]
IAT_18_22 <- IAT_age[IAT_age$Age >=18,]

PCIAT_5_9 <- PCIAT_age[IAT_age$Age < 9,]
PCIAT_7_11 <- PCIAT_age[IAT_age$Age >= 7 & IAT_age$Age < 11,]
PCIAT_9_13 <- PCIAT_age[IAT_age$Age >= 9 & IAT_age$Age < 13,]
PCIAT_11_15 <- PCIAT_age[IAT_age$Age >= 11 & IAT_age$Age < 15,]
PCIAT_13_17 <- PCIAT_age[IAT_age$Age >= 13 & IAT_age$Age < 17,]
PCIAT_15_19 <- PCIAT_age[IAT_age$Age >= 15 & IAT_age$Age < 19,]
PCIAT_17_22 <- PCIAT_age[IAT_age$Age >=17,]

###################################### Confirmatory Factor Analysis for all ages together #######################

library(lavaan)
library(psych)

# tell R that IAT/PCIAT are categorical variables
IAT_Standard[, -c(1:2)] <-lapply(IAT_Standard[, -c(1:2)], ordered) 
PCIAT_Standard[, -c(1:2)] <-lapply(PCIAT_Standard[, -c(1:2)], ordered) 

#Unidimensional CFA
IAT_confirmatory_1f <- '
F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20+
IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17+
IAT_10 + IAT_12 + IAT_14+
IAT_02+IAT_07+IAT_11'
fit_IAT <- cfa(IAT_confirmatory_1f, data = IAT_Standard[, -c(1:2)], std.lv=TRUE)
summary (fit_IAT, fit.measures=TRUE)

#create IAT total score from 1-factor CFA
IAT_Items$IAT_fscores <- predict(fit_IAT)
hist(IAT_Items$IAT_fscores)

PCIAT_confirmatory_1f <- '
F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20+
PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17+
PCIAT_10 + PCIAT_12 + PCIAT_14+
PCIAT_02+PCIAT_07+PCIAT_11'
fit_PCIAT <- cfa(PCIAT_confirmatory_1f, data = PCIAT_Standard[, -c(1:2)], std.lv=TRUE)
summary (fit_PCIAT, fit.measures=TRUE)

# create total PCIAT scores from 1-factor CFA
PCIAT_Items$PCIAT_fscores <- predict(fit_PCIAT)
hist(PCIAT_Items$PCIAT_fscores)




# with Man Law & Kit Chang paper factors
IAT_confirmatory_1 <- 'F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20
F2 =~ IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17
F3 =~ IAT_10 + IAT_12 + IAT_14'
fit <- cfa(IAT_confirmatory_1, data = IAT_Standard[, -c(1:2)], std.lv=TRUE)
summary (fit, fit.measures=TRUE)

PCIAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(PCIAT_confirmatory_1, data = PCIAT_Standard[, -c(1:2)], std.lv=TRUE)
summary (fit, fit.measures=TRUE)

# with Widyanto and mcmurran paper factors

IAT_confirmatory_2 <- 'F1 =~ IAT_19 + IAT_13 + IAT_12 + IAT_15 + IAT_10
F2 =~ IAT_02 + IAT_14 + IAT_20 + IAT_01 + IAT_18
F3 =~ IAT_06 + IAT_08 + IAT_09
F4 =~ IAT_11 + IAT_07
F5 =~ IAT_17 + IAT_05 + IAT_16
F6 =~ IAT_04 + IAT_03'
fit <- cfa(IAT_confirmatory_2, data = IAT_Standard[, -c(1:2)], std.lv=TRUE)
summary (fit, fit.measures=TRUE) # std.lv=TRUE

PCIAT_confirmatory_2 <- 'F1 =~ PCIAT_19 + PCIAT_13 + PCIAT_12 + PCIAT_15 + PCIAT_10
F2 =~ PCIAT_02 + PCIAT_14 + PCIAT_20 + PCIAT_01 + PCIAT_18
F3 =~ PCIAT_06 + PCIAT_08 + PCIAT_09
F4 =~ PCIAT_11 + PCIAT_07
F5 =~ PCIAT_17 + PCIAT_05 + PCIAT_16
F6 =~ PCIAT_04 + PCIAT_03'
fit <- cfa(PCIAT_confirmatory_2, data = PCIAT_Standard[, -c(1:2)], std.lv=TRUE)
summary (fit, fit.measures=TRUE) # std.lv=TRUE

# with Faraci et al. factors (first 2-factor model, then 1-factor model)

IAT_confirmatory_3 <- 'F1 =~ IAT_20 + IAT_15 + IAT_03 + IAT_19 + IAT_18 + IAT_11 + IAT_12 + IAT_13 + IAT_10 + IAT_04 + IAT_14
F2 =~ IAT_02 + IAT_01 + IAT_16 + IAT_06 + IAT_05 + IAT_09 + IAT_07'
fit <- cfa(IAT_confirmatory_3, data = IAT, std.lv=TRUE)
summary (fit, fit.measures=TRUE) # std.lv=TRUE

PCIAT_confirmatory_3 <- 'F1 =~ PCIAT_20 + PCIAT_15 + PCIAT_03 + PCIAT_19 + PCIAT_18 + PCIAT_11 + PCIAT_12 + PCIAT_13 + PCIAT_10 + PCIAT_04 + PCIAT_14
F2 =~ PCIAT_02 + PCIAT_01 + PCIAT_16 + PCIAT_06 + PCIAT_05 + PCIAT_09 + PCIAT_07'
fit <- cfa(PCIAT_confirmatory_3, data = PCIAT, std.lv=TRUE)
summary (fit, fit.measures=TRUE) # std.lv=TRUE

IAT_confirmatory_4 <- 'F1 =~ IAT_01 + IAT_02 + IAT_03 + IAT_04 + IAT_05 + IAT_06 + IAT_07 + 
IAT_08 + IAT_09 + IAT_10 + IAT_11 + IAT_12 + IAT_13 + IAT_14 + IAT_15 + IAT_16 + IAT_17 + IAT_18 + IAT_19 + IAT_20'
fit <- cfa(IAT_confirmatory_4, data = IAT, std.lv=TRUE)
summary (fit, fit.measures=TRUE) # std.lv=TRUE

PCIAT_confirmatory_4 <- 'F1 =~ PCIAT_01 + PCIAT_02 + PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_06 + PCIAT_07 + 
PCIAT_08 + PCIAT_09 + PCIAT_10 + PCIAT_11 + PCIAT_12 + PCIAT_13 + PCIAT_14 + PCIAT_15 + PCIAT_16 + PCIAT_17 +
PCIAT_18 + PCIAT_19 + PCIAT_20'
fit <- cfa(PCIAT_confirmatory_4, data = PCIAT, std.lv=TRUE)
summary (fit, fit.measures=TRUE) # std.lv=TRUE

################## CFA for different age windows (Man Law & Kit Chang) #############################

# IAT
#8-12
IAT_confirmatory_1 <- 'F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20
F2 =~ IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17
F3 =~ IAT_10 + IAT_12 + IAT_14'
fit <- cfa(IAT_confirmatory_1, data = IAT_8_12, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#10-14
IAT_confirmatory_1 <- 'F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20
F2 =~ IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17
F3 =~ IAT_10 + IAT_12 + IAT_14'
fit <- cfa(IAT_confirmatory_1, data = IAT_10_14, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#12-16
IAT_confirmatory_1 <- 'F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20
F2 =~ IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17
F3 =~ IAT_10 + IAT_12 + IAT_14'
fit <- cfa(IAT_confirmatory_1, data = IAT_12_16, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#14-18
IAT_confirmatory_1 <- 'F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20
F2 =~ IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17
F3 =~ IAT_10 + IAT_12 + IAT_14'
fit <- cfa(IAT_confirmatory_1, data = IAT_14_18, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#16-20
IAT_confirmatory_1 <- 'F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20
F2 =~ IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17
F3 =~ IAT_10 + IAT_12 + IAT_14'
fit <- cfa(IAT_confirmatory_1, data = IAT_16_20, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#18-22
IAT_confirmatory_1 <- 'F1 =~ IAT_03 + IAT_04 + IAT_05 + IAT_09 + IAT_13 + IAT_15 + IAT_18 + IAT_19 + IAT_20
F2 =~ IAT_01 + IAT_06 + IAT_08 + IAT_16 + IAT_17
F3 =~ IAT_10 + IAT_12 + IAT_14'
fit <- cfa(IAT_confirmatory_1, data = IAT_18_22, std.lv=TRUE)
summary (fit, fit.measures=TRUE)


# PCIAT
#5-9
IAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(IAT_confirmatory_1, data = PCIAT_5_9, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#7-11
IAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(IAT_confirmatory_1, data = PCIAT_7_11, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#9-13
IAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(IAT_confirmatory_1, data = PCIAT_9_13, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#11-15
IAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(IAT_confirmatory_1, data = PCIAT_11_15, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#13-17
IAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(IAT_confirmatory_1, data = PCIAT_13_17, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#15-19
IAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(IAT_confirmatory_1, data = PCIAT_15_19, std.lv=TRUE)
summary (fit, fit.measures=TRUE)

#17-22
IAT_confirmatory_1 <- 'F1 =~ PCIAT_03 + PCIAT_04 + PCIAT_05 + PCIAT_09 + PCIAT_13 + PCIAT_15 + PCIAT_18 + PCIAT_19 + PCIAT_20
F2 =~ PCIAT_01 + PCIAT_06 + PCIAT_08 + PCIAT_16 + PCIAT_17
F3 =~ PCIAT_10 + PCIAT_12 + PCIAT_14'
fit <- cfa(IAT_confirmatory_1, data = PCIAT_17_22, std.lv=TRUE)
summary (fit, fit.measures=TRUE)
