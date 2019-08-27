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

############################ Exploratory Factor Analysis for all ages together #####################

# EFA code specifying categorical variables
IAT_Standard[,-c(1:2)]<-lapply(IAT_Standard[,-c(1:2)], as.numeric)
nfactors(IAT_Standard[,-c(1:2)])
fa.parallel(IAT_Items, fa = "fa", cor = "poly") # fa specifies factor analysis instead of principal components/ poly specifies variables are categorical

fa.poly(IAT_Items, nfactors = 3) # EFA retaining 3 factors
fa.poly(IAT_Items, nfactors = 2)
fa.poly(IAT_Items, nfactors = 4)


# parallel analysis to determine how many factors to keep after principal component analysis
library(paran)

paran(IAT_Standard[,-c(1:2)], iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

paran(PCIAT_Standard[,-c(1:2)], iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

# EFA with indicated number of factors

#data.pca <- princomp(IAT_Items[2:21])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(IAT_Standard[,-c(1:2)], factors = 5, rotation = "varimax")
data.fa1

#data.pca <- princomp(PCIAT_Items[2:21])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(PCIAT_Standard[,-c(1:2)], factors = 5, rotation = "varimax")
data.fa1


#########################  EFA for different age windows ##################################

# create different dataframes for each age group with NO missing data
IAT_8_12 <- na.omit(IAT_age[IAT_age$Age >= 8 & IAT_age$Age < 12,])
IAT_10_14 <- na.omit(IAT_age[IAT_age$Age >= 10 & IAT_age$Age < 14,])
IAT_12_16 <- na.omit(IAT_age[IAT_age$Age >= 12 & IAT_age$Age < 16,])
IAT_14_18 <- na.omit(IAT_age[IAT_age$Age >= 14 & IAT_age$Age < 18,])
IAT_16_20 <- na.omit(IAT_age[IAT_age$Age >= 16 & IAT_age$Age < 20,])
IAT_18_22 <- na.omit(IAT_age[IAT_age$Age >=18,])

PCIAT_5_9 <- na.omit(PCIAT_age[IAT_age$Age < 9,])
PCIAT_7_11 <- na.omit(PCIAT_age[IAT_age$Age >= 7 & IAT_age$Age < 11,])
PCIAT_9_13 <- na.omit(PCIAT_age[IAT_age$Age >= 9 & IAT_age$Age < 13,])
PCIAT_11_15 <- na.omit(PCIAT_age[IAT_age$Age >= 11 & IAT_age$Age < 15,])
PCIAT_13_17 <- na.omit(PCIAT_age[IAT_age$Age >= 13 & IAT_age$Age < 17,])
PCIAT_15_19 <- na.omit(PCIAT_age[IAT_age$Age >= 15 & IAT_age$Age < 19,])
PCIAT_17_22 <- na.omit(PCIAT_age[IAT_age$Age >=17,])


# IAT
#8-12
paran(na.omit(IAT_8_12[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(IAT_8_12[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(IAT_8_12[3:22]), factors = 3, rotation = "varimax")
data.fa1

#10-14
paran(na.omit(IAT_8_12[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(IAT_10_14[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(IAT_10_14[3:22]), factors = 3, rotation = "varimax")
data.fa1

#12-16
paran(na.omit(IAT_12_16[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(IAT_12_16[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(IAT_12_16[3:22]), factors = 4, rotation = "varimax")
data.fa1

#14-18
paran(na.omit(IAT_14_18[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(IAT_14_18[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(IAT_14_18[3:22]), factors = 3, rotation = "varimax")
data.fa1

#16-20
paran(na.omit(IAT_16_20[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(IAT_16_20[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(IAT_16_20[3:22]), factors = 4, rotation = "varimax")
data.fa1

#18-22
paran(na.omit(IAT_18_22[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(IAT_18_22[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(IAT_18_22[3:22]), factors = 2, rotation = "varimax")
data.fa1

# PCIAT
#5-9
paran(na.omit(PCIAT_5_9[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(PCIAT_5_9[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(PCIAT_5_9[3:22]), factors = 5, rotation = "varimax")
data.fa1

#7-11
paran(na.omit(PCIAT_7_11[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(PCIAT_7_11[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(PCIAT_7_11[3:22]), factors = 5, rotation = "varimax")
data.fa1

#9-13
paran(na.omit(PCIAT_9_13[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(PCIAT_9_13[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(PCIAT_9_13[3:22]), factors = 4, rotation = "varimax")
data.fa1

#11-15
paran(na.omit(PCIAT_11_15[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(PCIAT_11_15[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(PCIAT_11_15[3:22]), factors = 4, rotation = "varimax")
data.fa1

#13-17
paran(na.omit(PCIAT_13_17[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(PCIAT_13_17[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(PCIAT_13_17[3:22]), factors = 3, rotation = "varimax")
data.fa1

#15-19
paran(na.omit(PCIAT_15_19[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(PCIAT_15_19[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(PCIAT_15_19[3:22]), factors = 5, rotation = "varimax")
data.fa1

#17-22
paran(na.omit(PCIAT_17_22[3:22]), iterations = 10000, centile = 95, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 0)

#data.pca <- princomp(PCIAT_17_22[3:22])
#summary(data.pca)
#plot(data.pca, main = "Scree Plot")
data.fa1 <- factanal(na.omit(PCIAT_17_22[3:22]), factors = 2, rotation = "varimax")
data.fa1
