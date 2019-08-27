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

####create dataframes to calculate numbers
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

CCSC_of_interest <- data.frame(
  ID = CCSC$URSI,
  Problem_Focused_Coping = CCSC$CCSC_PFC,
  Avoidance_Coping = CCSC$CCSC_AC,
  Positive_Cognitive_Restructuring = CCSC$CCSC_PCR,
  Religion = CCSC$CCSC_REL,
  Support_Seeking = CCSC$CCSC_SS)

CIS_P_of_interest <- data.frame(
  ID = CIS_P$URSI,
  CIS_P_Score = CIS_P$CIS_P_Score)

CIS_SR_of_interest <- data.frame(
  ID = CIS_SR$URSI,
  CIS_SR_Score = CIS_SR$CIS_SR_Total)

#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
Categorical_IAT <- data.frame(ID = IAT$URSI, Problematic = ifelse (IAT$IAT_Total >=40, 1, 0))
Categorical_PCIAT <- data.frame(ID = PCIAT$URSI, Problematic = ifelse (PCIAT$PCIAT_Total >=40, 1, 0))

#merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

#dataframes for odds ratios
Threshold_df <- Reduce(Merge, list(Categorical_IAT, Categorical_PCIAT,
                                  Dx_of_interest, Demos_of_interest, Barratt_of_interest))
#dataframe for regression -- dxs
Logistic_Regression <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                          PCIAT_of_interest, Demos_of_interest, Barratt_of_interest))
#dataframe for regression -- q's
linear_Regression <- Reduce(Merge, list(IAT_of_interest,
                                        PCIAT_of_interest, ASSQ_of_interest, 
                                        Conners_of_interest, SWAN_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                        MFQ_SR_of_interest, MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest))
#dataframe for regression - impairment
Logistic_Regression_with_CIS <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                                   PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CIS_P_of_interest, CIS_SR_of_interest))
#dataframe for regression -- coping
Reg_CCSC <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, 
                               Barratt_of_interest, CCSC_of_interest, Dx_of_interest))
#remove all Nas for all dataframes
Threshold_df <- Threshold_df[complete.cases(Threshold_df), ]
Logistic_Regression <- Logistic_Regression[complete.cases(Logistic_Regression), ]
linear_Regression <- linear_Regression[complete.cases(linear_Regression), ]
Reg_CCSC <- Reg_CCSC[complete.cases(Reg_CCSC), ]
Logistic_Regression_with_CIS <- Logistic_Regression_with_CIS[complete.cases(Logistic_Regression_with_CIS), ]

#find overlap between dataframes
nrow(table(intersect(Logistic_Regression$ID, linear_Regression$ID)))
nrow(table(intersect(Logistic_Regression$ID, Logistic_Regression_with_CIS$ID)))
nrow(table(intersect(linear_Regression$ID, Logistic_Regression_with_CIS$ID)))
nrow(table(Reduce(intersect, list(Logistic_Regression$ID, linear_Regression$ID, Logistic_Regression_with_CIS$ID))))

#Venn Diagram for sample size overlaps
library(VennDiagram)
pdf(file="Samples_Venn.pdf", width = 100, height = 100)
grid.newpage()
draw.triple.venn(area1 = 1009, area2 = 830, area3 = 893, n12 = 830, n23 = 718, n13 = 893, 
                 n123 = 718, category = c("Categorical Regression/\nOdds Ratios\n(N = 1009)\n", "Impairment Regressions\n(N = 830)\n", 
                                          "Dimensional Regression\n(N = 893)\n"), lty = "solid", 
                 fill = c("skyblue", "pink1", "mediumorchid"), col = c("skyblue", "pink1", "mediumorchid"),
                 cex = 20, cat.cex = 20, cat.col = (c("skyblue", "pink1", "mediumorchid")), 
                 cat.pos = c(340, 0, 20), cat.dist = c(0.03, 0.04, -0.04))
dev.off()

################# UpSet Plot for dxs ##########
install.packages("UpSetR")
library(UpSetR)
#create df for dxs including no dx
all_Dxs <- data.frame(
  ID = ConsensusDx$URSI,
  ASD = ifelse ((ConsensusDx$DX_01_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_02_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_03_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_04_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_05_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_06_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_07_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_08_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_09_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_10_Sub == "Autism Spectrum Disorder"), 1, 0),
  Learning_Disorder = ifelse (ConsensusDx$DX_01_Sub == "Specific Learning Disorder" | ConsensusDx$DX_02_Sub == "Specific Learning Disorder" | ConsensusDx$DX_03_Sub == "Specific Learning Disorder" | ConsensusDx$DX_04_Sub == "Specific Learning Disorder" | ConsensusDx$DX_05_Sub == "Specific Learning Disorder" | ConsensusDx$DX_06_Sub == "Specific Learning Disorder" | ConsensusDx$DX_07_Sub == "Specific Learning Disorder" | ConsensusDx$DX_08_Sub == "Specific Learning Disorder" | ConsensusDx$DX_09_Sub == "Specific Learning Disorder" | ConsensusDx$DX_10_Sub == "Specific Learning Disorder", 1, 0),
  Anxiety = ifelse (ConsensusDx$DX_01_Cat == "Anxiety Disorders" | ConsensusDx$DX_02_Cat == "Anxiety Disorders" | ConsensusDx$DX_03_Cat == "Anxiety Disorders" | ConsensusDx$DX_04_Cat == "Anxiety Disorders" | ConsensusDx$DX_05_Cat == "Anxiety Disorders" | ConsensusDx$DX_06_Cat == "Anxiety Disorders" | ConsensusDx$DX_07_Cat == "Anxiety Disorders" | ConsensusDx$DX_08_Cat == "Anxiety Disorders" | ConsensusDx$DX_09_Cat == "Anxiety Disorders" | ConsensusDx$DX_10_Cat == "Anxiety Disorders", 1, 0),
  Depression = ifelse (ConsensusDx$DX_01_Cat == "Depressive Disorders" | ConsensusDx$DX_02_Cat == "Depressive Disorders" | ConsensusDx$DX_03_Cat == "Depressive Disorders" | ConsensusDx$DX_04_Cat == "Depressive Disorders" | ConsensusDx$DX_05_Cat == "Depressive Disorders" | ConsensusDx$DX_06_Cat == "Depressive Disorders" | ConsensusDx$DX_07_Cat == "Depressive Disorders" | ConsensusDx$DX_08_Cat == "Depressive Disorders" | ConsensusDx$DX_09_Cat == "Depressive Disorders" | ConsensusDx$DX_10_Cat == "Depressive Disorders", 1, 0),
  ADHD_Combined = ifelse (ConsensusDx$DX_01 == "ADHD-Combined Type" | ConsensusDx$DX_02 == "ADHD-Combined Type" | ConsensusDx$DX_03 == "ADHD-Combined Type" | ConsensusDx$DX_04 == "ADHD-Combined Type" | ConsensusDx$DX_05 == "ADHD-Combined Type" | ConsensusDx$DX_06 == "ADHD-Combined Type" | ConsensusDx$DX_07 == "ADHD-Combined Type" | ConsensusDx$DX_08 == "ADHD-Combined Type" | ConsensusDx$DX_09 == "ADHD-Combined Type" | ConsensusDx$DX_10 == "ADHD-Combined Type", 1, 0),
  ADHD_Inattentive = ifelse (ConsensusDx$DX_01 == "ADHD-Inattentive Type" | ConsensusDx$DX_02 == "ADHD-Inattentive Type" | ConsensusDx$DX_03 == "ADHD-Inattentive Type" | ConsensusDx$DX_04 == "ADHD-Inattentive Type" | ConsensusDx$DX_05 == "ADHD-Inattentive Type" | ConsensusDx$DX_06 == "ADHD-Inattentive Type" | ConsensusDx$DX_07 == "ADHD-Inattentive Type" | ConsensusDx$DX_08 == "ADHD-Inattentive Type" | ConsensusDx$DX_09 == "ADHD-Inattentive Type" | ConsensusDx$DX_10 == "ADHD-Inattentive Type", 1, 0),
  ADHD_Hyperactive = ifelse (ConsensusDx$DX_01 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_02 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_03 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_04 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_05 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_06 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_07 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_08 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_09 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_10 == "ADHD-Hyperactive/Impulsive Type", 1, 0),
  No_Dx = ifelse (ConsensusDx$NoDX == 1, 1, 0)
)
#add variable other_dx as 1 if all other columns are blank
all_Dxs$Other_Dx <- ifelse (rowSums(all_Dxs[, (2:9)]) == 0, 1, 0)

#simple upset plot as pdf
pdf(file="Upset.pdf", width = 320, height = 80, paper = "USr")
upset(all_Dxs, sets = c("ASD", "Learning_Disorder", "Anxiety", "Depression",
                               "ADHD_Combined", "ADHD_Inattentive", "ADHD_Hyperactive", "No_Dx", "Other_Dx"), 
      sets.bar.color = "#56B4E9",
      order.by = "freq", empty.intersections = "on", 
      text.scale = 1.5, keep.order = TRUE)
dev.off()

################## more Venn Diagrams for dx comorbidity #########
install.packages("venn")
library(venn)
#create df for dxs including no dx
all_Dxs <- data.frame(
  ID = ConsensusDx$URSI,
  ASD = ifelse ((ConsensusDx$DX_01_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_02_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_03_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_04_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_05_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_06_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_07_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_08_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_09_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_10_Sub == "Autism Spectrum Disorder"), 1, 0),
  Learning_Disorder = ifelse (ConsensusDx$DX_01_Sub == "Specific Learning Disorder" | ConsensusDx$DX_02_Sub == "Specific Learning Disorder" | ConsensusDx$DX_03_Sub == "Specific Learning Disorder" | ConsensusDx$DX_04_Sub == "Specific Learning Disorder" | ConsensusDx$DX_05_Sub == "Specific Learning Disorder" | ConsensusDx$DX_06_Sub == "Specific Learning Disorder" | ConsensusDx$DX_07_Sub == "Specific Learning Disorder" | ConsensusDx$DX_08_Sub == "Specific Learning Disorder" | ConsensusDx$DX_09_Sub == "Specific Learning Disorder" | ConsensusDx$DX_10_Sub == "Specific Learning Disorder", 1, 0),
  Anxiety = ifelse (ConsensusDx$DX_01_Cat == "Anxiety Disorders" | ConsensusDx$DX_02_Cat == "Anxiety Disorders" | ConsensusDx$DX_03_Cat == "Anxiety Disorders" | ConsensusDx$DX_04_Cat == "Anxiety Disorders" | ConsensusDx$DX_05_Cat == "Anxiety Disorders" | ConsensusDx$DX_06_Cat == "Anxiety Disorders" | ConsensusDx$DX_07_Cat == "Anxiety Disorders" | ConsensusDx$DX_08_Cat == "Anxiety Disorders" | ConsensusDx$DX_09_Cat == "Anxiety Disorders" | ConsensusDx$DX_10_Cat == "Anxiety Disorders", 1, 0),
  Depression = ifelse (ConsensusDx$DX_01_Cat == "Depressive Disorders" | ConsensusDx$DX_02_Cat == "Depressive Disorders" | ConsensusDx$DX_03_Cat == "Depressive Disorders" | ConsensusDx$DX_04_Cat == "Depressive Disorders" | ConsensusDx$DX_05_Cat == "Depressive Disorders" | ConsensusDx$DX_06_Cat == "Depressive Disorders" | ConsensusDx$DX_07_Cat == "Depressive Disorders" | ConsensusDx$DX_08_Cat == "Depressive Disorders" | ConsensusDx$DX_09_Cat == "Depressive Disorders" | ConsensusDx$DX_10_Cat == "Depressive Disorders", 1, 0),
  ADHD_Combined = ifelse (ConsensusDx$DX_01 == "ADHD-Combined Type" | ConsensusDx$DX_02 == "ADHD-Combined Type" | ConsensusDx$DX_03 == "ADHD-Combined Type" | ConsensusDx$DX_04 == "ADHD-Combined Type" | ConsensusDx$DX_05 == "ADHD-Combined Type" | ConsensusDx$DX_06 == "ADHD-Combined Type" | ConsensusDx$DX_07 == "ADHD-Combined Type" | ConsensusDx$DX_08 == "ADHD-Combined Type" | ConsensusDx$DX_09 == "ADHD-Combined Type" | ConsensusDx$DX_10 == "ADHD-Combined Type", 1, 0),
  ADHD_Inattentive = ifelse (ConsensusDx$DX_01 == "ADHD-Inattentive Type" | ConsensusDx$DX_02 == "ADHD-Inattentive Type" | ConsensusDx$DX_03 == "ADHD-Inattentive Type" | ConsensusDx$DX_04 == "ADHD-Inattentive Type" | ConsensusDx$DX_05 == "ADHD-Inattentive Type" | ConsensusDx$DX_06 == "ADHD-Inattentive Type" | ConsensusDx$DX_07 == "ADHD-Inattentive Type" | ConsensusDx$DX_08 == "ADHD-Inattentive Type" | ConsensusDx$DX_09 == "ADHD-Inattentive Type" | ConsensusDx$DX_10 == "ADHD-Inattentive Type", 1, 0),
  ADHD_Hyperactive = ifelse (ConsensusDx$DX_01 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_02 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_03 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_04 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_05 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_06 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_07 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_08 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_09 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_10 == "ADHD-Hyperactive/Impulsive Type", 1, 0),
  No_Dx = ifelse (ConsensusDx$NoDX == 1, 1, 0)
)
#add variable other_dx as 1 if all other columns are blank
all_Dxs$Other_Dx <- ifelse (rowSums(all_Dxs[, (2:9)]) == 0, 1, 0)
#remove ADHD-H, no_dx
all_Dxs <- all_Dxs[ , -(8:9)]
#make venn diagram
png(file = "Dx_Comorbidity_Venn_7.png", width = 1000, height = 1000)
par(cex = 3)
venn(all_Dxs[, -1], 
     snames = c("ASD", "LD", "Anxiety", "Depression", "ADHD-C", "ADHD-I", "Other_Dx"),
     ilabels = TRUE, zcolor = c("aquamarine3", "blue2", "brown3", "burlywood3", 
     "chartreuse2", "darkgoldenrod2", "darkorchid1"), opacity = 0.5)
dev.off()

#grouping all ADHD into one
all_Dxs <- data.frame(
  ID = ConsensusDx$URSI,
  ASD = ifelse ((ConsensusDx$DX_01_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_02_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_03_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_04_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_05_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_06_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_07_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_08_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_09_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_10_Sub == "Autism Spectrum Disorder"), 1, 0),
  Learning_Disorder = ifelse (ConsensusDx$DX_01_Sub == "Specific Learning Disorder" | ConsensusDx$DX_02_Sub == "Specific Learning Disorder" | ConsensusDx$DX_03_Sub == "Specific Learning Disorder" | ConsensusDx$DX_04_Sub == "Specific Learning Disorder" | ConsensusDx$DX_05_Sub == "Specific Learning Disorder" | ConsensusDx$DX_06_Sub == "Specific Learning Disorder" | ConsensusDx$DX_07_Sub == "Specific Learning Disorder" | ConsensusDx$DX_08_Sub == "Specific Learning Disorder" | ConsensusDx$DX_09_Sub == "Specific Learning Disorder" | ConsensusDx$DX_10_Sub == "Specific Learning Disorder", 1, 0),
  Anxiety = ifelse (ConsensusDx$DX_01_Cat == "Anxiety Disorders" | ConsensusDx$DX_02_Cat == "Anxiety Disorders" | ConsensusDx$DX_03_Cat == "Anxiety Disorders" | ConsensusDx$DX_04_Cat == "Anxiety Disorders" | ConsensusDx$DX_05_Cat == "Anxiety Disorders" | ConsensusDx$DX_06_Cat == "Anxiety Disorders" | ConsensusDx$DX_07_Cat == "Anxiety Disorders" | ConsensusDx$DX_08_Cat == "Anxiety Disorders" | ConsensusDx$DX_09_Cat == "Anxiety Disorders" | ConsensusDx$DX_10_Cat == "Anxiety Disorders", 1, 0),
  Depression = ifelse (ConsensusDx$DX_01_Cat == "Depressive Disorders" | ConsensusDx$DX_02_Cat == "Depressive Disorders" | ConsensusDx$DX_03_Cat == "Depressive Disorders" | ConsensusDx$DX_04_Cat == "Depressive Disorders" | ConsensusDx$DX_05_Cat == "Depressive Disorders" | ConsensusDx$DX_06_Cat == "Depressive Disorders" | ConsensusDx$DX_07_Cat == "Depressive Disorders" | ConsensusDx$DX_08_Cat == "Depressive Disorders" | ConsensusDx$DX_09_Cat == "Depressive Disorders" | ConsensusDx$DX_10_Cat == "Depressive Disorders", 1, 0),
  ADHD_all = ifelse (ConsensusDx$DX_01_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_02_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_03_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_04_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_05_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_06_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_07_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_08_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_09_Sub == "Attention-Deficit/Hyperactivity Disorder" | ConsensusDx$DX_10_Sub == "Attention-Deficit/Hyperactivity Disorder", 1, 0),
  No_Dx = ifelse (ConsensusDx$NoDX == 1, 1, 0)
)
#add variable other_dx as 1 if all other columns are blank
all_Dxs$Other_Dx <- ifelse (rowSums(all_Dxs[, (2:7)]) == 0, 1, 0)
#remove no_dx
all_Dxs <- all_Dxs[ , -(7)]
#make venn diagram
png(file = "Dx_Comorbidity_Venn_6.png", width = 1000, height = 1000)
par(cex = 3)
venn(all_Dxs[, -1], 
     snames = c("ASD", "LD", "Anxiety", "Depression", "ADHD-all", "Other_Dx"),
     ilabels = TRUE, zcolor = c("aquamarine3", "blue2", "brown3", "burlywood3", 
                                "chartreuse2", "darkgoldenrod2"), opacity = 0.5)
dev.off()
