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

###################### descriptives #############
#Dataframe with both PCIAT and IAT and demos
Merge <- function(x, y){
  df <- merge(x, y, by= "URSI", all.x=TRUE, all.y=TRUE)
  return(df)
}
IAT_and_PCIAT_and_demos <- Reduce(Merge, list(IAT, PCIAT, Basic_Demos))
#remove participants without IAT or PCIAT
IAT_and_PCIAT_and_demos <- IAT_and_PCIAT_and_demos[1:1131,]
#age: mean and SD
summary(floor(IAT_and_PCIAT_and_demos$Age))
sd(floor(IAT_and_PCIAT_and_demos$Age))
#number female
Sex_Table <- table(IAT_and_PCIAT_and_demos$Sex)
Sex_Table[names(Sex_Table)==1]
#Ethnicity break-down




##################### Q1. Parent-Child Discrepancies ##################

##### Scatterplot

#Dataframe with both PCIAT and IAT
Merge <- function(x, y){
  df <- merge(x, y, by= "URSI", all.x=TRUE, all.y=TRUE)
  return(df)
}
IAT_and_PCIAT <- Reduce(Merge, list(IAT, PCIAT))
#Scatterplot of total scores
library(psych)
pdf(file="IAT_PCIAT_Scatterplot.pdf", width = 25, height = 25)
par(mar=c(12,12,12,2)+0.1,mgp=c(8,3,0))
plot(IAT_and_PCIAT$IAT_Total, IAT_and_PCIAT$PCIAT_Total, main="Self-Report Vs. Parent-Report IAT Total Scores", 
     xlab="Self-Report IAT Total Score ", ylab="Parent-Report IAT Total Score", cex.main = 4, cex.lab = 4, cex.axis = 4, cex =4)
abline(lm(IAT_and_PCIAT$PCIAT_Total ~ IAT_and_PCIAT$IAT_Total), col="red", lwd = 4) # regression line (y~x)
legend("topright", bty="n", cex = 4, legend=paste("R2 =", format(summary(lm(IAT_and_PCIAT$PCIAT_Total ~ IAT_and_PCIAT$IAT_Total))$adj.r.squared, digits=4)))
zz <- dev.off() # send to garbage variable to avoid null device problem

####### Boxplot for comparisons
#pull out data of interest
IAT_of_interest <- data.frame(
  ID = IAT$URSI,
  IAT_SR = IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$URSI,
  IAT_Parent = PCIAT$PCIAT_Total)

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

# create z-scores for each APQ subscale and average out relevant subscales to form positive and negative parenting composites
APQ_P_of_interest <- data.frame(
  ID = APQ_P$URSI,
  Involvement_P = APQ_P$APQ_P_INV,
  Involvement_P_z = ((APQ_P$APQ_P_INV - mean(APQ_P$APQ_P_INV))/sd(APQ_P$APQ_P_INV)),
  Positive_Parenting_P = APQ_P$APQ_P_PP,
  Positive_Parenting_P_z = ((APQ_P$APQ_P_PP - mean(APQ_P$APQ_P_PP))/sd(APQ_P$APQ_P_PP)),
  Poor_Monitoring_P = APQ_P$APQ_P_PM,
  Poor_Monitoring_P_z = ((APQ_P$APQ_P_PM - mean(APQ_P$APQ_P_PM))/sd(APQ_P$APQ_P_PM)),
  Inconsistent_Discipline_P = APQ_P$APQ_P_ID,
  Inconsistent_Discipline_P_z = ((APQ_P$APQ_P_ID - mean(APQ_P$APQ_P_ID))/sd(APQ_P$APQ_P_ID)),
  Corporal_Punishment_P = APQ_P$APQ_P_CP,
  Corporal_Punishment_P_z = ((APQ_P$APQ_P_CP - mean(APQ_P$APQ_P_CP))/sd(APQ_P$APQ_P_CP)))

APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$URSI,
  Involvement_Mother_SR = APQ_SR$APQ_SR_INV_M,
  Involvement_Mother_SR_z = ((APQ_SR$APQ_SR_INV_M - mean(APQ_SR$APQ_SR_INV_M))/sd(APQ_SR$APQ_SR_INV_M)),
  Involvement_Father_SR = APQ_SR$APQ_SR_INV_D,
  Involvement_Father_SR_z = ((APQ_SR$APQ_SR_INV_D - mean(APQ_SR$APQ_SR_INV_D))/sd(APQ_SR$APQ_SR_INV_D)),
  Positive_Parenting_SR = APQ_SR$APQ_SR_PP,
  Positive_Parenting_SR_z = ((APQ_SR$APQ_SR_PP - mean(APQ_SR$APQ_SR_PP))/sd(APQ_SR$APQ_SR_PP)),
  Poor_Monitoring_SR = APQ_SR$APQ_SR_PM,
  Poor_Monitoring_SR_z = ((APQ_SR$APQ_SR_PM - mean(APQ_SR$APQ_SR_PM))/sd(APQ_SR$APQ_SR_PM)),
  Inconsistent_Discipline_SR = APQ_SR$APQ_SR_ID,
  Inconsistent_Discipline_SR_z = ((APQ_SR$APQ_SR_ID - mean(APQ_SR$APQ_SR_ID))/sd(APQ_SR$APQ_SR_ID)),
  Corporal_Punishment_SR = APQ_SR$APQ_SR_CP,
  Corporal_Punishment_SR_z = ((APQ_SR$APQ_SR_CP - mean(APQ_SR$APQ_SR_CP))/sd(APQ_SR$APQ_SR_CP)))

# calculate positive and negative parenting composites
#parent-report
APQ_P_of_interest$Positive_Parenting_Composite_P <- (APQ_P_of_interest$Involvement_P_z + APQ_P_of_interest$Positive_Parenting_P_z)/2
APQ_P_of_interest$Negative_Parenting_Composite_P <- (APQ_P_of_interest$Poor_Monitoring_P_z + APQ_P_of_interest$Inconsistent_Discipline_P_z +
                                                       APQ_P_of_interest$Corporal_Punishment_P_z)/3

#self-report
APQ_SR_of_interest$Positive_Parenting_Composite_SR <- (APQ_SR_of_interest$Involvement_Mother_SR_z + APQ_SR_of_interest$Involvement_Father_SR_z +
                                                         APQ_SR_of_interest$Positive_Parenting_SR_z)/2
APQ_SR_of_interest$Negative_Parenting_Composite_SR <- (APQ_SR_of_interest$Poor_Monitoring_SR_z + APQ_SR_of_interest$Inconsistent_Discipline_SR_z +
                                                         APQ_SR_of_interest$Corporal_Punishment_SR_z)/3

#dataframe for comparison of differentials
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Parent_Child_Comparisons <- Reduce(Merge, list(IAT_of_interest,
                                               PCIAT_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                               MFQ_SR_of_interest, MFQ_P_of_interest, APQ_P_of_interest, APQ_SR_of_interest))

#compute differential scores (parent minus child for all)
Parent_Child_Comparisons$IAT_Delta <- Parent_Child_Comparisons$IAT_Parent - Parent_Child_Comparisons$IAT_SR

Parent_Child_Comparisons$Total_Anxiety_Delta <- Parent_Child_Comparisons$Total_Anxiety_P - 
  Parent_Child_Comparisons$Total_Anxiety_SR

Parent_Child_Comparisons$Depression_Delta <- Parent_Child_Comparisons$Depression_P - 
  Parent_Child_Comparisons$Depression_SR

Parent_Child_Comparisons$Positive_Parenting_Composite_Delta <- Parent_Child_Comparisons$Positive_Parenting_Composite_P - 
  Parent_Child_Comparisons$Positive_Parenting_Composite_SR

Parent_Child_Comparisons$Negative_Parenting_Composite_Delta <- Parent_Child_Comparisons$Negative_Parenting_Composite_P - 
  Parent_Child_Comparisons$Negative_Parenting_Composite_SR

#Parent_Child_Comparisons$Positive_Parenting_Delta <- Parent_Child_Comparisons$Positive_Parenting_P - 
#  Parent_Child_Comparisons$Positive_Parenting_SR
#
#Parent_Child_Comparisons$Poor_Monitoring_Delta <- Parent_Child_Comparisons$Poor_Monitoring_P - 
#  Parent_Child_Comparisons$Poor_Monitoring_SR
#
#Parent_Child_Comparisons$Inconsistent_Discipline_Delta <- Parent_Child_Comparisons$Inconsistent_Discipline_P - 
#  Parent_Child_Comparisons$Inconsistent_Discipline_SR
#
#Parent_Child_Comparisons$Corporal_Punishment_Delta <- Parent_Child_Comparisons$Corporal_Punishment_P - 
#  Parent_Child_Comparisons$Corporal_Punishment_SR

#boxplot of differential scores
pdf(file="Parent_Child_Comparisons_Boxplot.pdf", width = 120, height = 60)
par(mar=c(70,40,12,2)+0.1,mgp=c(20,3,0))
#end_point = 0.5 + nrow(Parent_Child_Comparisons) + nrow(Parent_Child_Comparisons)-1 #this is the line which does the trick (together with barplot "space = 1" parameter)
boxplot(Parent_Child_Comparisons$IAT_Delta, 
        Parent_Child_Comparisons$Total_Anxiety_Delta, Parent_Child_Comparisons$Depression_Delta, 
        Parent_Child_Comparisons$Positive_Parenting_Composite_Delta, Parent_Child_Comparisons$Negative_Parenting_Composite_Delta,
        cex.main = 8, cex.lab = 8, cex.axis = 8, main = "Parent-Child Discrepancies by Measure",
        #xlab = "", boxlwd = 12, whisklwd = 12, staplelwd = 12,
        ylab = "Discrepancy Score (Parent - Child)", las = 2, space = 1, col = c("lightcyan1"), border = c("darkcyan"))
        #names = c("IAT", "Anxiety", "Depression", "Positive Parenting Composite", "Negative Parenting Composite"))
#text(seq(1,end_point,by=1), par("usr")[3]-0.25, 
#     srt = 60, adj= 1, xpd = TRUE, cex=8,
#     labels = paste(c("IAT", "Anxiety", "Depression", "Positive Parenting Composite", "Negative Parenting Composite"))
abline(h=0)
zz <- dev.off() # send to garbage variable to avoid null device problem

####################### Q2. Diagnoses and IAT #################
########### Odds Ratios (general)

#create dataframe "Dx_of_interest"
# making new dataframe by coding each diagnosis as either 0 (no) or 1 (yes) ------>>> if get error message, it is because URSI is screwed up
Dx_of_interest <- data.frame(
  ID = ConsensusDx$URSI,
  ASD = ifelse ((ConsensusDx$DX_01_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_02_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_03_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_04_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_05_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_06_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_07_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_08_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_09_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_10_Sub == "Autism Spectrum Disorder"), 1, 0),
  Learning_Disorder = ifelse (ConsensusDx$DX_01_Sub == "Specific Learning Disorder" | ConsensusDx$DX_02_Sub == "Specific Learning Disorder" | ConsensusDx$DX_03_Sub == "Specific Learning Disorder" | ConsensusDx$DX_04_Sub == "Specific Learning Disorder" | ConsensusDx$DX_05_Sub == "Specific Learning Disorder" | ConsensusDx$DX_06_Sub == "Specific Learning Disorder" | ConsensusDx$DX_07_Sub == "Specific Learning Disorder" | ConsensusDx$DX_08_Sub == "Specific Learning Disorder" | ConsensusDx$DX_09_Sub == "Specific Learning Disorder" | ConsensusDx$DX_10_Sub == "Specific Learning Disorder", 1, 0),
  Anxiety = ifelse (ConsensusDx$DX_01_Cat == "Anxiety Disorders" | ConsensusDx$DX_02_Cat == "Anxiety Disorders" | ConsensusDx$DX_03_Cat == "Anxiety Disorders" | ConsensusDx$DX_04_Cat == "Anxiety Disorders" | ConsensusDx$DX_05_Cat == "Anxiety Disorders" | ConsensusDx$DX_06_Cat == "Anxiety Disorders" | ConsensusDx$DX_07_Cat == "Anxiety Disorders" | ConsensusDx$DX_08_Cat == "Anxiety Disorders" | ConsensusDx$DX_09_Cat == "Anxiety Disorders" | ConsensusDx$DX_10_Cat == "Anxiety Disorders", 1, 0),
  Depression = ifelse (ConsensusDx$DX_01_Cat == "Depressive Disorders" | ConsensusDx$DX_02_Cat == "Depressive Disorders" | ConsensusDx$DX_03_Cat == "Depressive Disorders" | ConsensusDx$DX_04_Cat == "Depressive Disorders" | ConsensusDx$DX_05_Cat == "Depressive Disorders" | ConsensusDx$DX_06_Cat == "Depressive Disorders" | ConsensusDx$DX_07_Cat == "Depressive Disorders" | ConsensusDx$DX_08_Cat == "Depressive Disorders" | ConsensusDx$DX_09_Cat == "Depressive Disorders" | ConsensusDx$DX_10_Cat == "Depressive Disorders", 1, 0),
  ADHD_Combined = ifelse (ConsensusDx$DX_01 == "ADHD-Combined Type" | ConsensusDx$DX_02 == "ADHD-Combined Type" | ConsensusDx$DX_03 == "ADHD-Combined Type" | ConsensusDx$DX_04 == "ADHD-Combined Type" | ConsensusDx$DX_05 == "ADHD-Combined Type" | ConsensusDx$DX_06 == "ADHD-Combined Type" | ConsensusDx$DX_07 == "ADHD-Combined Type" | ConsensusDx$DX_08 == "ADHD-Combined Type" | ConsensusDx$DX_09 == "ADHD-Combined Type" | ConsensusDx$DX_10 == "ADHD-Combined Type", 1, 0),
  ADHD_Inattentive = ifelse (ConsensusDx$DX_01 == "ADHD-Inattentive Type" | ConsensusDx$DX_02 == "ADHD-Inattentive Type" | ConsensusDx$DX_03 == "ADHD-Inattentive Type" | ConsensusDx$DX_04 == "ADHD-Inattentive Type" | ConsensusDx$DX_05 == "ADHD-Inattentive Type" | ConsensusDx$DX_06 == "ADHD-Inattentive Type" | ConsensusDx$DX_07 == "ADHD-Inattentive Type" | ConsensusDx$DX_08 == "ADHD-Inattentive Type" | ConsensusDx$DX_09 == "ADHD-Inattentive Type" | ConsensusDx$DX_10 == "ADHD-Inattentive Type", 1, 0),
  ADHD_Hyperactive = ifelse (ConsensusDx$DX_01 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_02 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_03 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_04 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_05 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_06 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_07 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_08 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_09 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_10 == "ADHD-Hyperactive/Impulsive Type", 1, 0),
  Social_Anxiety = ifelse (ConsensusDx$DX_01 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_02 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_03 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_04 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_05 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_06 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_07 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_08 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_09 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_10 == "Social Anxiety (Social Phobia)", 1, 0)
)
# save file and turn all na into 0
write.csv(Dx_of_interest, "Dx_of_interest.csv", row.names = FALSE, na="0")
#read in new Dx_of_interest
Dx_of_interest <- read.csv("Dx_of_interest.csv")

#create problematic (1) vs. non-problematic (0) based on score of 40 (Kim et al.)
Categorical_IAT <- data.frame(ID = IAT$URSI, Problematic = ifelse (IAT$IAT_Total >=40, 1, 0))
Categorical_PCIAT <- data.frame(ID = PCIAT$URSI, Problematic = ifelse (PCIAT$PCIAT_Total >=40, 1, 0))

#dataframe with dxs and IAT/PCIAT
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
IAT_Dx <- Reduce(Merge, list(Categorical_IAT, Dx_of_interest))
PCIAT_Dx <- Reduce(Merge, list(Categorical_PCIAT, Dx_of_interest))


#calculate odds ratios for each Dx for IAT
#ASD
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#Learning Disorder
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#anxiety
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#depression
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#ADHD-C
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#ADHD-I
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#ADHD-H
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#Social Anxiety
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#calculate odds ratios for each Dx for PCIAT
#ASD
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#Learning Disorder
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#anxiety
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#depression
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#ADHD-C
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#ADHD-I
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#ADHD-H
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#Social Anxiety
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

############ Odds Ratios (by site)
#create site info dataframe
Site_Info <- data.frame(
  ID = Basic_Demos$URSI,
  Site = Basic_Demos$Study_Site)

# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# merge dataframes with DX of interest for PCIAT and IAT respectively
IAT_Dx <- Reduce(Merge, list(Categorical_IAT, Dx_of_interest, Site_Info))
PCIAT_Dx <- Reduce(Merge, list(Categorical_PCIAT, Dx_of_interest, Site_Info))

# split dataframes into Midtown and Staten Island
x <- split.data.frame(IAT_Dx, IAT_Dx$Site)
str(x)

names(x) <- c("IAT_Dx_SI", "IAT_Dx_MRV", "IAT_Dx_MT")
list2env(x, envir = .GlobalEnv)

y <- split.data.frame(PCIAT_Dx, PCIAT_Dx$Site)
names(y) <- c("PCIAT_Dx_SI", "PCIAT_Dx_MRV", "PCIAT_Dx_MT")
list2env(y, envir = .GlobalEnv)

#calculate odds ratios for each Dx for IAT for different sites
#ASD
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Learning Disorder
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#anxiety
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#depression
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)
#ADHD-C
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-I
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-H -- too few (don't use)
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Social Anxiety # don't use midtown - too few
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#calculate odds ratios for each Dx for PCIAT for diff. sites

#ASD
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Learning Disorder
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#anxiety
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#depression
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-C
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-I
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-H
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Social Anxiety
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)




############################ Q3. Substance Use ##############
###### Correlation Matrix
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

# pull out variables of interest
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

#merge dataframes for nicotine and YFAs
Nicotine_of_interest = rbind(FTQA_of_interest, FTND_of_interest)

YFAS_of_interest = rbind(YFAS_A_of_interest, YFAS_C_of_interest)

#dataframe for correlation
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Corr_substance <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, AUDIT_of_interest, ESPAD_of_interest, 
                                     Nicotine_of_interest, YFAS_of_interest, Demos_of_interest, 
                                     Barratt_of_interest))

#Correlation (substance) in table form
library(apaTables)
apa.cor.table(Corr_substance, filename="Corr_substance_APA.doc", table.number=1)

#Correlation (substance) heat map
library(psych)
correlationvars2 <- Corr_substance[,-1]
rvalues2 <- cor(correlationvars2, use="pairwise.complete.obs")
#get P and n values for correlations
significance2 <- corr.test(correlationvars2, adjust="fdr")
pvalues2 <- significance2$p
nvalues2 <- array(significance2$n)
# Set plot parameters, color, and labels
par(mfrow=c(1,1))
col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                           "cyan", "#007FFF","#00007F")) 
#colnames(rvalues2) <- c("Sex", "Barratt", "CBCL: Total", "CBCL: Int", "CBCL: Ext","SDQ: Total","SDQ: Int","SDQ: Ext",
#                        "SDQ: Peer Prob", "SDQ: Prosocial", "MFQ(P)","MFQ(SR)","SCARED(P)",
#                       "SCARED(SR)","ARI(P)","ARI(SR)","SWAN: Total","SWAN: Inatt", "SWAN: Hyper",
#                        "ICU(P)","ASSQ","SCQ","SRS", "FSIQ","PIQ","VIQ","CELF",
#                        "BMI","Food Addiction: Symp Count","Food Addiction: DX",
#                        "Internet Addiction")
#rownames(rvalues2) <- c("Sex", "Barratt", "CBCL: Total", "CBCL: Int", "CBCL: Ext","SDQ: Total","SDQ: Int","SDQ: Ext",
#                        "SDQ: Peer Prob", "SDQ: Prosocial", "MFQ(P)","MFQ(SR)","SCARED(P)",
#                        "SCARED(SR)","ARI(P)","ARI(SR)","SWAN: Total","SWAN: Inatt", "SWAN: Hyper",
#                        "ICU(P)","ASSQ","SCQ","SRS", "FSIQ","PIQ","VIQ","CELF",
#                        "BMI","Food Addiction: Symp Count","Food Addiction: DX",
#                        "Internet Addiction")
#PLOT CORRELATION MATRIX
library(corrplot)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
#save correlation matrix
pdf(file="Corr_substance.pdf", width = 100, height = 100)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper", tl.cex = 10, cl.cex = 10)
dev.off()

########################## Q4. Regressions ###############
########## Dx Regression 
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

#dataframe for regression -- dxs
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Logistic_Regression <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                          PCIAT_of_interest, Demos_of_interest, Barratt_of_interest))
#regression for IAT (Dx)
library(apaTables)
Reg_Dx_SR<-lm(IAT_SR ~ ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
                Social_Anxiety + Sex + Age + SES + Site, data = Logistic_Regression)
apa.reg.table(Reg_Dx_SR, filename = "Reg_Dx_SR.doc", table.number = 2)
summary(Reg_Dx_SR)
library(broom)
write.csv(tidy(Reg_Dx_SR), "Reg_Dx_SR_tidy.csv", row.names = FALSE)
write.csv(glance(Reg_Dx_SR), "Reg_Dx_SR_glance.csv", row.names = FALSE)

#regression for PCIAT (Dx)
library(apaTables)
Reg_Dx_P<-lm(IAT_Parent ~ ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
               Social_Anxiety + Sex + Age + SES + Site, data = Logistic_Regression)
apa.reg.table(Reg_Dx_P, filename = "Reg_Dx_P.doc", table.number = 3)
summary(Reg_Dx_P)

summary(lm(IAT_Parent ~ ASD + Learning_Disorder + Anxiety
              + Sex + Age + SES + Site, data = Logistic_Regression))
summary(lm(IAT_SR ~ ASD + Learning_Disorder + Anxiety
           + Sex + Age + SES + Site, data = Logistic_Regression))

library(broom)
write.csv(tidy(Reg_Dx_P), "Reg_Dx_P_tidy.csv", row.names = FALSE)
write.csv(glance(Reg_Dx_P), "Reg_Dx_P_glance.csv", row.names = FALSE)

########### Dimensional Q's Regression
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
linear_regression <- Reduce(Merge, list(IAT_of_interest,
                                       PCIAT_of_interest, ASSQ_of_interest, 
                                       Conners_of_interest, SWAN_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                       MFQ_SR_of_interest, MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest))

#regression for IAT (Q's)
library(apaTables)
Reg_Qs_SR<-lm(IAT_SR ~ Conners_Hyperactivity_Impulsivity + Conners_Inattention + Conners_Learning_Problems + 
                Social_Anxiety_SR + Total_Anxiety_SR +
                Depression_SR + Sex + Age + SES + Site, data = linear_regression)
apa.reg.table(Reg_Qs_SR, filename = "Reg_Qs_SR.doc", table.number = 4)
summary(Reg_Qs_SR)
library(broom)
write.csv(tidy(Reg_Qs_SR), "Reg_Qs_SR_tidy.csv", row.names = FALSE)
write.csv(glance(Reg_Qs_SR), "Reg_Qs_SR_glance.csv", row.names = FALSE)

#regression for PCIAT (Q's)
library(apaTables)
Reg_Qs_P<-lm(IAT_Parent ~ ASSQ_Total +
               SWAN_Inattentive + SWAN_Hyperactive + Social_Anxiety_P + Total_Anxiety_P +
               Depression_P + Sex + Age + SES + Site, data = linear_regression)

summary(lm(IAT_Parent ~ ASSQ_Total + Total_Anxiety_P + Sex + Age + SES + Site, data = linear_regression))
summary(lm(Logistic_Regression$IAT_Parent ~ Logistic_Regression$ADHD_Combined + linear_regression$SWAN_Inattentive + Logistic_Regression$Sex + Logistic_Regression$Age + Logistic_Regression$Site + Logistic_Regression$SES))
summary(lm(IAT_SR ~ Total_Anxiety_SR +Sex + Age + SES + Site, data = linear_regression))


apa.reg.table(Reg_Qs_P, filename = "Reg_Qs_P.doc", table.number = 5)
summary(Reg_Qs_P)
library(broom)
write.csv(tidy(Reg_Qs_P), "Reg_Qs_P_tidy.csv", row.names = FALSE)
write.csv(glance(Reg_Qs_P), "Reg_Qs_P_glance.csv", row.names = FALSE)

############## Coping Regressions
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

CCSC_of_interest <- data.frame(
  ID = CCSC$URSI,
  Problem_Focused_Coping = CCSC$CCSC_PFC,
  Avoidance_Coping = CCSC$CCSC_AC,
  Positive_Cognitive_Restructuring = CCSC$CCSC_PCR,
  Religion = CCSC$CCSC_REL,
  Support_Seeking = CCSC$CCSC_SS)

#dataframe for regression with coping and IAT/PCIAT
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Reg_CCSC <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CCSC_of_interest))

# Regression with overarching subscales of CCSC predicting IAT_SR
library(apaTables)
Reg_CCSC_SR<-lm(IAT_SR ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring + Religion + Support_Seeking +
          Sex + Age + SES + Site, data = Reg_CCSC)
apa.reg.table(Reg_CCSC_SR, filename = "Reg_CCSC_SR.doc", table.number = 6)
summary(Reg_CCSC_SR)
library(broom)
write.csv(tidy(Reg_CCSC_SR), "Reg_CCSC_SR_tidy.csv", row.names = FALSE)
write.csv(glance(Reg_CCSC_SR), "Reg_CCSC_SR_glance.csv", row.names = FALSE)

# Regression with overarching subscales of CCSC predicting PCIAT
library(apaTables)
Reg_CCSC_P<-lm(IAT_Parent ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring + Religion + Support_Seeking +
          Sex + Age + SES + Site, data = Reg_CCSC)
apa.reg.table(Reg_CCSC_P, filename = "Reg_CCSC_P.doc", table.number = 7)
summary(Reg_CCSC_P)
library(broom)
write.csv(tidy(Reg_CCSC_P), "Reg_CCSC_P_tidy.csv", row.names = FALSE)
write.csv(glance(Reg_CCSC_P), "Reg_CCSC_P_glance.csv", row.names = FALSE)

########### Parenting Regressions
# pull out data of interest
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

APQ_P_of_interest <- data.frame(
  ID = APQ_P$URSI,
  Involvement_P = APQ_P$APQ_P_INV,
  Positive_Parenting_P = APQ_P$APQ_P_PP,
  Poor_Monitoring_P = APQ_P$APQ_P_PM,
  Inconsistent_Discipline_P = APQ_P$APQ_P_ID,
  Corporal_Punishment_P = APQ_P$APQ_P_CP)

APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$URSI,
  Involvement_Mother_SR = APQ_SR$APQ_SR_INV_M,
  Involvement_Father_SR = APQ_SR$APQ_SR_INV_D,
  Positive_Parenting_SR = APQ_SR$APQ_SR_PP,
  Poor_Monitoring_SR = APQ_SR$APQ_SR_PM,
  Inconsistent_Discipline_SR = APQ_SR$APQ_SR_ID,
  Corporal_Punishment_SR = APQ_SR$APQ_SR_CP)

# create z-scores for each APQ subscale and average out relevant subscales to form positive and negative parenting composites
APQ_P_of_interest <- data.frame(
  ID = APQ_P$URSI,
  Involvement_P = APQ_P$APQ_P_INV,
  Involvement_P_z = ((APQ_P$APQ_P_INV - mean(APQ_P$APQ_P_INV))/sd(APQ_P$APQ_P_INV)),
  Positive_Parenting_P = APQ_P$APQ_P_PP,
  Positive_Parenting_P_z = ((APQ_P$APQ_P_PP - mean(APQ_P$APQ_P_PP))/sd(APQ_P$APQ_P_PP)),
  Poor_Monitoring_P = APQ_P$APQ_P_PM,
  Poor_Monitoring_P_z = ((APQ_P$APQ_P_PM - mean(APQ_P$APQ_P_PM))/sd(APQ_P$APQ_P_PM)),
  Inconsistent_Discipline_P = APQ_P$APQ_P_ID,
  Inconsistent_Discipline_P_z = ((APQ_P$APQ_P_ID - mean(APQ_P$APQ_P_ID))/sd(APQ_P$APQ_P_ID)),
  Corporal_Punishment_P = APQ_P$APQ_P_CP,
  Corporal_Punishment_P_z = ((APQ_P$APQ_P_CP - mean(APQ_P$APQ_P_CP))/sd(APQ_P$APQ_P_CP)))

APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$URSI,
  Involvement_Mother_SR = APQ_SR$APQ_SR_INV_M,
  Involvement_Mother_SR_z = ((APQ_SR$APQ_SR_INV_M - mean(APQ_SR$APQ_SR_INV_M))/sd(APQ_SR$APQ_SR_INV_M)),
  Involvement_Father_SR = APQ_SR$APQ_SR_INV_D,
  Involvement_Father_SR_z = ((APQ_SR$APQ_SR_INV_D - mean(APQ_SR$APQ_SR_INV_D))/sd(APQ_SR$APQ_SR_INV_D)),
  Positive_Parenting_SR = APQ_SR$APQ_SR_PP,
  Positive_Parenting_SR_z = ((APQ_SR$APQ_SR_PP - mean(APQ_SR$APQ_SR_PP))/sd(APQ_SR$APQ_SR_PP)),
  Poor_Monitoring_SR = APQ_SR$APQ_SR_PM,
  Poor_Monitoring_SR_z = ((APQ_SR$APQ_SR_PM - mean(APQ_SR$APQ_SR_PM))/sd(APQ_SR$APQ_SR_PM)),
  Inconsistent_Discipline_SR = APQ_SR$APQ_SR_ID,
  Inconsistent_Discipline_SR_z = ((APQ_SR$APQ_SR_ID - mean(APQ_SR$APQ_SR_ID))/sd(APQ_SR$APQ_SR_ID)),
  Corporal_Punishment_SR = APQ_SR$APQ_SR_CP,
  Corporal_Punishment_SR_z = ((APQ_SR$APQ_SR_CP - mean(APQ_SR$APQ_SR_CP))/sd(APQ_SR$APQ_SR_CP)))

# calculate positive and negative parenting composites
#parent-report
APQ_P_of_interest$Positive_Parenting_Composite_P <- (APQ_P_of_interest$Involvement_P_z + APQ_P_of_interest$Positive_Parenting_P_z)/2
APQ_P_of_interest$Negative_Parenting_Composite_P <- (APQ_P_of_interest$Poor_Monitoring_P_z + APQ_P_of_interest$Inconsistent_Discipline_P_z +
                                                       APQ_P_of_interest$Corporal_Punishment_P_z)/3

#self-report
APQ_SR_of_interest$Positive_Parenting_Composite_SR <- (APQ_SR_of_interest$Involvement_Mother_SR_z + APQ_SR_of_interest$Involvement_Father_SR_z +
                                                         APQ_SR_of_interest$Positive_Parenting_SR_z)/2
APQ_SR_of_interest$Negative_Parenting_Composite_SR <- (APQ_SR_of_interest$Poor_Monitoring_SR_z + APQ_SR_of_interest$Inconsistent_Discipline_SR_z +
                                                         APQ_SR_of_interest$Corporal_Punishment_SR_z)/3
#dataframe for parenting regressions
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Reg_APQ <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, APQ_P_of_interest, APQ_SR_of_interest))

# Regression with subscales of APQ_P predicting PCIAT
library(apaTables)
Reg_APQ_subscales_P<-lm(IAT_Parent ~ Involvement_P + Positive_Parenting_P + Poor_Monitoring_P + Inconsistent_Discipline_P +
          Corporal_Punishment_P + Sex + Age + SES + Site, data = Reg_APQ)
apa.reg.table(Reg_APQ_subscales_P, filename = "Reg_APQ_subscales_P.doc", table.number = 8)
summary(Reg_APQ_subscales_P)

# Regression with P positive and negative composites predicting PCIAT
Reg_APQ_composites_P<-lm(IAT_Parent ~ Positive_Parenting_Composite_P +
          Negative_Parenting_Composite_P + Sex + Age + SES + Site, data = Reg_APQ)
apa.reg.table(Reg_APQ_composites_P, filename = "Reg_APQ_composites_P.doc", table.number = 9)
summary(Reg_APQ_composites_P)

# Regression with subscales of APQ_SR predicting IAT
Reg_APQ_subscales_SR<-lm(IAT_SR ~ Involvement_Mother_SR + Involvement_Father_SR + Positive_Parenting_SR + 
                           Poor_Monitoring_SR + Inconsistent_Discipline_SR +
          Corporal_Punishment_SR + Sex + Age + SES + Site, data = Reg_APQ)
apa.reg.table(Reg_APQ_subscales_SR, filename = "Reg_APQ_subscales_SR.doc", table.number = 10)
summary(Reg_APQ_subscales_SR)

# Regression with SR positive and negative composites predicting IAT
Reg_APQ_composites_SR<-lm(IAT_SR ~ Positive_Parenting_Composite_SR +
          Negative_Parenting_Composite_SR + Sex + Age + SES + Site, data = Reg_APQ)
apa.reg.table(Reg_APQ_composites_SR, filename = "Reg_APQ_composites_SR.doc", table.number = 11)
summary(Reg_APQ_composites_SR)
