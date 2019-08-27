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
IAT_CIS <- Reduce(Merge, list(IAT_of_interest, CIS_SR_of_interest))
PCIAT_CIS <- Reduce(Merge, list(PCIAT_of_interest, CIS_P_of_interest))

#remove nans
IAT_CIS <- IAT_CIS[complete.cases(IAT_CIS), ]
PCIAT_CIS <- PCIAT_CIS[complete.cases(PCIAT_CIS), ]

#reorder dataframes by IAT scores (ascending)
IAT_CIS <- IAT_CIS[order(IAT_CIS$IAT_SR), ]
PCIAT_CIS <- PCIAT_CIS[order(PCIAT_CIS$IAT_Parent), ]

#create new Quartile variable assigning each participant to each quartile of the IAT
IAT_CIS$Quartile <- ifelse(IAT_CIS$IAT_SR < 14, 1, ifelse(IAT_CIS$IAT_SR >= 14 & IAT_CIS$IAT_SR < 26, 2, 
                                                          ifelse(IAT_CIS$IAT_SR >= 26 & IAT_CIS$IAT_SR < 39, 3, 4)))
PCIAT_CIS$Quartile <- ifelse(PCIAT_CIS$IAT_Parent < 10, 1, ifelse(PCIAT_CIS$IAT_Parent >= 10 & PCIAT_CIS$IAT_Parent < 26, 2, 
                                                            ifelse(PCIAT_CIS$IAT_Parent >= 26 & PCIAT_CIS$IAT_Parent < 39, 3, 4)))

#Split dataframe by IAT Quartile
x <- split.data.frame(IAT_CIS, IAT_CIS$Quartile)
str(x)
names(x) <- c("SRQ1", "SRQ2", "SRQ3", "SRQ4")
list2env(x, envir = .GlobalEnv)

y <- split.data.frame(PCIAT_CIS, PCIAT_CIS$Quartile)
str(y)
names(y) <- c("PQ1", "PQ2", "PQ3", "PQ4")
list2env(y, envir = .GlobalEnv)


#create boxplots with IAT and CIS broken into quartiles
#SR
pdf(file="SR_IAT_v_CIS.pdf", width = 120, height = 60)
par(mar=c(70,40,12,2)+0.1,mgp=c(30,10,0))
boxplot(SRQ1$CIS_SR_Score, SRQ2$CIS_SR_Score, SRQ3$CIS_SR_Score, SRQ4$CIS_SR_Score, 
        cex.main = 8, cex.lab = 8, cex.axis = 8, main = "Self-Report: CIS vs. IAT by Quartiles",
        xlab = "Self-Report IAT Score", boxlwd = 12, whisklwd = 12, staplelwd = 12, las = 1, names = c("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"),
        ylab = "Self-Report CIS Score", las = 2, space = 1, col = c("lightcyan1"), border = c("darkcyan"))
dev.off()
#P
pdf(file="P_IAT_v_CIS.pdf", width = 120, height = 60)
par(mar=c(70,40,12,2)+0.1,mgp=c(30,10,0))
boxplot(PQ1$CIS_P_Score, PQ2$CIS_P_Score, PQ3$CIS_P_Score, PQ4$CIS_P_Score, 
        cex.main = 8, cex.lab = 8, cex.axis = 8, main = "Parent-Report: CIS vs. IAT by Quartiles",
        xlab = "Parent-Report IAT Score", boxlwd = 12, whisklwd = 12, staplelwd = 12, las = 1, names = c("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"),
        ylab = "Parent-Report CIS Score", las = 2, space = 1, col = c("lightcyan1"), border = c("darkcyan"))
dev.off()

#create new Quintile variable assigning each participant to each quintile of the IAT
IAT_CIS$Quintile <- ifelse(IAT_CIS$IAT_SR < 10, 1, ifelse(IAT_CIS$IAT_SR >= 10 & IAT_CIS$IAT_SR < 21, 2, 
                                                          ifelse(IAT_CIS$IAT_SR >= 21 & IAT_CIS$IAT_SR < 30, 3, 
                                                                 ifelse(IAT_CIS$IAT_SR >= 30 & IAT_CIS$IAT_SR < 42, 4, 5))))
PCIAT_CIS$Quintile <- ifelse(PCIAT_CIS$IAT_Parent < 5, 1, ifelse(PCIAT_CIS$IAT_Parent >= 5 & PCIAT_CIS$IAT_Parent < 20, 2, 
                                                                  ifelse(PCIAT_CIS$IAT_Parent >= 20 & PCIAT_CIS$IAT_Parent < 31, 3, 
                                                                         ifelse(PCIAT_CIS$IAT_Parent >= 31 & PCIAT_CIS$IAT_Parent < 43, 4, 5))))

#Split dataframe by IAT Quintile
x <- split.data.frame(IAT_CIS, IAT_CIS$Quintile)
str(x)
names(x) <- c("SRQ1", "SRQ2", "SRQ3", "SRQ4", "SRQ5")
list2env(x, envir = .GlobalEnv)

y <- split.data.frame(PCIAT_CIS, PCIAT_CIS$Quintile)
str(y)
names(y) <- c("PQ1", "PQ2", "PQ3", "PQ4", "PQ5")
list2env(y, envir = .GlobalEnv)

#create boxplots with IAT broken into quintiles and CIS broken into quartiles
#SR
pdf(file="SR_IAT_v_CIS_quintile.pdf", width = 120, height = 60)
par(mar=c(70,40,12,2)+0.1,mgp=c(30,10,0))
boxplot(SRQ1$CIS_SR_Score, SRQ2$CIS_SR_Score, SRQ3$CIS_SR_Score, SRQ4$CIS_SR_Score, SRQ5$CIS_SR_Score,
        cex.main = 8, cex.lab = 8, cex.axis = 8, main = "Self-Report: CIS vs. IAT by Quintiles",
        xlab = "Self-Report IAT Score", boxlwd = 12, whisklwd = 12, staplelwd = 12, las = 1, names = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5"),
        ylab = "Self-Report CIS Score", las = 2, space = 1, col = c("lightcyan1"), border = c("darkcyan"))
dev.off()
#P
pdf(file="P_IAT_v_CIS_quintile.pdf", width = 120, height = 60)
par(mar=c(70,40,12,2)+0.1,mgp=c(30,10,0))
boxplot(PQ1$CIS_P_Score, PQ2$CIS_P_Score, PQ3$CIS_P_Score, PQ4$CIS_P_Score, PQ5$CIS_P_Score, 
        cex.main = 8, cex.lab = 8, cex.axis = 8, main = "Parent-Report: CIS vs. IAT by Quintiles",
        xlab = "Parent-Report IAT Score", boxlwd = 12, whisklwd = 12, staplelwd = 12, las = 1, names = c("Quintile 1", "Quintile 2", "Quintile 3", "Quintile 4", "Quintile 5"),
        ylab = "Parent-Report CIS Score", las = 2, space = 1, col = c("lightcyan1"), border = c("darkcyan"))
dev.off()
