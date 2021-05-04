#########################################################
#########################################################
# R Code to Manuscript
#
# Time spent on school-related activities at home during the pandemic:
# A longitudinal analysis of social group inequality 
# among secondary school students
#
# Data preparation for 2019  
#
# SZ, 23.11.2020
#########################################################
#########################################################

rm(list=ls())
library(haven)
library(mice)
library(miceadds)
library(questionr)
library(Hmisc)
library(plyr)
library(survey)
library(reshape2)
library(ggplot2)
library(questionr)

#################################################################
# Data preparation
#################################################################
# ---------------------------------------------------------------
# Read Data
# ---------------------------------------------------------------
COV <- read_dta("SOEP_CoV.dta") # derived from plong in SOEP data v36 (simply write plong here, will also work)
COV2019 <- COV[COV$syear == 2019,]

# ---------------------------------------------------------------
# FOR KIDS BORN in 2007 and 2005: age, schooltype, grades, time usage
# ---------------------------------------------------------------
School1 <- read_dta("bjschool.dta") # SOEP pupil questionnaire
School1 <- School1[!(School1$sample1 %in% c(30,31,34)),] # without refugees
School1 <- as.data.frame(School1[, c("hid", "pid", "bjsbirthy", "bjs_06_01", "bjs_06_02", "bjs_14")])
colnames(School1) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime") 
School2 <- read_dta("bjschool2.dta")
School2 <- School2[!(School2$sample1 %in% c(30,31,34)),] # without refugees
School2 <- as.data.frame(School2[, c("hid", "pid", "bjfjbirthy", "bjfj_06_01", "bjfj_06_02", "bjfj_12")])
colnames(School2) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime")
School <- rbind(School1, School2)
School$ageKid <- 2019 - School$birthy
table(School$ageKid , exclude=NULL) # only age 11-12, 13-14, 16-17 (miss age 10, 15)
SchoolR <- School[order(School$hid, School$ageKid),]
#SchoolR <- SchoolR[!duplicated(SchoolR$hid),] # only youngest child
SchoolR <- SchoolR[SchoolR$ageKid < 17, ] # info for 17 years old stem from youth questionnaire, schooltype info not in questionnaire of household, prepare data for 17 years old separately
# take schooltype info from household questionnaire
bjh_kind <- as.data.frame(read_dta("bjh_kind.dta"))
table(SchoolR$hid %in% bjh_kind$hid)
SchoolR1 <- SchoolR[SchoolR$hid %in% bjh_kind$hid,] # N=961 (miss N=302)
birthYN <- c("bjk_87_03", "bjk_87_06", "bjk_87_09", "bjk_87_12", "bjk_87_15")
schoolN <- c("bjk_88_02", "bjk_88_04", "bjk_88_06", "bjk_88_08", "bjk_88_10")
bjh_kind <- bjh_kind[bjh_kind$hid %in% SchoolR1$hid, c("hid",birthYN, schoolN)]
namN1 <- paste("birthY", 1:5, sep="_")
namN2 <- paste("school", 1:5, sep="_")
colnames(bjh_kind) <- c("hid", namN1, namN2)
bjh_kind[bjh_kind<0] <- NA
bjh_kind_long <- reshape(bjh_kind, direction="long", varying  = list(namN1, namN2), idvar = "hid")
bjh_kind_long <- bjh_kind_long[order(bjh_kind_long$hid),]
colnames(bjh_kind_long) <- c("hid", "numKid", "birthY", "schoolType") # 1 Grundschule, 2 Hauptschule, 3 Realschule, 4 Gymnasium, 5 Gesamtschule, 6 Berufsschule, 7 sonstige Schule
bjh_kind_long <- bjh_kind_long[,-2]
table(bjh_kind_long$birthY, bjh_kind_long$schoolType)
bjh_kind_long <- bjh_kind_long[order(bjh_kind_long$hid),]
table(SchoolR$hid %in% bjh_kind_long$hid) # only 5 miss, N=1070
table(bjh_kind_long$birthY, bjh_kind_long$schoolType, exclude=NULL)

findSchool <- function(i){
  res <- NA
  school_hid <- bjh_kind_long[bjh_kind_long$hid %in% SchoolR$hid[i],]
  sT <- school_hid[school_hid$birthY %in% SchoolR$birthy[i],]
  if(nrow(sT)>0)
    res <- sT[1,]["schoolType"]
  return(res)
}
rr <- sapply(1:nrow(SchoolR), findSchool)
rr <- as.numeric(unlist(rr))
head(rr)
SchoolR$schoolType <- rr
table(SchoolR$schoolType, exclude=NULL)
table(SchoolR$ageKid, SchoolR$schoolType, exclude=NULL)

# ---------------------------------------------------------------
# FOR KIDS BORN in 2007 and 2005: age, schooltype, grades, time usage
# ---------------------------------------------------------------
bjjugend <- as.data.frame(read_dta("bjjugend.dta")) # SOEP youth questionnaire
bjjugend <- bjjugend[!(bjjugend$sample1 %in% c(30,31,34)),] 
School3 <- as.data.frame(bjjugend[, c("hid", "pid", "bjjbirthy", "bjj_33_01", "bjj_33_02", "bjj_36", "bjj_20")])
colnames(School3) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime", "schoolType") 
School3[School3$schoolType < 0, "schoolType"] <- NA
School3$schoolType <- School3$schoolType + 1 
School3$ageKid <- 2019 - School3$birthy
School3 <- School3[, c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime", "ageKid", "schoolType")]
SchoolR <- rbind(SchoolR, School3)
table(SchoolR$ageKid, SchoolR$schoolType, exclude=NULL)

# ---------------------------------------------------------------
# Highest educational attainment in family
# ---------------------------------------------------------------
casM <- as.data.frame(COV[COV$syear %in% 2011:2018, c("pid", "hid", "syear", "gebjahr", "sex", "pgcasmin")])
casM <- casM[order(casM$pid),]
casM <- casM[setdiff(1:nrow(casM), which(is.na(casM$pgcasmin), arr.ind=TRUE)),]
casM <- casM[setdiff(1:nrow(casM), which(casM$pgcasmin<0)),]
casM <- casM[order(casM$pid, casM$syear, decreasing = TRUE),]
casM <- casM[!duplicated(casM$pid),]
casM <- casM[casM$gebjahr < 2000,]
casM <- casM[order(casM$hid, casM$pgcasmin, decreasing = TRUE),] 
casM <- casM[!duplicated(casM$hid),] 
table(SchoolR$hid %in% casM$hid) # N=16 miss
SchoolR <- merge(SchoolR, casM, by="hid", all.x=TRUE)
table(SchoolR$pgcasmin, exclude=NULL)
SchoolR$bild <- ifelse(SchoolR$pgcasmin %in% c(0, 1, 2, 4), "niedrig", ifelse(SchoolR$pgcasmin %in% c(3,5,6,7), "mittel", ifelse(SchoolR$pgcasmin %in% c(8,9), "hoch", NA))) 
table(SchoolR$bild, exclude=NULL)
SchoolR <- SchoolR[,-c(10:13)]
colnames(SchoolR)[colnames(SchoolR) %in% "pid.x"] <- "pid"

# ---------------------------------------------------------------
# Survey weights of person who reported (here: child) and child gender
# ---------------------------------------------------------------
ppL <- as.data.frame(read_dta("ppathl.dta"))
ppL2019 <- ppL[ppL$syear %in% 2019, c("hid", "pid", "phrf", "sex")]
table(SchoolR$hid %in% ppL2019$hid) # no miss, N=1480
table(SchoolR$pid %in% ppL2019$pid) # no miss, N=1480
SchoolR <- merge(SchoolR, ppL2019[,-1], by="pid", all.x = TRUE)
SchoolR[SchoolR<0] <- NA
table(SchoolR$sex) # 1: male, 2: fem
colnames(SchoolR)[colnames(SchoolR) %in% "sex"] <- "sexChild"

# ---------------------------------------------------------------
# Survey weights of household 
# ---------------------------------------------------------------
hpL <- as.data.frame(read_dta("hpathl.dta"))
hpL2019 <- hpL[hpL$syear %in% 2019, c("hid", "hhrf")]
table(SchoolR$hid %in% hpL2019$hid) # no miss, N=1480
SchoolR <- merge(SchoolR, hpL2019, by="hid", all.x = TRUE)

# ---------------------------------------------------------------
# Employment situation in household
# ---------------------------------------------------------------
pidSexGeb <- COV[COV$syear %in% 1984:2018,c("pid", "syear", "gebjahr", "sex")]
pidSexGeb <- pidSexGeb[order(pidSexGeb$pid),]
pidSexGeb <- pidSexGeb[setdiff(1:nrow(pidSexGeb), which(pidSexGeb$gebjahr<0)),]
pidSexGeb <- pidSexGeb[order(pidSexGeb$pid, pidSexGeb$syear, decreasing = TRUE),]
pidSexGeb <- pidSexGeb[!duplicated(pidSexGeb$pid),]
COV201119 <- COV[COV$syear %in% 2011:2019,]
erw <- COV201119[, c("pid", "hid" ,"syear", "plb0022_h")] 
erw <- erw[order(erw$pid),]
erw <- erw[setdiff(1:nrow(erw), which(is.na(erw$plb0022_h), arr.ind=TRUE)),]
erw <- erw[setdiff(1:nrow(erw), which(erw$plb0022_h<0)),]
erw <- erw[order(erw$pid, erw$syear, decreasing = TRUE),]
erw <- erw[!duplicated(erw$pid),]
table(erw$pid %in% pidSexGeb$pid)
erw <- merge(erw, pidSexGeb[, c("pid","gebjahr", "sex")], by="pid", all.x=TRUE)
erw$empl2019 <- ifelse(erw$plb0022_h %in% 1, "VZ", ifelse(erw$plb0022_h %in% 2, "PT", ifelse(erw$plb0022_h %in% 9, "nichtEW", ifelse(erw$plb0022_h %in% c(3,4,7,8,10),"Anderes",NA))))

options(warn=2)
findEmplHH <- function(i){
  #cat("It: ",i,"\n")
  #i <- 1246
  res <- NA
  hidI <- SchoolR$hid[i]
  mat1 <- erw[erw$hid %in% hidI,]
  mat1 <- mat1[mat1$gebjahr < 2000 & mat1$gebjahr > 1944,] # parent age between 19 and 74
  if(nrow(mat1)>2){
    # cat("more people in HH IT: ", i, "\n")
    vv <- mat1$gebjahr - median(mat1$gebjahr)
    mat1 <- mat1[order(vv, decreasing = FALSE),][1:2,]
  }
  if(nrow(mat1)==2){
    mm <- paste(mat1$empl2019, collapse ="-")
    if(mm %in% c("VZ-VZ", "VZ-PT", "VZ-nichtEW", "VZ-Anderes", "PT-VZ", "nichtEW-VZ", "Anderes-VZ")) 
      res <- "VZ"
    if(mm %in% c("PT-PT", "PT-nichtEW", "PT-Anderes", "nichtEW-PT", "Anderes-PT")) res <- "PT"    
    if(mm %in% c("nichtEW-Anderes", "Anderes-nichtEW", "Anderes-Anderes")) res <- "Anderes"
    if(mm %in% c("nichtEW-nichtEW")) res <- "nichtEW"
    if(NA %in% mat1$empl2019) res <- "unknown"
    if(is.na(res)) res <- "other"
  }
  if(nrow(mat1)==1) {
    if(mat1$empl2019 %in% "VZ") res <- "VZ"
    if(mat1$empl2019 %in% "PT") res <- "PT"  
    if(mat1$empl2019 %in% "nichtEW") res <- "nichtEW"
    if(mat1$empl2019 %in% "Anderes") res <- "Anderes"   
    if(NA %in% mat1$empl2019) res <- "unknown"
    if(is.na(res)) res <- "other"
  }

  if(nrow(mat1)==0){
    #cat("nobody in HH IT: ", i, "\n")
    res <- "unknown"
  }
  
  if(res %in% "other"){
    cat("It: ", i, "\n")
    print(mat1)
  }
  
  return(res)
}
rr <- sapply(1:nrow(SchoolR), findEmplHH)
table(rr)
rr[rr %in% "unknown"] <- NA
table(rr, exclude=NULL)
SchoolR$empl2019_HH <- rr

# ---------------------------------------------------------------
# Family status
# ---------------------------------------------------------------
setwd("C:\\Users\\Freddie\\Documents\\CovS\\Zeitverwendung\\DATA")
hgen <- read_dta("hgen.dta")
hidHT <- hgen[hgen$syear %in% 1984:2019, c("hid", "syear", "hgtyp1hh")]
hidHT <- hidHT[order(hidHT$hid),]
hidHT <- hidHT[setdiff(1:nrow(hidHT), which(is.na(hidHT$hgtyp1hh), arr.ind=TRUE)),]
hidHT <- hidHT[setdiff(1:nrow(hidHT), which(hidHT$hgtyp1hh<0)),]
hidHT <- hidHT[order(hidHT$hid, hidHT$syear, decreasing = TRUE),]
hidHT <- hidHT[!duplicated(hidHT$hid),]
hidHT <- hidHT[,-2]
SchoolR <- merge(SchoolR, hidHT, by="hid", all.x=TRUE)
table(SchoolR$hgtyp1hh, exclude=NULL)
SchoolR$alleinE <- ifelse(SchoolR$hgtyp1hh %in% 3,1,0)

# ---------------------------------------------------------------
# Migrationbackground in HH
# ---------------------------------------------------------------
options(warn=2) # Stop if warning 
mig <- as.data.frame(ppL[ppL$syear %in% 2011:2019, c("pid", "hid", "syear", "migback")])
mig <- mig[order(mig$pid),]
mig <- mig[setdiff(1:nrow(mig), which(is.na(mig$migback), arr.ind=TRUE)),]
mig <- mig[setdiff(1:nrow(mig), which(mig$migback<0)),]
mig <- mig[order(mig$pid, mig$syear, decreasing = TRUE),]
mig <- mig[!duplicated(mig$pid),]
ppL2019 <- ppL[ppL$syear %in% 2019, c("hid", "pid", "migback")]
getMigHHMember <- function(i){
  #i <- 64
  pid <- SchoolR$pid[i]
  hid <- SchoolR$hid[i]
  pidsHH2019 <- unlist(unique(ppL2019[ppL2019$hid %in% hid, "pid"]))
  hhMat <- pidSexGeb[pidSexGeb$pid %in% pidsHH2019 & pidSexGeb$gebjahr <=2000,]
  pidsHH_ohneAK <- setdiff(hhMat$pid, pid)
  res <- NA
  temp <- NA
  if(length(pidsHH_ohneAK)>0){
    if(length(pidsHH_ohneAK)==1){
      temp <- as.numeric(mig[mig$pid %in% pidsHH_ohneAK,4])
      res <- ifelse(temp %in% 2,1,0)
    } else {
      if(length(pidsHH_ohneAK)==2){
        temp <- as.numeric(mig[mig$pid %in% pidsHH_ohneAK,4])
      } else {
        vv <- hhMat$gebjahr - median(hhMat$gebjahr)
        hhMat <- hhMat[order(vv, decreasing = FALSE),][1:2,]       
        temp <- as.numeric(mig[mig$pid %in% hhMat$pid,4])
      }
    } 
    temp <- paste(temp,collapse="-")
    res <- ifelse(temp %in% "2-2", 1, ifelse(temp %in% c("1-2", "2-1", "2-3", "3-2"),2,0))
  } else {
    cat("It: ",i,"\n")
  }
  return(res)
}
nn <- sapply(1:nrow(SchoolR), getMigHHMember) 
table(nn, exclude=NULL) # 8 miss, 0 none of parents has migback, 1 both have direct migback, 2 one parent has direct migback
SchoolR$migParents <- nn

# ---------------------------------------------------------------
# Number of children in HH
# ---------------------------------------------------------------
bjh_kind <- as.data.frame(read_dta("bjh_kind.dta"))
nnam <- c("hid", "bjk_87_02", "bjk_87_05", "bjk_87_08", "bjk_87_11", "bjk_87_14") 
kkANZ <- bjh_kind[bjh_kind$hid %in% SchoolR$hid,nnam]
rra <- (apply(kkANZ[,-1], 1, function(vec) {length(vec[vec>0])}))
kkANZ <- cbind(kkANZ, rra)
kkANZ <- kkANZ[, c(1,7)]
SchoolR <- merge(SchoolR, kkANZ, by="hid", all.x=TRUE)
SchoolR$numKids <- ifelse(SchoolR$rra %in% 1, 0, ifelse(SchoolR$rra %in% 2, 1, 2))

# ---------------------------------------------------------------
# Last Steps
# ---------------------------------------------------------------
SchoolR$GY <- ifelse(is.na(SchoolR$schoolType), NA, ifelse(SchoolR$schoolType %in% 4,1,0)) 
SchoolR$gpa <- (SchoolR$Note_D + SchoolR$Note_M)/2
SchoolR$perf <- ifelse(is.na(SchoolR$gpa), NA, ifelse(SchoolR$gpa <=2,1,0))
table(SchoolR$schoolType, SchoolR$ageKid, exclude=NULL) # remove Grundschule, but let NAs in
SchoolR <- SchoolR[!(SchoolR$schoolType %in% 1),]
SchoolR <- SchoolR[, !(colnames(SchoolR) %in% c("hgtyp1hh","rra", "pgcasmin", "birthm", "birthy", "schoolType", "Note_D", "Note_M", "gpa"))]

#################################################################
# Descriptives 2019
#################################################################

mean(SchoolR$ageKid)
table(SchoolR$HausTime, exclude=NULL)
w <- wtd.table(SchoolR$HausTime, weights=SchoolR$hhrf)
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
cumsum(rw)

table(SchoolR$HausTime, exclude=NULL)
SchoolR$HausTime[is.na(SchoolR$HausTime)] <- -1
w <- wtd.table(SchoolR$HausTime, weights=SchoolR$hhrf)
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
cumsum(rw)
SchoolR$HausTime[SchoolR$HausTime %in% -1] <- NA
table(SchoolR$HausTime, exclude=NULL)

table(SchoolR$HausTime[SchoolR$bild %in% "niedrig"], exclude=NULL)
w <- wtd.table(SchoolR$HausTime[SchoolR$bild %in% "niedrig"], weights=SchoolR$hhrf[SchoolR$bild %in% "niedrig"])
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
rw
cumsum(rw)

table(SchoolR$HausTime[SchoolR$bild %in% "mittel"], exclude=NULL)
w <- wtd.table(SchoolR$HausTime[SchoolR$bild %in% "mittel"], weights=SchoolR$hhrf[SchoolR$bild %in% "mittel"])
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
rw
cumsum(rw)

table(SchoolR$HausTime[SchoolR$bild %in% "hoch"], exclude=NULL)
w <- wtd.table(SchoolR$HausTime[SchoolR$bild %in% "hoch"], weights=SchoolR$hhrf[SchoolR$bild %in% "hoch"])
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
rw
cumsum(rw)

table(SchoolR$HausTime[SchoolR$perf %in% 0], exclude=NULL)
w <- wtd.table(SchoolR$HausTime[SchoolR$perf %in% 0], weights=SchoolR$hhrf[SchoolR$perf %in% 0])
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
rw
cumsum(rw)

table(SchoolR$HausTime[SchoolR$perf %in% 1], exclude=NULL)
w <- wtd.table(SchoolR$HausTime[SchoolR$perf %in% 1], weights=SchoolR$hhrf[SchoolR$perf %in% 1])
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
rw
cumsum(rw)

table(SchoolR$HausTime[SchoolR$GY %in% 0], exclude=NULL)
w <- wtd.table(SchoolR$HausTime[SchoolR$GY %in% 0], weights=SchoolR$hhrf[SchoolR$GY %in% 0])
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
rw
cumsum(rw)

table(SchoolR$HausTime[SchoolR$GY %in% 1], exclude=NULL)
w <- wtd.table(SchoolR$HausTime[SchoolR$GY %in% 1], weights=SchoolR$hhrf[SchoolR$GY %in% 1])
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
rw
cumsum(rw)

table(SchoolR$bild, exclude=NULL)
SchoolR$bild[is.na(SchoolR$bild)] <- -1
w <- wtd.table(SchoolR$bild, weights=SchoolR$hhrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
SchoolR$bild[SchoolR$bild %in% -1] <- NA
table(SchoolR$bild, exclude=NULL)

table(SchoolR$empl2019_HH, exclude=NULL)
SchoolR$empl2019_HH[is.na(SchoolR$empl2019_HH)] <- -1
w <- wtd.table(SchoolR$empl2019_HH, weights=SchoolR$hhrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
SchoolR$empl2019_HH[SchoolR$empl2019_HH %in% -1] <- NA
table(SchoolR$empl2019_HH, exclude=NULL)

table(SchoolR$ageKid, exclude=NULL)
SchoolR$ageKidCat <- ifelse(SchoolR$ageKid <15,0,1)
w <- wtd.table(SchoolR$ageKidCat, weights=SchoolR$hhrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(SchoolR$sexChild, exclude=NULL)
table(SchoolR$sexChild, exclude=NULL)/nrow(SchoolR)
w <- wtd.table(SchoolR$sexChild, weights=SchoolR$hhrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(SchoolR$GY, exclude=NULL)
SchoolR$GY[is.na(SchoolR$GY)] <- -1
w <- wtd.table(SchoolR$GY, weights=SchoolR$hhrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
table(SchoolR$GY, exclude=NULL)/nrow(SchoolR)
SchoolR$GY[SchoolR$GY %in% -1] <- NA
table(SchoolR$GY, exclude=NULL)

table(SchoolR$perf, exclude=NULL)
SchoolR$perf[is.na(SchoolR$perf)] <- -1
w <- wtd.table(SchoolR$perf, weights=SchoolR$hhrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
SchoolR$perf[SchoolR$perf %in% -1] <- NA
table(SchoolR$perf, exclude=NULL)
