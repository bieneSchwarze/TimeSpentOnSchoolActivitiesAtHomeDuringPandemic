#########################################################
#########################################################
# R Code to Manuscript
#
# Time spent on school-related activities at home during the pandemic:
# A longitudinal analysis of social group inequality 
# among secondary school students
#
# Data preparation for 2020  
#
# SZ, 02.09.2020
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

#################################################################
# A. Data preparation
#################################################################
# ---------------------------------------------------------------
# Read Data
# ---------------------------------------------------------------
COV <- read_dta("SOEP_CoV.dta") # SOEP-CoV Data
ww_cov <- read_dta("SOEP_CoV_Weights.dta") # survey weights
COV2019 <- COV[COV$syear == 2019,]
COV2020 <- COV[COV$syear == 2020,]

pidCov <- ww_cov$pid[ww_cov$phrf_cati!=0] # N=6667 netto cases
COV_n <- COV[COV$pid %in% pidCov, c("hid", "pid", "syear", "pkkinder", "pskinder", "pcovskl1", "pcovskl2", "pcovskl3", 
                                    "pcovskl4", "pcovskl5", "pcovskl6", "pcovskl7", "pcovskl8", "pcovskl9",
                                    "pcovzuf4", "pcovhaus2",
                                    "plb0022_h", "bula_cov",
                                    "ppgeb", "ppgeb2", "ppgeb3", "ppgeb4", "ppgeb5", "ppgeb6", "ppgeb7", "ppgeb8",
                                    "pnrfest2", "pnrfest3", "pnrfest4", "pnrfest5", "pnrfest6", "pnrfest7", "pnrfest8",
                                    "pdatt", "pdatm", "tranche")] # netto cases
COV_n <- COV_n[COV_n$syear %in% 2020 & COV_n$pskinder %in% 1,] # N=1906 HH with children
COV_n <- COV_n[order(COV_n$hid, COV_n$pid), ]
COV_n[COV_n<0] <- NA
colnames(COV_n)[6:14] <- c("Email", "ServerCloud", "vorSchulschließung", "Konferenzschaltung", "Nichts", "Anderes", "Digital", "regSchule", "tageStSchule") 

# ---------------------------------------------------------------
# Children in household and age
# ---------------------------------------------------------------
COV_n$kidsID <- NA
COV_n$ageKid <- NA
for(i in 1:nrow(COV_n)){
  bY <- as.numeric(COV_n[i, c("ppgeb", "ppgeb2", "ppgeb3", "ppgeb4", "ppgeb5", "ppgeb6", "ppgeb7", "ppgeb8")])
  bY <- bY[bY>0]
  inD <- which(bY>=2002 & bY<=2014) # zw. 6 und 18 Jahren
  inD <- inD[inD>1] # nicht die Befragungsperson
  inD <- inD[which.min(bY[inD])]
  if(length(inD)>0){
    COV_n$kidsID[i] <- as.numeric(COV_n[i, paste("pnrfest", inD, sep = "")])   
    COV_n$ageKid[i] <- 2020-bY[inD]
  } 
}
table(COV_n$ageKid, exclude=NULL)

# ---------------------------------------------------------------
# CASMIN in household (highest of adults)
# ---------------------------------------------------------------
casM <- as.data.frame(COV[COV$syear %in% 2011:2018, c("pid", "hid", "syear", "gebjahr", "pgcasmin")])
casM <- casM[order(casM$hid, casM$pid),]
casM <- casM[setdiff(1:nrow(casM), which(is.na(casM$pgcasmin), arr.ind=TRUE)),]
casM <- casM[setdiff(1:nrow(casM), which(casM$pgcasmin<0)),]
casM <- casM[setdiff(1:nrow(casM), which(casM$hid<0)),]
casM <- casM[order(casM$hid, casM$pid, casM$syear, decreasing = TRUE),]
casM <- casM[!duplicated(casM$pid),]
casM <- casM[casM$gebjahr <= 2000,]
casM <- casM[order(casM$hid, casM$pgcasmin, decreasing = TRUE),]
casM <- casM[!duplicated(casM$hid),]
table(COV_n$hid %in% casM$hid) # N=29 miss
COV_n <- merge(COV_n, casM[, c("hid", "pgcasmin")], by="hid", all.x=TRUE)
table(COV_n$pgcasmin, exclude=NULL)
COV_n$bild <- ifelse(COV_n$pgcasmin %in% c(0, 1, 2, 4), "niedrig", ifelse(COV_n$pgcasmin %in% c(3,5,6,7), "mittel", ifelse(COV_n$pgcasmin %in% c(8,9), "hoch", NA))) 
table(COV_n$bild, exclude=NULL)

# ---------------------------------------------------------------
# Emplyoment in 2019 in household (not yet available for 2020)
# ---------------------------------------------------------------
# Employment situation of household adult members
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
# Employment situation of household
options(warn=2) 
findEmplHH <- function(i){ 
  res <- NA
  hidI <- COV_n$hid[i]
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
    cat("nobody in HH IT: ", i, "\n")
    res <- "unknown"
  }
  
  if(res %in% "other"){
    cat("It: ", i, "\n")
    print(mat1)
  }
  
  return(res)
}
rr <- sapply(1:nrow(COV_n), findEmplHH)
table(rr)
rr[rr %in% "unknown"] <- NA
table(rr, exclude=NULL)
COV_n$empl2019_HH <- rr

# ---------------------------------------------------------------
# Number children (<=18J) in household
# ---------------------------------------------------------------
COV_n$numKids <- NA
for(i in 1:nrow(COV_n)){
  #i <- 1
  bY <- as.numeric(COV_n[i, c("ppgeb", "ppgeb2", "ppgeb3", "ppgeb4", "ppgeb5", "ppgeb6", "ppgeb7", "ppgeb8")])
  bY <- bY[bY>0]
  inD <- which(bY>=2002 & bY<=2020) # zw. 0 und 18 Jahren
  inD <- inD[inD>1] # nicht die Befragungsperson
  if(length(inD)>0){
    COV_n$numKids[i] <- length(inD)
  } else {
    COV_n$numKids[i] <- 0
  }  
}
table(COV_n$numKids, exclude=NULL) 
table(COV_n$numKids, COV_n$ageKid, exclude=NULL)
COV_n <- COV_n[COV_n$numKids > 0,] # kick out hh with zero children; in these hh there are only adult children (N=90)
table(COV_n$numKids, COV_n$ageKid, exclude=NULL)

# ---------------------------------------------------------------
# Sysrel occupation
# ---------------------------------------------------------------
table(COV[COV$syear %in% 2019 & COV$pid %in% COV_n$pid, "systemrelevant_nace"], exclude=NULL) # 283 miss
sysrel <- COV[COV$syear %in% 2019 & COV$pid %in% COV_n$pid, c("pid", "systemrelevant_nace")]
COV_n <- merge(COV_n, sysrel, by="pid", all.x=TRUE)
table(COV_n$systemrelevant_nace, exclude=NULL) # N=377 miss (~21%)

# ---------------------------------------------------------------
# Marital Status
# ---------------------------------------------------------------
hgen <- read_dta("hgen.dta") # from usal SOEP data
hidHT <- hgen[hgen$syear %in% 1984:2019, c("hid", "syear", "hgtyp1hh")]
hidHT <- hidHT[order(hidHT$hid),]
hidHT <- hidHT[setdiff(1:nrow(hidHT), which(is.na(hidHT$hgtyp1hh), arr.ind=TRUE)),]
hidHT <- hidHT[setdiff(1:nrow(hidHT), which(hidHT$hgtyp1hh<0)),]
hidHT <- hidHT[order(hidHT$hid, hidHT$syear, decreasing = TRUE),]
hidHT <- hidHT[!duplicated(hidHT$hid),]
hidHT <- hidHT[,-2]
COV_n <- merge(COV_n, hidHT, by="hid", all.x=TRUE)
table(COV_n$hgtyp1hh, exclude=NULL) # no miss
COV_n$singP <- ifelse(COV_n$hgtyp1hh %in% 3,1,0)

# ---------------------------------------------------------------
# Migration status partner/person in the same household in 2019 
# ---------------------------------------------------------------
mig <- as.data.frame(COV[COV$syear %in% 2011:2018, c("pid", "syear", "migback")])
mig <- mig[order(mig$pid),]
mig <- mig[setdiff(1:nrow(mig), which(is.na(mig$migback), arr.ind=TRUE)),]
mig <- mig[setdiff(1:nrow(mig), which(mig$migback<0)),]
mig <- mig[order(mig$pid, mig$syear, decreasing = TRUE),]
mig <- mig[!duplicated(mig$pid),]
COV_n <- merge(COV_n, mig[,-2], by="pid", all.x=TRUE) # only for surveyed person migback
table(COV_n$migback, exclude=NULL) # code: 1 no migback, 2 direct migback, 3 indirect migback
options(warn=2) # Stop if warning 
getMigHHMember <- function(i){ # get mig also for other adult person in hh
  pid <- COV_n$pid[i]
  hid <- COV_n$hid[i]
  pidsHH2019 <- unlist(unique(COV2019[COV2019$hid %in% hid, "pid"]))
  hhMat <- pidSexGeb[pidSexGeb$pid %in% pidsHH2019,]
  pidsHH_ohneAK <- setdiff(hhMat$pid, pid)
  if(length(pidsHH_ohneAK)==0){
    res <- -1 # No other person in household matrix
  } else {
    if(length(pidsHH_ohneAK)==1){
      res <- as.numeric(mig[mig$pid %in% pidsHH_ohneAK,3])
    } else {  
      b1 <- unlist(hhMat[hhMat$pid %in% pidsHH_ohneAK,  "gebjahr"])
      if(!(TRUE %in% (2020-b1>18))){ # all under 18
        res <- -2 # no other person in household matrix over 18 years old
      }
      b0 <- COV_n[COV_n$pid %in% pid, "ppgeb"]  #  If there are several people in the HH, take the one who is closest in age and over 18.
      ageAbs <- abs(b1-b0)
      minAbs <- min(abs(b1-b0))
      pidPN <- hhMat[which(ageAbs==minAbs),"pid"]
      res <- as.numeric(mig[mig$pid %in% pidPN,3])
      if(length(unlist(res))==0)
        return(NA) # No mig available for other person in HH
    }
  }
  
  if(i%%500==0) 
    cat("It: ", i, "\n")
  return(unlist(res))
}
nn <- sapply(1:nrow(COV_n), getMigHHMember) 
mig2019_hhMember <- cbind(COV_n$pid, nn)
table(mig2019_hhMember[,2], exclude=NULL) #-1 keine andere Person im HH
colnames(mig2019_hhMember) <- c("pid", "mig2019_HHMitgl")
COV_n <- merge(COV_n, mig2019_hhMember, by="pid", all.x=TRUE)
table(COV_n$mig2019_HHMitgl, COV_n$singP, exclude=NULL)
table(COV_n$mig2019_HHMitgl, exclude=NULL)
COV_n$mig2019_HHMitgl[COV_n$mig2019_HHMitgl==-1 & COV_n$singP%in% 0] <- NA # the assignment to the pair may not fit because I cannot find any partner info, so set to NA.
table(COV_n$mig2019_HHMitgl, COV_n$migback, exclude=NULL)
COV_n$migParents <- NA
COV_n$migParents[COV_n$mig2019_HHMitgl %in% -1 & COV_n$migback %in% 2] <- 2 
COV_n$migParents[COV_n$mig2019_HHMitgl %in% -1 & COV_n$migback %in% c(1,3)] <- 0 
COV_n$migParents[COV_n$mig2019_HHMitgl %in%  c(1,3) & COV_n$migback %in% c(1,3)] <- 0 
COV_n$migParents[COV_n$mig2019_HHMitgl %in%  c(1,3) & COV_n$migback %in% 2] <- 1 
COV_n$migParents[COV_n$mig2019_HHMitgl %in%  2 & COV_n$migback %in% 2] <- 2 
COV_n$migParents[COV_n$mig2019_HHMitgl %in%  2 & COV_n$migback %in% c(1,3)] <- 1  # 0 no direct Migration background of couple, 1 one partner has direct mig back, 2 both have
table(COV_n$migParents, exclude=NULL) # N=284 miss von N=1816 (~14,5%)

# ---------------------------------------------------------------
# Schooltype
# ---------------------------------------------------------------
# Take schooltype info from household questionnaire, for those younger than 17 years
bjh_kind <- as.data.frame(read_dta("bjh_kind.dta"))
table(COV_n$hid %in% bjh_kind$hid) # miss 84
birthYN <- c("bjk_87_03", "bjk_87_06", "bjk_87_09", "bjk_87_12", "bjk_87_15")
schoolN <- c("bjk_88_02", "bjk_88_04", "bjk_88_06", "bjk_88_08", "bjk_88_10")
bjh_kind <- bjh_kind[bjh_kind$hid %in% COV_n$hid, c("hid",birthYN, schoolN)]
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
table(bjh_kind_long$birthY, bjh_kind_long$schoolType, exclude=NULL)
COV_n$birthy <- 2020 - COV_n$ageKid 
table(COV_n$birthy)
table(COV_n$ageKid)
findSchool <- function(i){
  cat("\n-\nIt: ",i,"\n")
  res <- NA
  school_hid <- bjh_kind_long[bjh_kind_long$hid %in% COV_n$hid[i],]
  if(FALSE %in% is.na(school_hid$birthY)){
    sT <- school_hid[school_hid$birthY %in% COV_n$birthy[i],]
    if(dim(sT)[1]>0)
      res <- as.numeric(sT[1,]["schoolType"]) 
      cat(res)
  }
  return(res)
}
rr <- sapply(1:nrow(COV_n), findSchool)
rr <- as.numeric(unlist(rr))
head(rr)
COV_n$schoolType <- rr
table(COV_n$schoolType, exclude=NULL)
table(COV_n$ageKid, COV_n$schoolType, exclude=NULL)
# Take schooltype info from youth questionnaire, for those who are 17 years old in 2019 and 18 in 2020
bjjugend <- as.data.frame(read_dta("bjjugend.dta"))
bjjugend <- bjjugend[!(bjjugend$sample1 %in% c(30,31,34)),] 
SchoolY <- as.data.frame(bjjugend[, c("hid", "pid", "bjjbirthy", "bjj_33_01", "bjj_33_02", "bjj_36", "bjj_20")])
colnames(SchoolY) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime", "schoolType") 
SchoolY[SchoolY$schoolType < 0, "schoolType"] <- NA
SchoolY$schoolType <- SchoolY$schoolType + 1 
SchoolY$ageKid <- 2020 - SchoolY$birthy
COV_n18 <- COV_n[COV_n$ageKid %in% 18,]
COV_n18 <- COV_n18[,!(colnames(COV_n18) %in% "schoolType")]
table(COV_n18$hid %in% SchoolY$hid) # n=35 miss
SchoolY <- SchoolY[!is.na(SchoolY$schoolType),]
SchoolY <- SchoolY[order(SchoolY$hid),]
SchoolY <- SchoolY[!duplicated(SchoolY$hid),] # 5 hh have more than one 18 year old, take randomly one - cannot make any smart choice here since I have no further info. 
COV_n18 <- merge(COV_n18, SchoolY[, c("hid", "schoolType")], by="hid", all.x=TRUE)
COV_n7to17 <- COV_n[COV_n$ageKid %in% 7:17,]
COV_n <- rbind(COV_n7to17, COV_n18)
COV_n <- COV_n[order(COV_n$hid,COV_n$pid, COV_n$syear), ]
table(COV_n$schoolType, exclude=NULL) # 170 miss

# ---------------------------------------------------------------
# Performance in school 2019
# ---------------------------------------------------------------
School1 <- read_dta("bjschool.dta") # SOEP data on school children (younger ones)
School1 <- School1[!(School1$sample1 %in% c(30,31,34)),] # without refugees
School1 <- as.data.frame(School1[, c("hid", "pid", "bjsbirthy", "bjs_06_01", "bjs_06_02", "bjs_14")])
colnames(School1) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime") 
School2 <- read_dta("bjschool2.dta")  # SOEP data on school children (older ones)
School2 <- School2[!(School2$sample1 %in% c(30,31,34)),] # without refugees
School2 <- as.data.frame(School2[, c("hid", "pid", "bjfjbirthy", "bjfj_06_01", "bjfj_06_02", "bjfj_12")])
colnames(School2) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime")
School <- rbind(School1, School2)
School$ageKid <- 2019 - School$birthy
table(School$ageKid , exclude=NULL) # only age 11-12, 13-14, 16-17 (miss age 10, 15) in 2019
SchoolR <- School[order(School$hid, School$ageKid),]
SchoolR <- SchoolR[SchoolR$ageKid < 17, ] 
bjjugend <- as.data.frame(read_dta("bjjugend.dta"))
bjjugend <- bjjugend[!(bjjugend$sample1 %in% c(30,31,34)),] 
School3 <- as.data.frame(bjjugend[, c("hid", "pid", "bjjbirthy", "bjj_33_01", "bjj_33_02", "bjj_36")])
colnames(School3) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime") 
School3$ageKid <- 2019 - School3$birthy
School3 <- School3[, c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime", "ageKid")]
SchoolR <- rbind(SchoolR, School3)
SchoolR <- SchoolR[order(SchoolR$hid, SchoolR$pid),]
getPerf <- function(i){
  res <- NA
  hidi <- COV_n[i,"hid"]
  sss <- SchoolR[SchoolR$hid %in% hidi,]
  if(nrow(sss)>0){
    mE <- mean(as.numeric(sss[1,][c("Note_D", "Note_M")]), na.rm=TRUE)
    if(!is.na(mE))
      res <- ifelse(mE <=2, 1, 0)
  }
  return(res)
}
bb <- sapply(1:nrow(COV_n),getPerf)
table(bb, exclude=NULL)
COV_n$perf_2019 <- bb # 64% miss
# Performance in school 2018 to replace missings  
School1 <- read_dta("bischool.dta")  # SOEP data on school children (younger ones)
School1 <- School1[!(School1$sample1 %in% c(30,31,34)),] # without refugees
School1 <- as.data.frame(School1[, c("hid", "pid", "bisbirthy", "bis_06_01", "bis_06_02", "bis_14")]) # check vars
colnames(School1) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime") 
School2 <- read_dta("bischool2.dta")  # SOEP data on school children (younger ones)
School2 <- School2[!(School2$sample1 %in% c(30,31,34)),] # without refugees
School2 <- as.data.frame(School2[, c("hid", "pid", "bifjbirthy", "bifj_06_01", "bifj_06_02", "bifj_12")]) # check vars
colnames(School2) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime")
School <- data.frame(hid= c(School1$hid, School2$hid),
                     pid= c(School1$pid, School2$pid),
                     birthy=c(School1$birthy, School2$birthy),
                     NoteD= c(School1$Note_D, School2$Note_D),
                     NoteM= c(School1$Note_M, School2$Note_M),
                     HausTime= c(School1$HausTime, School2$HausTime))
School$ageKid <- 2018 - School$birthy
table(School$ageKid , exclude=NULL) # only age 11-12, 13-14 (miss age 10, 15) in 2018
SchoolR <- School[order(School$hid, School$ageKid),]
bjjugend <- as.data.frame(read_dta("bijugend.dta"))
bjjugend <- bjjugend[!(bjjugend$sample1 %in% c(30,31,34)),] 
School3 <- as.data.frame(bjjugend[, c("hid", "pid", "bijbirthy", "bij_45_01", "bij_45_02", "bij_89_q121")]) # check vars
colnames(School3) <- c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime") 
School3$ageKid <- 2018 - School3$birthy
table(School3$ageKid)
School3 <- School3[, c("hid", "pid", "birthy", "Note_D", "Note_M", "HausTime", "ageKid")]
SchoolR <- data.frame(hid= c(SchoolR$hid, School3$hid),
                     pid= c(SchoolR$pid, School3$pid),
                     birthy=c(SchoolR$birthy, School3$birthy),
                     Note_D= c(SchoolR$NoteD, School3$Note_D),
                     Note_M= c(SchoolR$NoteM, School3$Note_M),
                     HausTime= c(SchoolR$HausTime, School3$HausTime),
                     ageKid= c(SchoolR$ageKid, School3$ageKid))
SchoolR <- SchoolR[order(SchoolR$hid, SchoolR$pid),]
bc <- sapply(1:nrow(COV_n),getPerf)
table(bc, exclude=NULL)
COV_n$perf_2018 <- bc
# Replace miss performance values in 2019 with values from 2018 
COV_n$perf <- COV_n$perf_2019
COV_n$perf[is.na(COV_n$perf)] <- COV_n$perf_2018[is.na(COV_n$perf)]
table(COV_n$perf, exclude=NULL) # 44% miss
COV_red <- COV_n[, c("pid", "hid", "Email", "ServerCloud", "vorSchulschließung", "Konferenzschaltung", 
                     "Nichts", "Anderes", "Digital", "regSchule", "tageStSchule", "pcovzuf4", "bula_cov",
                     "ppgeb", "tranche", "ageKid", "psex", "bild", "empl2020", "empl2019_HH", "singP",
                     "numKids", "systemrelevant_nace", "migParents", "pcovhaus2", "schoolType", "perf",
                     "kidsID")] 

# ---------------------------------------------------------------
# Schooltype Gymnasium 
# ---------------------------------------------------------------
COV_red$GY <- NA
COV_red$GY[COV_red$schoolType %in% 4] <- 1
COV_red$GY[COV_red$schoolType %in% c(1,2,3,5,6,7)] <- 0
table(COV_red$GY, exclude=NULL) # N=170 miss (~9,4%)

# ---------------------------------------------------------------
# How schooling material was submitted during lockdown
# ---------------------------------------------------------------
# Replace NAs indicate that the related channel was not used
COV_red$Email[is.na(COV_red$Email)] <- 0
COV_red$ServerCloud[is.na(COV_red$ServerCloud)] <- 0
COV_red$vorSchulschließung[is.na(COV_red$vorSchulschließung)] <- 0
COV_red$Konferenzschaltung[is.na(COV_red$Konferenzschaltung)] <- 0
COV_red$Nichts[is.na(COV_red$Nichts)] <- 0
COV_red$Anderes[is.na(COV_red$Anderes)] <- 0
COV_red$Digital[is.na(COV_red$Digital)] <- 0
COV_red$tageStSchule[is.na(COV_red$tageStSchule)] <- 0
COV_red$regSchule[is.na(COV_red$regSchule)] <- 0
COV_red$Digital[COV_red$Email %in% 1 | COV_red$ServerCloud %in% 1] <- 1
COV_red <- COV_red[,!(colnames(COV_red) %in% c("Email","ServerCloud"))]
mm <- apply(COV_red[, c("Digital", "vorSchulschließung", "Konferenzschaltung", "Anderes")], 1,sum, na.rm=TRUE)
mm <- ifelse(mm>1,1,0)
table(mm, exclude=NULL)
mm <- cbind(COV_red$pid, mm)
colnames(mm) <- c("pid", "mehrereKanäle") # more than one channel
COV_red <- merge(COV_red, mm, by="pid", all.x=TRUE)

# ---------------------------------------------------------------
# Add child gender
# ---------------------------------------------------------------
ppL <- as.data.frame(read_dta("ppathl.dta"))
ppL2019 <- ppL[ppL$syear %in% 2019, c("hid", "pid", "sex")]
table(!is.na(COV_red$kidsID))
table(COV_red$kidsID %in% ppL2019$pid)
COV_red <- merge(COV_red, ppL2019[,-1], by.x="kidsID", by="pid", all.x = TRUE)
table(COV_red$sex, exclude=NULL)
colnames(COV_red)[colnames(COV_red) %in% "sex"] <- "sexChild"

# ---------------------------------------------------------------
# Remove Grundschule, but let NAs in and select via age
# ---------------------------------------------------------------
table(COV_red$schoolType, COV_red$ageKid, exclude=NULL)
COV_red <- COV_red[!(COV_red$schoolType %in% 1) & !(COV_red$ageKid %in% 7:9),]
table(COV_red$schoolType, COV_red$ageKid, exclude=NULL)

#################################################################
# Create the two data sets for tranches 2-4 and 5-9
#################################################################
# Tranchen 2-4
#################################################################
# ---------------------------------------------------------------
# Add survey weights Tranches 1-4
# ---------------------------------------------------------------
w_tr1to4 <- read_dta("SOEP_CoV_Weigths_TR1-4.dta") 
COV_tr14 <- COV_red[COV_red$tranche %in% 1:4,] 
table(COV_tr14$pid %in% w_tr1to4$pid) # N=1429, ok
COV_tr14 <- COV_tr14[,!(colnames(COV_tr14) %in% c("regSchule", "tageStSchule", "kidsID", "schoolType", "pcovzuf4"))]
COV_tr14 <- merge(COV_tr14, w_tr1to4[, c("pid", "hhrf", "phrf_cati")], by="pid", all.x = TRUE) # N=1008
COV_tr14$indtr24 <- ifelse(COV_tr14$tranche %in% 2:4, 1,0) 
table(COV_tr14$indtr24) # N=789

# ---------------------------------------------------------------
# Study missingness pattern in data
# ---------------------------------------------------------------
M <- md.pattern(COV_tr14, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr14),2) # up to 49% missing values, special case "pcovhaus" = question for use on time for school related activities
table(is.na(COV_tr14$pcovhaus2), COV_tr14$tranche ) # question for use on time for school related activities not in tranche 1

# ---------------------------------------------------------------
# Study selection between Tranches 1-4 and 2-4
# ---------------------------------------------------------------
M <- md.pattern(COV_tr14, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr14),2)
table(complete.cases(COV_tr14)) # ~29% complete cases / data lines
missVar <- round(M[nrow(M),]/nrow(COV_tr14),2)
missVar <- missVar[-length(missVar)]
varHighMiss <- names(missVar[missVar >= 0.04]) # form missing categories for variables with miss shares >=4%
COV_tr14_m <- COV_tr14
for(mv in varHighMiss){
  mval <- max(COV_tr14_m[,mv], na.rm=TRUE) + 1
  COV_tr14_m[,mv][is.na(COV_tr14_m[,mv])] <- mval
}
M <- md.pattern(COV_tr14_m, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr14_m),2)
fact <- c("vorSchulschließung", "Konferenzschaltung", "Nichts", "Anderes",
          "Digital", "bula_cov", "psex", "bild", "empl2019_HH", 
          "empl2020", "mehrereKanäle", 
          "singP", "systemrelevant_nace", "migParents",          
          "perf", "ferien", "GY", "indtr24", "pcovhaus2") 
num <- c("ppgeb", "ageKid", "numKids", "phrf_cati", "hhrf", "tranche")
for(f in fact){
  COV_tr14_m[,colnames(COV_tr14_m) %in% f] <- as.factor(COV_tr14_m[,colnames(COV_tr14_m) %in% f])
}
for(n in num){
  COV_tr14_m[,colnames(COV_tr14_m) %in% n] <- as.numeric(COV_tr14_m[,colnames(COV_tr14_m) %in% n])
}
options(warn=0) 
predM <- mice::make.predictorMatrix(data=COV_tr14_m) # impute data for selection analysis
predM[,1:2] <- 0
predM[1:2,] <- 0
impM <- mice::make.method(data=COV_tr14_m)
impP <- mice::mice(COV_tr14_m, m=1, predictorMatrix=predM, method=impM, maxit=20,seed=25732)
impP$loggedEvents # passt
D_sel <- complete(impP, action=1) # single imputation for missings in vars with <5% miss
covNam <- names(D_sel)[!(names(D_sel)%in% c("pid", "hid", "indtr24", "phrf_cati", "hhrf",
                                            "regSchule", "tageStSchule", "bula_cov",
                                             "tranche", "ferien", "pcovhaus2"))]
m_sel <- step(glm(as.formula(paste("indtr24",paste(covNam, collapse="+"), sep="~")), family= binomial(link = "logit"), data=D_sel), direction="both")
summary(glm(indtr24~ vorSchulschließung + Konferenzschaltung + Anderes + Digital + empl2020 + migParents, family= binomial(link = "logit"), data=D_sel)) 
m_sel <- glm(indtr24~ vorSchulschließung + Konferenzschaltung + Anderes + Digital + empl2020 + migParents, family= binomial(link = "logit"), data=D_sel)
p_tr24 <- predict(m_sel, type="response")
boxplot(p_tr24~indtr24, data=D_sel) # hardly any differences between parents who are part of Tranches 2-4 compared to the Tranches 1-4 -> no additional adjustment of the weights 
COV_tr24 <- COV_tr14[COV_tr14$indtr24 %in% 1,]
COV_tr24 <- COV_tr24[,!colnames(COV_tr24) %in% c("indtr24")]

# ---------------------------------------------------------------
# # What about distribution of child age?
# ---------------------------------------------------------------
# Does not really match official numbers -> post-stratify weights
COV_tr24$ageKidCat <- ifelse(COV_tr24$ageKid %in% c(10:14), 0, ifelse(COV_tr24$ageKid %in% NA, NA, 1))
table(COV_tr24$ageKidCat, exclude=NULL)
w <- wtd.table(COV_tr24$ageKidCat, weights=COV_tr24$hhrf)
round(w$sum.of.weights/sum(w$sum.of.weights),2) # This does not match any population total of SEK I students. Take 2019 distribution to adjust weights. 
ageK <- c(0.7, 0.3) # take from 2019
names(ageK) <- c("0", "1")
ageK <- round(ageK*sum(D_sel$hhrf),0)
ageK/sum(D_sel$hhrf)
pop.type <- data.frame(ageKidCat=names(ageK), Freq=as.numeric(ageK))
D_sel1 <- D_sel[D_sel$indtr24 %in% 1,]
D_sel1$ageKidCat <- ifelse(D_sel1$ageKid %in% c(10:14), 0, ifelse(D_sel1$ageKid %in% NA, NA, 1))
ddata <- svydesign(id=~pid, weights=~hhrf, data=D_sel1) # using pid here is ok, since there was only one person interviewed in each household
postSt <- postStratify(ddata, ~ageKidCat, pop.type, partial=T)
D_sel1$calweights1 <- weights(postSt)
COV_tr24 <- merge(COV_tr24, D_sel1[, c("pid","calweights1")], by="pid", all.x=TRUE)
wc <- questionr::wtd.table(x=COV_tr24$ageKidCat, weights=COV_tr24$calweights1, exclude=NULL)
wc/sum(wc)

#################################################################
# Tranchen 5-9
#################################################################
# ---------------------------------------------------------------
# Add survey weights Tranches 1-4
# ---------------------------------------------------------------
w_tr1to9 <- read_dta("2020-07-22_SOEP_CoV_Gewichte_v13.dta") 
COV_tr19 <- merge(COV_red, w_tr1to9[, c("pid", "hhrf", "phrf_cati")], by="pid", all.x = TRUE)
#COV_tr19$indtr59 <- ifelse(COV_tr19$tranche %in% 5:9 & COV_tr19$ferien %in% 0, 1,0) # nur Eltern, deren Kinder z.Z. der Befragung keine Ferien hatten und die in Tr5-9 befragt worden 
COV_tr19$indtr59 <- ifelse(COV_tr19$tranche %in% 5:9, 1,0) 
table(COV_tr19$indtr59) # N=343

# ---------------------------------------------------------------
# Study selection between Tranches 1-9 and 5-9
# ---------------------------------------------------------------
COV_tr19 <- COV_tr19[,!(colnames(COV_tr19) %in% c("schoolType", "pcovzuf4"))]
M <- md.pattern(COV_tr19, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr19),2) # Up to 44% missing values, special case "pvovhaus2": not collected in tranche 1
M <- md.pattern(COV_tr19, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr19),2)
table(complete.cases(COV_tr19)) # ~31% with complete data lines
missVar <- round(M[nrow(M),]/nrow(COV_tr19),2)
missVar <- missVar[-length(missVar)]
varHighMiss <- names(missVar[missVar >= 0.04]) # form missing categories for variables with miss shares >=2%
COV_tr19_m <- COV_tr19
for(mv in varHighMiss){
  mval <- max(COV_tr19_m[,mv], na.rm=TRUE) + 1
  COV_tr19_m[,mv][is.na(COV_tr19_m[,mv])] <- mval
}
M <- md.pattern(COV_tr19_m, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr19_m),2)
fact <- c("vorSchulschließung", "Konferenzschaltung", "Nichts", "Anderes",
          "Digital", "regSchule", "tageStSchule",  "bula_cov", "psex", "bild", 
          "empl2020", "empl2019_HH", "pcovhaus2", "mehrereKanäle", 
          "singP", "systemrelevant_nace", "migParents",          
          "perf", "ferien", "GY", "indtr59") 
num <- c("ppgeb", "ageKid", "numKids", "phrf_cati", "hhrf", "tranche")
for(f in fact){
  COV_tr19_m[,colnames(COV_tr19_m) %in% f] <- as.factor(COV_tr19_m[,colnames(COV_tr19_m) %in% f])
}
for(n in num){
  COV_tr19_m[,colnames(COV_tr19_m) %in% n] <- as.numeric(COV_tr19_m[,colnames(COV_tr19_m) %in% n])
}
options(warn=0) 
predM <- mice::make.predictorMatrix(data=COV_tr19_m)
predM[,1:2] <- 0
predM[1:2,] <- 0
impM <- mice::make.method(data=COV_tr19_m) # impute data for selection analysis
impM["bild"] <- "cart"
impM["empl2019_HH"] <- "cart"
impP <- mice::mice(COV_tr19_m, m=1, predictorMatrix=predM, method=impM, maxit=20,seed=987)
impP$loggedEvents # ok
covNam <- names(D_sel)[!(names(D_sel)%in% c("pid", "hid", "indtr59", "phrf_cati", "hhrf",
                                            "regSchule", "tageStSchule",  "Nichts", "bula_cov",
                                            "Digital", "Anderes", "vorSchulschließung", "Konferenzschaltung",
                                            "mehrereKanäle", "tranche", "ferien", "pcovhaus2"))]
m_sel <- step(glm(as.formula(paste("indtr59",paste(covNam, collapse="+"), sep="~")), family= binomial(link = "logit"), data=D_sel), direction="both")

summary(glm(indtr59~ empl2020, family= binomial(link = "logit"), data=D_sel)) 
m_sel <- glm(indtr59~ empl2020, family= binomial(link = "logit"), data=D_sel)
p_tr59 <- predict(m_sel, type="response")
boxplot(p_tr59~indtr59, data=D_sel) # no differences between parents who are part of Tranches 5-9 compared to Tranches 1-9 -> no additional adjustment of the weights 
COV_tr59 <- COV_tr19[COV_tr19$indtr59 %in% 1,]
COV_tr59 <- COV_tr59[,!colnames(COV_tr59) %in% c("indtr59")]

# ---------------------------------------------------------------------------------------
# Adjust to distribution: Education and employment status of Tranches 1-9 (post-stratify)
# ---------------------------------------------------------------------------------------
wb <- questionr::wtd.table(x=COV_tr14$bild, weights=COV_tr14$hhrf, exclude=NULL) # N=25 miss
bild <- round(wb/sum(wb),2)
bild <- round(bild*sum(D_sel$hhrf),0)
wb <- questionr::wtd.table(x=COV_tr14$empl2019_HH, weights=COV_tr14$hhrf, exclude=NULL) # no miss
emp <- round(wb/sum(wb),2)
emp <- round(emp*sum(D_sel$hhrf),0)
ageK <- c(0.7, 0.3) # take from 2019 data
names(ageK) <- c("0", "1")
ageK <- round(ageK*sum(D_sel$hhrf),0)

D_sel1 <- D_sel[D_sel$indtr59 %in% 1,] # no miss in bild
ddata1 <- svydesign(id=~pid, weights=~hhrf, data=D_sel1) # here it makes no difference whether pid or hid are used since there was only one person interviewed in the household
pop.type1 <- data.frame(bild=names(bild), Freq=as.numeric(bild))
postSt1 <- postStratify(ddata1, ~bild, pop.type1, partial=T)
D_sel1$calweights1 <- weights(postSt1)

ddata2 <- svydesign(id=~pid, weights=~calweights1, data=D_sel1) # no miss in empl 2019
pop.type2 <- data.frame(empl2019_HH=names(emp), Freq=as.numeric(emp))
postSt2 <- postStratify(ddata2, ~empl2019_HH, pop.type2, partial=T)
D_sel1$calweights2 <- weights(postSt2)

pop.type3 <- data.frame(ageKidCat=names(ageK), Freq=as.numeric(ageK))
D_sel1$ageKidCat <- ifelse(D_sel1$ageKid %in% c(10:14), 0, ifelse(D_sel1$ageKid %in% NA, NA, 1))
ddata3 <- svydesign(id=~pid, weights=~calweights2, data=D_sel1)
postSt3 <- postStratify(ddata3, ~ageKidCat, pop.type3, partial=T)
D_sel1$calweights3 <- weights(postSt3)

sexK <- c(0.5, 0.5)
sexK <- round(sexK*sum(D_sel$hhrf),0)
names(sexK) <- c("1", "2")
pop.type4 <- data.frame(sexChild=names(sexK), Freq=as.numeric(sexK))
ddata4 <- svydesign(id=~pid, weights=~calweights3, data=D_sel1)
postSt4 <- postStratify(ddata4, ~sexChild, pop.type4, partial=T)
D_sel1$calweights4 <- weights(postSt4)

COV_tr59 <- merge(COV_tr59, D_sel1[, c("pid","calweights4")], by="pid", all.x=TRUE)

bild/sum(D_sel$hhrf)
wc <- questionr::wtd.table(x=COV_tr59$bild, weights=COV_tr59$calweights4, exclude=NULL)
wc/sum(wc) # passt

emp/sum(D_sel$hhrf)
wc <- questionr::wtd.table(x=COV_tr59$empl2019_HH, weights=COV_tr59$calweights4, exclude=NULL)
wc/sum(wc)

ageK/sum(D_sel$hhrf)
COV_tr59$ageKidCat <- ifelse(COV_tr59$ageKid %in% c(10:14), 0, ifelse(COV_tr59$ageKid %in% NA, NA, 1))
wc <- questionr::wtd.table(x=COV_tr59$ageKidCat, weights=COV_tr59$calweights4, exclude=NULL)
wc/sum(wc)

sexK/sum(D_sel$hhrf)
COV_tr59$sexChild <- ifelse(COV_tr59$sexChild %in% 1, 0, ifelse(COV_tr59$sexChild %in% 2, 1, NA))
wc <- questionr::wtd.table(x=COV_tr59$sexChild, weights=COV_tr59$calweights4, exclude=NULL)
wc/sum(wc)

range(COV_tr59$hhrf)
range(COV_tr59$calweights4)

#################################################################
# Descriptives 2020
#################################################################

table(COV_tr24$pcovhaus2, exclude=NULL)
COV_tr24$pcovhaus2[is.na(COV_tr24$pcovhaus2)] <- -1
w <- wtd.table(COV_tr24$pcovhaus2, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr24$pcovhaus2[COV_tr24$pcovhaus2 %in% -1] <- NA
table(COV_tr24$pcovhaus2, exclude=NULL)

w <- wtd.table(COV_tr24$pcovhaus2, weights=COV_tr24$calweights1)
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
cumsum(rw)

table(COV_tr59$pcovhaus2, exclude=NULL)
COV_tr59$pcovhaus2[is.na(COV_tr59$pcovhaus2)] <- -1
w <- wtd.table(COV_tr59$pcovhaus2, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr59$pcovhaus2[COV_tr59$pcovhaus2 %in% -1] <- NA
table(COV_tr59$pcovhaus2, exclude=NULL)

w <- wtd.table(COV_tr59$pcovhaus2, weights=COV_tr59$calweights4)
rw <- round(w$sum.of.weights/sum(w$sum.of.weights),2)
cumsum(rw)

table(COV_tr24$bild, exclude=NULL)
COV_tr24$bild[is.na(COV_tr24$bild)] <- -1
w <- wtd.table(COV_tr24$bild, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr24$bild[COV_tr24$bild %in% -1] <- NA
table(COV_tr24$bild, exclude=NULL)

table(COV_tr59$bild, exclude=NULL)
COV_tr59$bild[is.na(COV_tr59$bild)] <- -1
w <- wtd.table(COV_tr59$bild, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr59$bild[COV_tr59$bild %in% -1] <- NA
table(COV_tr59$bild, exclude=NULL)

table(COV_tr24$empl2019_HH, exclude=NULL)
COV_tr24$empl2019_HH[is.na(COV_tr24$empl2019_HH)] <- -1
w <- wtd.table(COV_tr24$empl2019_HH, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr24$empl2019_HH[COV_tr24$empl2019_HH %in% -1] <- NA
table(COV_tr24$empl2019_HH, exclude=NULL)

table(COV_tr59$empl2019_HH, exclude=NULL)
COV_tr59$empl2019_HH[is.na(COV_tr59$empl2019_HH)] <- -1
w <- wtd.table(COV_tr59$empl2019_HH, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr59$empl2019_HH[COV_tr59$empl2019_HH %in% -1] <- NA
table(COV_tr59$empl2019_HH, exclude=NULL)

ageKidCat <- ifelse(COV_tr24$ageKid %in% c(10:14), 1, ifelse(COV_tr24$ageKid %in% c(15:18),0,NA))
table(ageKidCat, exclude=NULL)
w <- wtd.table(ageKidCat, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

ageKidCat <- ifelse(COV_tr59$ageKid %in% c(10:14), 1, ifelse(COV_tr59$ageKid %in% c(15:18),0,NA))
table(ageKidCat, exclude=NULL)
w <- wtd.table(ageKidCat, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(COV_tr24$sexChild, exclude=NULL)
w <- wtd.table(COV_tr24$sexChild, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(COV_tr59$sexChild, exclude=NULL)
w <- wtd.table(COV_tr59$sexChild, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(COV_tr24$GY, exclude=NULL)
COV_tr24$GY <- as.numeric(as.character(COV_tr24$GY))
COV_tr24$GY[is.na(COV_tr24$GY)] <- -1
w <- wtd.table(COV_tr24$GY, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
table(COV_tr24$GY)/nrow(COV_tr24)
COV_tr24$GY[COV_tr24$GY %in% -1] <- NA
table(COV_tr24$GY, exclude=NULL)

table(COV_tr59$GY, exclude=NULL)
COV_tr59$GY <- as.numeric(as.character(COV_tr59$GY))
COV_tr59$GY[is.na(COV_tr59$GY)] <- -1
w <- wtd.table(COV_tr59$GY, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
table(COV_tr59$GY)/nrow(COV_tr59)
COV_tr59$GY[COV_tr59$GY %in% -1] <- NA
table(COV_tr59$GY, exclude=NULL)

table(COV_tr24$perf, exclude=NULL)
COV_tr24$perf <- as.numeric(as.character(COV_tr24$perf))
COV_tr24$perf[is.na(COV_tr24$perf)] <- -1
w <- wtd.table(COV_tr24$perf, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr24$perf[COV_tr24$perf %in% -1] <- NA
table(COV_tr24$perf, exclude=NULL)

table(COV_tr59$perf, exclude=NULL)
COV_tr59$perf <- as.numeric(as.character(COV_tr59$perf))
COV_tr59$perf[is.na(COV_tr59$perf)] <- -1
w <- wtd.table(COV_tr59$perf, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)
COV_tr59$perf[COV_tr59$perf %in% -1] <- NA
table(COV_tr59$perf, exclude=NULL)

table(COV_tr24$Digital, exclude=NULL)
w <- wtd.table(COV_tr24$Digital, weights=COV_tr24$calweights1)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

table(COV_tr24$mehrereKanäle, exclude=NULL)
w <- wtd.table(COV_tr24$mehrereKanäle, weights=COV_tr24$calweights1)

someSchool <- ifelse(COV_tr59$tageStSchule %in% 1 | COV_tr59$regSchule %in% 1,1,0)
table(someSchool, exclude=NULL)
w <- wtd.table(someSchool, weights=COV_tr59$calweights4)
round(w$sum.of.weights/sum(w$sum.of.weights),2)

