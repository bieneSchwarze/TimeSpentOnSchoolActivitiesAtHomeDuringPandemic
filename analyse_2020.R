#########################################################
#########################################################
# R Code to Manuscript
#
# Time spent on school-related activities at home during the pandemic:
# A longitudinal analysis of social group inequality 
# among secondary school students
#
# Data analysis for 2020 data 
#
# SZ, 19.10.2020
#########################################################
#########################################################

rm(list=ls())
library(ggplot2)
library(questionr)
library(Hmisc)
library(plyr)
library(car)
library(VGAM)
library(mice)
library(survey)
library(jtools)

#################################################################
# Load Data
#################################################################
# run first prepareData_2020.R

#################################################################
# Missing Values and Imputation
#################################################################
# ---------------------------------------------------------------
# TRANCHE 2-4
# ---------------------------------------------------------------
COV_tr24 <- COV_tr24[,!(colnames(COV_tr24) %in% c("ferien", "ageKidCat", "pid2021", "pcovhaus2021"))]
M <- md.pattern(COV_tr24, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr24),2) # up to 25% missing values
table(complete.cases(COV_tr24)) # ~54% with complete cases
fact <- c("vorSchulschließung", "Konferenzschaltung", "Nichts", "Anderes", "Digital", "bula_cov",
          "psex", "bild", "empl2020", "empl2019_HH", "systemrelevant_nace", "migParents",
          "perf", "GY", "singP", "mehrereKanäle", "sexChild", "pcovhaus2")
num <- c("ppgeb", "ageKid", "numKids", "tranche", "hhrf", "phrf_cati", "calweights1")
for(f in fact){
  COV_tr24[,colnames(COV_tr24) %in% f] <- as.factor(COV_tr24[,colnames(COV_tr24) %in% f])
}
for(n in num){
  COV_tr24[,colnames(COV_tr24) %in% n] <- as.numeric(COV_tr24[,colnames(COV_tr24) %in% n])
}
options(warn=0)
predM <- mice::make.predictorMatrix(data=COV_tr24)
predM[,1:2] <- 0
predM[1:2,] <- 0
impM <- mice::make.method(data=COV_tr24)
impM[!(impM =="")] <- "cart"
imp_tr24 <- mice::mice(COV_tr24, m=20, predictorMatrix=predM, method=impM, maxit=20,seed=581)
imp_tr24$loggedEvents # ok

# ---------------------------------------------------------------
# TRANCHE 5-9
# ---------------------------------------------------------------
names(COV_tr59)
COV_tr59 <- COV_tr59[,!colnames(COV_tr59) %in% c("vorSchulschließung", "Konferenzschaltung",
                                                 "Nichts", "Anderes", "Digital", "ferien", "mehrereKanäle",
                                                 "pid2021", "pcovhaus2021")]
M <- md.pattern(COV_tr59, plot=FALSE)
round(M[nrow(M),]/nrow(COV_tr59),2) # up to 30% missing values
table(complete.cases(COV_tr59)) # ~50% with complete cases
fact <- c("regSchule", "tageStSchule", "bula_cov",
          "psex", "bild", "empl2020", "empl2019_HH", "systemrelevant_nace", "migParents",
          "perf", "GY", "transf", "singP", "pcovhaus2")
num <- c("ppgeb", "ageKid", "numKids", "phrf_cati", "calweights4", "hhrf", "tranche")
for(f in fact){
  COV_tr59[,colnames(COV_tr59) %in% f] <- as.factor(COV_tr59[,colnames(COV_tr59) %in% f])
}
for(n in num){
  COV_tr59[,colnames(COV_tr59) %in% n] <- as.numeric(COV_tr59[,colnames(COV_tr59) %in% n])
}

options(warn=0)
predM <- mice::make.predictorMatrix(data=COV_tr59)
predM[,1:2] <- 0
predM[1:2,] <- 0
impM <- mice::make.method(data=COV_tr59)
impM[!(impM =="")] <- "cart"
imp_tr59 <- mice::mice(COV_tr59, m=20, predictorMatrix=predM, method=impM, maxit=20,seed=2547)
imp_tr59$loggedEvents # ok: non-employed are not sys. relevant & in bula `10' only one household (without migback)

#################################################################
# AME for logit and weighted data
#################################################################

# ---------------------------------------------------------------
# Functions
# taken from https://stackoverflow.com/questions/26468360/how-can-i-generate-marginal-effects-for-a-logit-model-when-using-survey-weights
# ---------------------------------------------------------------

# AME for weighted probit
probitMfxEstSurv <- function(formula, 
                             design, 
                             atmean = TRUE, 
                             robust = FALSE, 
                             clustervar1 = NULL, 
                             clustervar2 = NULL, 
                             start = NULL){
    
    if(is.null(formula)){
      stop("model formula is missing")
    }
    
    for( i in 1:length(class(design))){
      if(!((class(design)[i] %in% "survey.design2") | (class(design)[i] %in% "survey.design"))){
        stop("design arguement must contain survey object")
      }
    }
    
    # fit the probit regression
    fit = svyglm(formula, 
                 design=design, 
                 family = quasibinomial(link = "probit"), 
                 x=T
    )

    # terms needed
    x1 = model.matrix(fit)
    if (any(alias <- is.na(coef(fit)))) {   # this conditional removes any vars with a NA coefficient
      x1 <- x1[, !alias, drop = FALSE]
    }
    
    xm = as.matrix(svymean(x1,design)) # calculate means of x variables
    be = as.matrix(na.omit(coef(fit))) # collect coefficients: be as in beta
    k1 = length(na.omit(coef(fit))) # collect number of coefficients or x variables
    xb = t(xm) %*% be # get the matrix product of xMean and beta, which is the model prediction at the mean
    fxb = ifelse(atmean==TRUE, dnorm(xb), mean(dnorm(x1 %*% be))) # collect either the overall predicted mean, or the average of every observation's predictions
    
    # get variances
    vcv = vcov(fit)
    
    # set mfx equal to predicted mean (or other value) multiplied by beta
    mfx = data.frame(mfx=fxb*be, se=NA)
    
    # get standard errors
    if(atmean){#    fxb *  id matrix - avg model prediction * (beta X xmean)
      gr = as.numeric(fxb)*(diag(k1) - as.numeric(xb) *(be %*% t(xm)))
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))            
    } else {
      gr = apply(x1, 1, function(x){
        as.numeric(as.numeric(dnorm(x %*% be))*(diag(k1) - as.numeric(x %*% be)*(be %*% t(x))))
      })
      gr = matrix(apply(gr,1,mean),nrow=k1)
      mfx$se = sqrt(diag(gr %*% vcv %*% t(gr)))                
    }
    
    # pick out constant and remove from mfx table
    temp1 = apply(x1,2,function(x)length(table(x))==1)
    const = names(temp1[temp1==TRUE])
    mfx = mfx[row.names(mfx)!=const,]
    
    # pick out discrete change variables
    temp1 = apply(x1,2,function(x)length(table(x))==2)
    disch = names(temp1[temp1==TRUE])
    
    # calculate the disctrete change marginal effects and standard errors
    if(length(disch)!=0){
      for(i in 1:length(disch)){
        if(atmean){
          disx0 = disx1 = xm
          disx1[disch[i],] = max(x1[,disch[i]])
          disx0[disch[i],] = min(x1[,disch[i]])
          # mfx equal to    prediction @ x=1     minus prediction @ x=0
          mfx[disch[i],1] = pnorm(t(be) %*% disx1) - pnorm(t(be) %*% disx0)
          # standard errors
          gr = dnorm(t(be) %*% disx1) %*% t(disx1) - dnorm(t(be) %*% disx0) %*% t(disx0)
          mfx[disch[i],2] = sqrt(gr %*% vcv %*% t(gr))
        } else {
          disx0 = disx1 = x1
          disx1[,disch[i]] = max(x1[,disch[i]])
          disx0[,disch[i]] = min(x1[,disch[i]])  
          mfx[disch[i],1] = mean(pnorm(disx1 %*% be) - pnorm(disx0 %*% be))
          # standard errors
          gr = as.numeric(dnorm(disx1 %*% be)) * disx1 - as.numeric(dnorm(disx0 %*% be)) * disx0
          avegr = as.matrix(colMeans(gr))
          mfx[disch[i],2] = sqrt(t(avegr) %*% vcv %*% avegr)
        }
      }
    } 
    mfx$discretechgvar = ifelse(rownames(mfx) %in% disch, 1, 0)
    output = list(fit=fit, mfx=mfx)
    return(output)
}

probitMfxSurv <- function( formula, 
                           design, 
                           atmean = TRUE, 
                           robust = FALSE, 
                           clustervar1 = NULL, 
                           clustervar2 = NULL, 
                           start = NULL ) {
    res = probitMfxEstSurv(formula, design, atmean, robust, clustervar1, clustervar2, start)
    
    est = NULL
    est$mfxest = cbind(dFdx = res$mfx$mfx,
                       StdErr = res$mfx$se,
                       z.value = res$mfx$mfx/res$mfx$se,
                       p.value = 2*pt(-abs(res$mfx$mfx/res$mfx$se), df = Inf))
    colnames(est$mfxest) = c("dF/dx","Std. Err.","z","P>|z|")
    rownames(est$mfxest) =  rownames(res$mfx)
    
    est$fit = res$fit
    est$dcvar = rownames(res$mfx[res$mfx$discretechgvar==1,])  
    est$call = match.call() 
    class(est) = "probitmfx"
    est
}


# Pool function for AME for imputed data
poolAME <- function(modList, n){
  m <- length(modList)
  ame <- NULL
  vars <- NULL
  for(i in 1:m){
    mi <- modList[[i]]$mfxest
    ame <- cbind(ame, as.numeric(mi[,1]))
    vars <- cbind(vars, as.numeric(mi[,2]^2)) 
  }
  rownames(ame) <- rownames(modList[[1]]$mfxest)
  rownames(vars) <- rownames(modList[[1]]$mfxest)
  amePool <- apply(ame, 1, mean)
  varWithin <- apply(vars, 1, mean)
  varBetween <- apply((ame - amePool)^2, 1, sum)/(n-1)
  varTotal <- varWithin + varBetween + varBetween/m
  seTotal <- sqrt(varTotal)
  lambda <- (varBetween + (varBetween / m))/(varTotal)
  dfold <- (m-1)/lambda^2
  k <- length(amePool)
  dfobserved <- ((n-k)+1)/((n-k)+3) * (n-k)*(1-lambda) 
  dfadjusted <- (dfold*dfobserved)/(dfold+dfobserved)
  alpha <- 0.1
  td <- qt(1-alpha/2, dfadjusted)
  ci_low <- amePool - td*seTotal
  ci_up <- amePool + td*seTotal
  res <- round(cbind(amePool, ci_low, ci_up),3)
  rownames(res) <- rownames(modList[[1]]$mfxest)
  return(res)
}

#################################################################
# MODEL (Probit regression): RUN THROUGH ALL IMPUTED DATA SETS
#################################################################
# ---------------------------------------------------------------
# Tranches 2-4
# ---------------------------------------------------------------
modListAME_tr24 <- vector(length=imp_tr24$m, mode="list") # collect average partial marginal effects
for(i in 1:imp_tr24$m){
  D_i <- complete(imp_tr24, action=i)
  D_i$ageKid <- as.numeric(as.character(D_i$ageKid))
  D_i$ageKidCat <- ifelse(D_i$ageKid %in% c(10:14), 0, 1)
  D_i$cuttime <- ifelse(D_i$pcovhaus2 %in% c(1,2,3),0,1)
  D_i$bild <- as.factor(D_i$bild)
  D_i$bild <- relevel(D_i$bild, ref="hoch")
  D_i$empl2019_HH_rev <- ifelse(D_i$empl2019_HH %in% c("PT", "VZ"), 0, ifelse(D_i$empl2019_HH %in% "nichtEW", 1, 2))
  D_i$empl2019_HH_rev <- as.factor(D_i$empl2019_HH_rev)
  D_i$sexChild <- as.factor(D_i$sexChild)
  D_i$sexChild <- relevel(D_i$sexChild, ref="1")
  D_i$GY <- as.factor(D_i$GY)
  D_i$GY <- relevel(D_i$GY, ref="1")
  D_i$perf <- as.factor(D_i$perf)
  D_i$perf <- relevel(D_i$perf, ref="1")  
  D_i$numKidsCat <- as.factor(ifelse(D_i$numKids %in% 1, 0, ifelse(D_i$numKids %in% 2, 1, 2)))
  des <- svydesign(ids=~pid, weights=~calweights1, data =D_i)
  ame_i <- probitMfxSurv(formula = cuttime ~ bild +
                           empl2019_HH_rev +
                           ageKidCat +
                           sexChild +
                           #numKidsCat +
                           GY +
                           perf+
                           Digital+
                           mehrereKanäle,
                         design = des)
  modListAME_tr24[[i]] <- ame_i
  fit.probit <- svyglm(formula = cuttime ~ bild +
                                empl2019_HH_rev +
                                ageKidCat +
                                sexChild +
                                GY +
                                perf+
                                Digital+
                                mehrereKanäle,
                              design = des,
                              family=binomial(link= "probit"))
  print(summ(model=fit.probit))
}
n <- nrow(D_i)
ame_tr24 <- poolAME(modListAME_tr24, n)
round(ame_tr24,2)

# ---------------------------------------------------------------
# Tranches 5-9
# ---------------------------------------------------------------
modListAME_tr59 <- vector(length=imp_tr59$m, mode="list") # collect average partial marginal effects
for(i in 1:imp_tr59$m){
  #i <- 1
  D_i <- complete(imp_tr59, action=i)
  D_i$ageKid <- as.numeric(as.character(D_i$ageKid))
  D_i$ageKidCat <- ifelse(D_i$ageKid %in% c(10:14), 0, 1)
  D_i$cuttime <- ifelse(D_i$pcovhaus2 %in% c(1,2,3),0,1)
  D_i$bild <- as.factor(D_i$bild)
  D_i$bild <- relevel(D_i$bild, ref="hoch")
  D_i$empl2019_HH_rev <- ifelse(D_i$empl2019_HH %in% c("PT", "VZ"), 0, ifelse(D_i$empl2019_HH %in% "nichtEW", 1, 2))
  D_i$empl2019_HH_rev <- as.factor(D_i$empl2019_HH_rev)
  D_i$sexChild <- as.factor(D_i$sexChild)
  D_i$sexChild <- relevel(D_i$sexChild, ref="1")
  D_i$GY <- as.factor(D_i$GY)
  D_i$GY <- relevel(D_i$GY, ref="1")
  D_i$perf <- as.factor(D_i$perf)
  D_i$perf <- relevel(D_i$perf, ref="1")  
  D_i$schule <- ifelse(D_i$regSchule %in% 1 | D_i$tageStSchule %in% 1, 1,0)
  D_i$numKidsCat <- as.factor(ifelse(D_i$numKids %in% 1, 0, ifelse(D_i$numKids %in% 2, 1, 2)))
  des <- svydesign(ids=~pid, weights=~calweights4, data =D_i)
  ame_i <- probitMfxSurv(formula = cuttime ~ bild +
                           empl2019_HH_rev +
                           ageKidCat +
                           sexChild +
                           #numKidsCat +
                           GY +
                           perf +
                           schule,
                         design = des)
  modListAME_tr59[[i]] <- ame_i
  fit.probit <- svyglm(formula = cuttime ~ bild +
                         empl2019_HH_rev +
                         ageKidCat +
                         sexChild +
                         GY +
                         perf +
                         schule,
                       design = des,
                       family=binomial(link= "probit"))
  print(summ(model=fit.probit))
}
n <- nrow(D_i)
ame_tr59 <- poolAME(modListAME_tr59, n)
round(ame_tr59,2)

