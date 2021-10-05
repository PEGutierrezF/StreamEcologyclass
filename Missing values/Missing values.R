

# https://stat.ethz.ch/education/semesters/ss2012/ams/paper/missForest_1.2.pdf

# ---------------------------------------------
# This is a practice for Missing value
# 14 Apr 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  
  
rm(list=ls(all=TRUE)) #give R a blank slate

data<-read.table(file.choose(),header=T)

install.packages('missForest')
library(missForest)

#setwd("D:/Curriculum/07_ Cursos/2019 Stream Ecology/Practicas/02 MissingData")


Discharge=read.csv("Missing values/Discharge.csv")
Discharge
head(Discharge)

summary(Discharge)
plot(Discharge$Discharge,type="l")

Discharge.imp<- missForest(Discharge, maxiter = 4, ntree = 100,
                           variablewise = TRUE, decreasing = FALSE, verbose = F, replace = TRUE,
                           classwt = NULL, cutoff = NULL, strata = NULL,
                           sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                           xtrue = NA, parallelize = "no")

DischargeWM <- Discharge.imp$ximp
summary(DischargeWM)
plot(DischargeWM$Discharge,type="l")


windows()
par(mfrow=c(1,3))
plot(Discharge$Discharge,type="l",main="Original Discharge")
abline(h=0.0154, lty=2)
plot(DischargeWM$Discharge,type="l",main="Imputated Discharge")
abline(h=0.0154, lty=2)
plot(Discharge$Rain,type="l",main="Rain")


#OOB Out Of Bag= imputation error estimate (normalized root mean squared error)
# The OOB observations can be used for example for estimating the prediction error of RF

Discharge.imp$OOBerror




#########################
#		Hmisc
# Harrell Miscellaneous
#########################

install.packages("Hmisc")
library("Hmisc")

Discharge.frm=read.csv("Missing values/Discharge.csv")
Discharge.frm
head(Discharge.frm)

summary(Discharge.frm)
plot(Discharge.frm$Discharge,type="l")

paste('How many missing values?', ' ', length(which(is.na(Discharge.frm))))

impute_arg <-aregImpute(~ Rain + Discharge, data = Discharge.frm, n.impute=100, nk=4, match='closest',
                        plotTrans=FALSE,)

impute_arg
impute_arg$imputed$Discharge

print(impute_arg, digits=3)
plot(impute_arg$Discharge)

completetable <- impute.transcan(impute_arg, imputation=1, data=Discharge.frm, 
                                 list.out=TRUE,pr=FALSE, check=FALSE)
head(completetable)
plot(completetable$Discharge,type="l")

# aregImpute	Multiple imputation based on additive regression,
# bootstrapping, and predictive mean matching


# Compare methods 
windows()
par(mfrow=c(1,2))
plot(completetable$Discharge,type="l",main="HMisc")
abline(h=0.0154, lty=2)
plot(DischargeWM$Discharge,type="l",main="Missfores")
abline(h=0.0154, lty=2)




#########################
#		Amelia
# Amelia Earhart
#########################

install.packages("Amelia")
library(Amelia)

missmap(Discharge.frm)

Completed_data<-amelia(Discharge.frm, idvars="Rain")
summary(Completed_data)
plot(Completed_data)
