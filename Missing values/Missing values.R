



# ---------------------------------------------
# Nonparametric Missing Value Imputation Using Random Forest
# 14 Apr 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  
  
rm(list=ls(all=TRUE)) #give R a blank slate

# data<-read.table(file.choose(),header=T)
# setwd("D:/Curriculum/07_ Cursos/2019 Stream Ecology/Practicas/02 MissingData")

# missforest  uses a random forest trained on the observed values of
# a data matrix to predict the missing values.

# It can be used to impute continuous and/or categorical data including complex
# interactions and non-linear relations. 

# It yields an out-of-bag (OOB) imputation error estimate without the need of 
# a test set or elaborate cross-validation.

install.packages('missForest')
library(missForest)


discharge.na = read.csv("Discharge.csv")
head(discharge.na)
summary(discharge.na)

plot(discharge.na$Discharge,type="l", lwd=2.0)


# ntree number of trees to grow in each forest.
# maxiter maximum number of iterations to be performed given the stopping criterion is
# not met beforehand.
discharge.imp <- missForest(discharge.na, maxiter = 4, ntree = 100,
                           variablewise = FALSE, decreasing = FALSE, verbose = F, replace = TRUE,
                           classwt = NULL, cutoff = NULL, strata = NULL,
                           sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                           xtrue = NA, parallelize = "no")

discharge.missf <- discharge.imp$ximp
summary(discharge.missf)
plot(discharge.missf$Discharge,type="l", lwd=2.0)


par(mfrow=c(1,3))
plot(discharge.na$Discharge,type="l",main="Original Discharge", lwd=2.0)
abline(h=0.0154, lty=2, lwd=4, col='green')
plot(discharge.missf$Discharge,type="l",main="Imputated Discharge", lwd=2.0)
abline(h=0.0154, lty=2, lwd=4, col='red')
plot(discharge.na$Rain,type="l",main="Rain", lwd=2.0)
abline(h=10.1, lty=2, lwd=4, col='blue')


# Out Of Bag (OOBerror)= imputation error estimate (normalized root mean squared error)
# The OOB observations can be used for example for estimating the prediction error of RF.

# The performance is assessed using the normalized root mean squared error (NRMSE)
# Good performance leads to a value close to 0 and bad performance to a value around 1.
NRMSE <- discharge.imp$OOBerror
NRMSE

# How big are the errors of the imputation?
# Not too bad, around >1%. NRMSE is The normalized root mean squared error, it is defined as:
# The Normalized Root Mean Square Error (NRMSE) the RMSE facilitates the comparison between models with different scales. 

#  Mean squared error (MSE)     
by_variable_error <- missForest(discharge.na, variablewise = TRUE)
by_variable_error$OOBerror

mean(discharge.na$Discharge,na.rm=TRUE)
(0.01560845 * sqrt(NRMSE))




#########################
#		Hmisc
# Harrell Miscellaneous
#########################

rm(list=ls(all=TRUE)) #give R a blank slate


# Using the argImpute function, Hmisc # performs multiple imputation using bootstraping 
# and predictive mean matching. 

# install.packages("Hmisc")
library("Hmisc")

discharge.na=read.csv("Discharge.csv")
head(discharge.na)

summary(discharge.na)
plot(discharge.na$Discharge,type="l")

paste('How many missing values?', ' ', length(which(is.na(discharge.na))))

# formula: The ~ Sepal.Length + . argument indicates what formula to use. In this case, we want all variables' missing data to be imputed, so we add each one.
# data: the data frame with missing data.
# n.impute: number of multiple imputations. 5 is frequently used, but 10 or more doesn't hurt
set.seed(1)
impute_arg <- aregImpute(Discharge ~ Rain , data = discharge.na, n.impute=100)

print(impute_arg, digits=3)
impute_arg$imputed$Discharge

# The R square values indicate the liklihood that the predicted missing values in the 'irt' 
# dataframe match what we would actually observe if participants had answered all questions. 
# Higher values are better.

# To use one of the iterations in our original data set, we can use the transcend function:
completetable <- impute.transcan(impute_arg, imputation=4, data=discharge.na, 
                                 list.out=TRUE,pr=FALSE, check=FALSE)

head(completetable)
plot(completetable$Discharge,type="l")


f <-  fit.mult.impute(Discharge ~ Rain , lm, impute_arg, 
                      data=discharge.na)
summary(f)
# aregImpute	Multiple imputation based on additive regression,
# bootstrapping, and predictive mean matching


# Compare methods 
windows()
par(mfrow=c(1,2))
plot(completetable$Discharge,type="l",main="HMisc")
abline(h=0.0154, lty=2)
plot(discharge.missf$Discharge,type="l",main="Missforest")
abline(h=0.0154, lty=2)



. <- with(discharge.na, impute(Discharge, mean))# 'random'
plot(.,type="l")


#########################
#		Amelia
# Amelia Earhart
#########################

rm(list=ls(all=TRUE)) #give R a blank slate

library(Amelia)



discharge.na=read.csv("Discharge.csv")
head(discharge.na)

missmap(discharge.na)
a <- amelia(discharge.na, ts = "Day", idvars="Rain", polytime = 2)
summary(a)

# m=3 number of imputed data sets we need.by default m=5
# ts- attribute that is time series class
# idvars - the attributes we need to retain but not used in 
# missing data imputation models are mentioned here.

b <- a$imputations[[1]]$Discharge
plot(b)


Reference

Stekhoven, D. J., & Bühlmann, P. (2012). MissForest-non-parametric missing value imputation for mixed-type data. Bioinformatics, 28(1), 112-118.

https://medium.com/coinmonks/dealing-with-missing-data-using-r-3ae428da2d17

https://stat.ethz.ch/education/semesters/ss2012/ams/paper/missForest_1.2.pdf
https://stats.stackexchange.com/questions/296060/how-to-interpret-ooberror-while-doing-data-imputation-with-missforest
