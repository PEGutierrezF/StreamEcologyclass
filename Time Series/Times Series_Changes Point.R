



# ---------------------------------------------
# Times series analysis 
# 28 Apr 2021
# Pablo E. Gutiérrez-Fonseca
# ---------------------------------------------
#  


# https://pdfs.semanticscholar.org/4f19/a6383e509feef7b3a1cc8bd45dbb8af6669a.pdf?_ga=2.114563975.1617510201.1619639788-390492459.1618174637

install.packages("trend")
install.packages("Kendall")
install.packages("changepoint")

library('Kendall') 
library("trend")
library("changepoint")


rain = read.csv("Precipitation.csv")
rainEVFS <- rain[,3]
  
rain.ts <- ts(rainEVFS, # Convert "Precipitation" to a time series object.
                    frequency=12, start=c(1975,1), end=c(2016,12))
head(rain.ts, n=24) 

plot(rain.ts, main="Precipitation El Verde")


# Is there a trend? -------------------------------------------------------
# We can use the Mann-Kendall Trend Test to see if there is a pattern in the data.

MannKendall(rain.ts)

###  tau is a measure of the strength of the trend
###  p-value for trend is also shown

# The test statistic is 0.0301, and the two-sided p-value associated with it is not 
# less than 0.05. We cannot reject the null hypothesis of the test and conclude that 
# no trend exists in the data because the p-value is greater than 0.05.



# Plot and trend ----------------------------------------------------------
# Now we can add a smooth line to visualize the trend
plot(rain.ts, main="Precipitation El Verde")
lines(lowess(time(rain.ts), rain.ts), col='red')




# Sen's slope -------------------------------------------------------------
 #The sens.slope function in the trend package is used with a time series object.  
# The sea.sens.slope function performs the test while taking into account the 
# seasonality of the data.
sens.slope(rain.ts)


# Pettitt's test for change in value --------------------------------------
# The pettitt.test function in the trend package identifies a point at which 
# the values in the data change.  The results here suggest that the stage values 
# were higher after May 2009 than before.
pettitt.test(rain.ts)
rain[244,]


### Multiple points
#The next few commands will be used to find a potential change point in the mean. 
mvalueP = cpt.mean(rain.ts, method="PELT") 
cpts(mvalueP)
plot(mvalueP)

mvalueA = cpt.mean(rain.ts, method="AMOC") #AMOC, at most one change, 
cpts(mvalueA)
plot(mvalueA)

mvalueS = cpt.mean(rain.ts, method="BinSeg", Q=5) # BinSeg Binary Segmentation
cpts(mvalueS)
rain[c(7,189,255,420,468),]
plot(mvalueS, cpt.width = 4)


plot(mvalueS,type='l',cpt.col='red',xlab='Days',
     ylab='Rainfall (mm)',cpt.width=2, main="Rainfall at El Verde FS")
abline(v=1994, lty=2)
abline(v=2015,lty=2)





if(!require(devtools)) install.packages("devtools")
devtools::install_github("sinhrks/ggfortify")
library("ggfortify")
autoplot(cpt.mean(Precipitation, method="BinSeg"))




install.packages("forecast")
library('forecast')
model <- auto.arima(rain.ts)
f1 <- forecast::stlf(rain.ts, method = "naive")
plot(f1, main = "Monthly Precipitation with Forecasting through 2022",
     ylab = "Rainfall (mm)")


f <- forecast(model, 48)
library(ggplot2)
autoplot(f)



# La Selva, Carapa streams ---------------------------------------------------------

carapa.rich=read.csv("carapa.csv")
carapa.richness <- carapa.rich[,1]

carapa.ts <- ts(carapa.richness, # Convert "Precipitation" to a time series object.
              frequency=12, start=c(1997,1))
head(carapa.ts, n=24) 


library(remotes)
install_github("cran/wq")
library(wq)
MannKendall(carapa.ts)
mannKen(carapa.ts, type = c("slope", "relative"))

carapa.richness1 <- na.omit(carapa.richness) 
sens.slope(carapa.richness1)

func <- function(x,na.rm=T) {
  if(sum(is.na(x)) == length(x)) return(NA)
  if(na.rm) x <- x[!is.na(x)]
  unlist(sens.slope(x)) 
}

func(carapa.ts)
