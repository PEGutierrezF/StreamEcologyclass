



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

Precipitation<- scan("Time Series/Precipitation.csv", skip=1)
Precipitation.timeseries <- ts(Precipitation, frequency=12, start=c(1975,1), end=c(2016,12))


plot(Precipitation.timeseries, main="Precipitation El Verde")

Precipitation.timeseries

# Is there a trend? -------------------------------------------------------

mk.test(Precipitation.timeseries)
MannKendall(Precipitation.timeseries)
sens.slope(Precipitation.timeseries)


# Single change point -----------------------------------------------------
pettitt.test(Precipitation.timeseries)




### Multiple points


mvalueP = cpt.mean(Precipitation, method="PELT") 
cpts(mvalueP)
plot(mvalueP)

mvalueA = cpt.mean(Precipitation, method="AMOC") #AMOC, at most one change, 
cpts(mvalueA)
plot(mvalueA)

mvalueS = cpt.mean(Precipitation, method="BinSeg") # BinSeg Binary Segmentation
cpts(mvalueS)
cpts.ts(mvalueS)
plot(mvalueS)

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
model <- auto.arima(Precipitation.timeseries)
f <- forecast(model, 12)
library(ggplot2)
autoplot(f)
