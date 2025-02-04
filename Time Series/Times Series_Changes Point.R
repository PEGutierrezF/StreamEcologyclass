



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
install.packages('Rbeast')

library('Kendall') 
library("trend")
library("changepoint")
library(Rbeast)

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

# AMOC is best for detecting a single change point.
mvalueA = cpt.mean(rain.ts, method="AMOC") #AMOC, At Most One Change(AMOC) 
cpts(mvalueA)
plot(mvalueA)

# PELT finds multiple change points optimally using dynamic programming.
mvalueP = cpt.mean(rain.ts, method="PELT") 
cpts(mvalueP)
plot(mvalueP,type="l")

# BinSeg also detects multiple change points but uses a greedy, 
#recursive approach, making it faster but sometimes less accurate than PELT.
mvalueS = cpt.mean(rain.ts, method="BinSeg", Q=5) # BinSeg Binary Segmentation
cpts(mvalueS)
rain[c(7,189,255,420,468),]
plot(mvalueS, cpt.width = 4)


plot(mvalueS,type='l',cpt.col='red',xlab='Days',
     ylab='Rainfall (mm)',cpt.width=2, main="Rainfall at El Verde FS")
abline(v=1994, lty=2, lwd=3, col='blue')
abline(v=2015,lty=2, lwd=3, col='red')

https://search.r-project.org/CRAN/refmans/Rbeast/html/beast.html
https://stackoverflow.com/questions/67749982/changepoints-detection-in-time-series-in-r
out=beast(rain.ts, season='none')
plot(out)
print(out)

# Rain La Selva  ----------------------------------------------------------

library(tidyverse)
library(quantmod)
library(xts)
library(emmeans)
library(gganimate)
library(broom)
library(ggplot2)
library("ggpubr")
library(WaveletComp)
library(dplyr)

setwd("D:/Curriculum/14_ Colaboracion/R help/La Selva rain/")
rain=read.csv("LaSelvaDailyrainfall.csv")
summary(rain)


rainLaSelva <- rain %>% select(date,rain) %>%
  filter(!is.na(date)) %>%
  filter(!is.na(rain))

summary(rainLaSelva)
head(rainLaSelva)

rainLaSelva$date<-as.POSIXct(rainLaSelva$date,"%Y-%m-%d",tz = "UTC")


#convert to xts (time series) object !!! assumes records of observations are ordered by time !!!
rainxts <- xts(rainLaSelva[,-1], order.by=rainLaSelva[,1])

# a few functions to check attributes
index(rainxts)
str(rainxts)
tzone(rainxts)
nmonths(rainxts)
nyears(rainxts)



threshold.peak<-62
LSrainpeaks<-findPeaks(rainxts, thresh=threshold.peak)
plot(rainxts[LSrainpeaks-1])


#turn peaks into a dataframe to add it to a ggplot of the raw data
#and calculate metrics
#here we use 20 threshold
peaks<-as.data.frame(rainxts[LSrainpeaks-1])
head(peaks)

#plot peaks onto raw data for precipitation
peaks_graphic <-ggplot(rainLaSelva, aes(x = date, y = rain)) +
  geom_line(colour='blue') +
  labs(x = "Date",
       y = "Precipitation (mm)") +
  geom_point(data=peaks,aes(x = as.POSIXct(row.names(peaks)), y = V1), colour='red')
peaks_graphic








#how many peaks per total obs, which are in units of days (here, obs = 365 days*33 years) ?
peak_per_d <- length(peaks$V1)/length(rainLaSelva$precip)
peak_per_d

#how many peaks per year?
peaks_per_y<-length(peaks$V1)/nyears(rainxts)
peaks_per_y

#average peak magnitude (in mm for precip)
peak_mean<-mean(peaks$V1)
peak_mean

#peak standard deviation
peak_sd<-sd(peaks$V1)

#peak CV
peak_CV<-sd(peaks$V1)/mean(peaks$V1)
peak_CV

#turn peaks into a dataframe to add it to a ggplot of the raw data
#and calculate metrics
#here we use 20 threshold
peaks<-as.data.frame(rainxts[LSrainpeaks-1])

# add year and time columns to peaks dataset
peaks$time<-as.POSIXct(row.names(peaks))
peaks$year<-as.numeric(format(as.POSIXct(row.names(peaks)),"%Y"))


#### peak number vs. time
# get slope of number of peaks per year for each year vs. year (and p-value)
# first, add any missing years that had no peaks (add zeros) - probably a more efficient way to do this...
year<-min(as.numeric(format(as.POSIXct(rainLaSelva$date),"%Y"))):max(as.numeric(format(as.POSIXct(rainLaSelva$date),"%Y")))
years<-as.data.frame(year)
years
peak.sum<-peaks %>% group_by(year) %>% summarise(mean.peak=mean(V1), count=n())
peak.sum
peak.number<-merge(years,peak.sum,by.x="year",by.y="year",all.x=TRUE)
peak.number[is.na(peak.number)] <- 0 
peak.number




# second, build the stats models and save the slope and p as output
peak.number.lm<-lm(count~year,data=peak.number)
#plot(peak.number.lm) # turn this on to check model statistical assumptions
lmsum.number<-summary(peak.number.lm)
peak.number.slope<-peak.number.lm$coefficients[2]
peak.number.slope
peak.number.p<-lmsum.number$coefficients[2,4]
peak.number.p
lmsum.number$r.squared




peaks <- ggplot(peak.number, aes(x = year, y = count)) + 
  geom_point() +
  xlab('Year')+ ylab("Number of peaks (>62mm)") +
  stat_smooth(method = "lm", col = "blue") + 
  
  stat_cor(label.y = 13,
           aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
  stat_regline_equation(label.y = 12) +
  
  theme(axis.title.x = element_text(size = 14, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 14, angle = 90)) + # axis y
  theme(axis.text.x=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=12, vjust=0.5, color="black")) + #subaxis y
  
  theme_classic() 

peaks


