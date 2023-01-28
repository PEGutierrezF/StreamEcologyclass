



# ---------------------------------------------
# Streamflow analysis (Stream Ecology course)
# 26 Jan 2023
# Pablo E. Gutiérrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# ---------------------------------------------
#  



rm(list=ls(all=TRUE)) #give R a blank slate

install.packages("dataRetrieval", 
                 repos=c("http://owi.usgs.gov/R",
                         getOption("repos")))
library(zoo)
library(raster)
library(rgdal)
library(hydroTSM)
library(dataRetrieval)  # USGS package that gets streamflow data direct from the USGS website
library(waterData)
library(ggplot2)
library(data.table)
library(lubridate)
library(dplyr)


# Data mining ------------------------------------------------------------
# Try the code below with the site.code here, then use the site code for your watershed.
site.code = "04290500"  #  The USGS streamgage code for Winnoski River Near Essex Junction

readNWISsite(site.code)  # Note:  doing readNWISsite("04290500") gives the same result.
what.data = whatNWISdata(siteNumber = site.code)
what.data[1:10,]  # just look a first 10 records


# Data manipulation -------------------------------------------------------
parameter.code = "00060"  # this is the code for stream discharge.
start.date = "1929-01-01"  # Blanks get all of the data
end.date = "2022-12-31"
#  Use your site code in the line below:
winooski = readNWISdv("04290500", parameter.code, start.date, end.date)
head(winooski)
tail(winooski)

# Changes names -----------------------------------------------------------
# The names for the discharge and QA columns aren't very nice, so rename them:
names(winooski)[c(4,5)] = c("Q.ft.s","QA")
head(winooski)



# Explore data ------------------------------------------------------------
plot(winooski$Date,winooski$Q.ft.s, type='l')
summary(winooski)
winooski[which.max(winooski$Q.ft.s), ] # entire row with max Q.ft.s value
winooski[which.min(winooski$Q.ft.s), ] # entire row with min Q.ft.s value


# Plot --------------------------------------------------------------------
winooski.df <- winooski %>%
  mutate(year = factor(year(Date)),     # use year to define separate curves
         Date = update(Date, year = 1))  # use a constant year for the x-axis
head(winooski.df)



p <-  ggplot(winooski.df, aes(x= Date, y= Q.ft.s, color = year)) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_bw()
p
p +  geom_line() + theme(legend.position = "none") 


p +  geom_line(aes(group = year), color = "gray20", alpha = 0.1) +
            geom_line(data = function(x) filter(x, year == 1936), 
                      color="blue", linewidth = 1) +
  theme_bw()



# More details ------------------------------------------------------------
year.1936 <- setDT(winooski)[Date %between% c('1936-01-01', '1936-12-31')]
not.1936   <- setDT(winooski)[(Date < '1936-01-01' | Date > '1936-12-31'),]

summary(year.1936)
summary(not.1936)


?setDT

# Cumulative analysis  ---------------------------------------------------
cum.data <- addWaterYear(winooski)

cumulative_dat <- group_by(cum.data, waterYear) %>% # Group by year
  mutate(cumulative_dis = cumsum(Q.ft.s), # Sum discharge
         wy_doy = seq(1:n())) # Add consecutive number


# Minimum value -----------------------------------------------------------
df <- cumulative_dat %>% 
  group_by(waterYear) %>% 
  summarize(mean = mean(Q.ft.s),
            sum = sum(Q.ft.s))
min(df$sum[df$sum != min(df$sum)]) 
df %>% filter_all(any_vars(. %in% c(303659)))


q <- ggplot(cumulative_dat, aes(x = wy_doy, y = cumulative_dis, 
                                group = waterYear)) + 
  geom_line(lwd = 0.6, color='gray60') +
  xlab("Julian Day") + ylab("Cumulative dischage (ft^3/s)") +
  ylim(c(0, 1300000)) +
  xlim(0,366) +
  theme_bw() 
q

q + geom_line(data=subset(cumulative_dat, waterYear == "1936"), colour="black", size=0.9) 
q + geom_line(data=subset(cumulative_dat, waterYear == "2011"), colour="blue", size=0.9) 
q + geom_line(data=subset(cumulative_dat, waterYear == "1965"), colour="red", size=0.9) 


# visually compare cumulative discharge across years
ggplot(cumulative_dat, aes(x = wy_doy, y = cumulative_dis, group = waterYear)) +
  geom_line(aes(color = waterYear)) +
  scale_color_viridis_c() +
  scale_x_continuous(breaks = c(1, 93, 184, 275), labels = c("Oct 1", "Jan 1", "Apr 1", "July 1")) +
  theme_bw() +
  labs(color = "Water Year", x = "", y = "Cumulative Discharge")




# Flow duration curves ----------------------------------------------------
# The flow-duration curve is a cumulative frequency curve that shows the percent 
# of time specified discharges were equaled or exceeded during a given period. 
# It combines in one curve the flow characteristics of a stream throughout the 
# range of discharge, without regard to the sequence of occurrence.

year.1936 <- setDT(winooski)[Date %between% c('1936-01-01', '1936-12-31')]
year.1965 <- setDT(winooski)[Date %between% c('1965-01-01', '1965-12-31')]
year.2011 <- setDT(winooski)[Date %between% c('2011-01-01', '2011-12-31')]


year.1936. = fdc(year.1936$Q.ft.s,new=TRUE, ylab="Q ft3/s")
year.1965. = fdc(year.1965$Q.ft.s,new=F,col="red")
year.2011. = fdc(year.2011$Q.ft.s,new=F,col="blue")
legend("topright",c("1936","1965","2011"),col=c("black","red","blue"),lty=c(1,1,1))


print(range(winooski$Q.ft.s))
df.fdc.1936 = fdc(year.1936$Q.ft.s,new=TRUE, ylab="Q ft3/s",
             las=1, cex.lab=1, cex.axis=0.8,yat=c(0.01,0.1,1,1,10,100,1000,3000),
             ylim=c(20,50000))
df.fdc.1965 = fdc(year.1965$Q.ft.s,new=FALSE,col="red", thr.shw=FALSE,
                  xat=NA,yat=NA,ylim=c(20,50000),cex.axis=0.8)
df.fdc.2011 = fdc(year.2011$Q.ft.s,new=FALSE,col="blue", thr.shw=FALSE, 
                  xat=NA,yat=NA,ylim=c(20,50000),cex.axis=0.8)
legend("topright",c("1936","1965","2011"),col=c("black","red","blue"),lty=c(1,1,1))



References

https://vt-hydroinformatics.github.io/fdcs.html

