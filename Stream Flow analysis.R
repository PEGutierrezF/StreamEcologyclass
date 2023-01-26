








install.packages("dataRetrieval", 
                 repos=c("http://owi.usgs.gov/R",
                         getOption("repos")))
install.packages("RBIcalc")

install.packages('waterData')
library(zoo)
library(raster)
library(rgdal)
library(hydroTSM)
library(dataRetrieval)  # USGS package that gets streamflow data direct from the USGS website
library(waterData)
library(ggplot2)
library(data.table)



# Data minning ------------------------------------------------------------

# Try the code below with the site.code here, then use the site code for your watershed.
site.code = "04290500"  #  The USGS streamgage code for Winnoski River at Esses

readNWISsite(site.code)  # Note:  doing readNWISsite("04290500") gives the same result.
what.data = whatNWISdata(siteNumber = site.code)
what.data[1:10,]  # just look a first 10 records



# Data manipulation -------------------------------------------------------
parameter.code = "00060"  # this is the code for stream discharge.
start.date = ""  # Blanks get all of the data
end.date = ""
#  Use your site code in the line below:
winnoski = readNWISdv("04290500", parameter.code,start.date,end.date)
head(winnoski)


# Changes names -----------------------------------------------------------
# The names for the discharge and QA columns aren't very nice, so rename them:
names(winnoski)[c(4,5)] = c("Q.ft.s","QA")
head(winnoski)



# Explore data ------------------------------------------------------------
plot(winnoski$Date,winnoski$Q.ft.s, type='l')
summary(winnoski)
winnoski[which.max(winnoski$Q.ft.s), ]
winnoski[which.min(winnoski$Q.ft.s), ]


# Plot --------------------------------------------------------------------

winnoski.df <- winnoski %>%
  mutate(year = factor(year(Date)),     # use year to define separate curves
         Date = update(Date, year = 1))  # use a constant year for the x-axis


p <-  ggplot(winnoski.df, aes(Date, Q.ft.s, color = year)) +
            scale_x_date(date_breaks = "1 month", date_labels = "%b")
p +  geom_line() + theme(legend.position = "none") 

p +   geom_line(aes(group = year), color = "gray", alpha = 0.1) +
            geom_line(data = function(x) filter(x, year == 1936), size = 1)



# More details ------------------------------------------------------------
year.1936 <- setDT(winnoski)[Date %between% c('1936-01-01', '1936-12-31')]
no.2023   <- setDT(winnoski)[(Date < '1936-01-01' | Date > '1936-12-31'),]

summary(data.1936)
summary(no.2023)
