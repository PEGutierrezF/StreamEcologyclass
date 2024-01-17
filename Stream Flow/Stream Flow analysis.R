



# ---------------------------------------------
# Streamflow analysis (NR-280, Stream Ecology course)
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
end.date = "2023-12-31"
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



# Frequency of events -----------------------------------------------------
hist(winooski.df$Q.ft.s)

# More details ------------------------------------------------------------
# Analysis 1936 alone
year.1936 <- setDT(winooski)[Date %between% c('1936-01-01', '1936-12-31')]
# Analysis whole years except 1936
not.1936   <- setDT(winooski)[(Date < '1936-01-01' | Date > '1936-12-31'),]

summary(year.1936)
summary(not.1936)


# Cumulative analysis  ---------------------------------------------------
cum.data <- addWaterYear(winooski) # This add a new column with the year

cumulative_dat <- group_by(cum.data, waterYear) %>% # Group by year
  mutate(cumulative_dis = cumsum(Q.ft.s), # Sum discharge
         wy_doy = seq(1:n())) # Add consecutive number


# Minimum value -----------------------------------------------------------
df <- cumulative_dat %>% # The previous dataframe
  group_by(waterYear) %>% # Group by waterYear
  summarize(mean = mean(Q.ft.s), # Give me the mean
            sum = sum(Q.ft.s)) # Give me the sum
min(df$sum[df$sum != min(df$sum)]) # Identify the min value in 'sum' column
df %>% filter_all(any_vars(. %in% c(303659))) # Identify the entire row with the lowest value


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
q + geom_line(data=subset(cumulative_dat, waterYear == "2023"), colour="red", size=0.9) 


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

# Combine into one dataset
combined_years <- rbind(
  data.frame(year = 1936, year.1936),
  data.frame(year = 1965, year.1965),
  data.frame(year = 2011, year.2011))

combined_years <- combined_years %>%
  group_by(year) %>%
  mutate(rank = rank(-Q.ft.s)) %>% 
  mutate(P = 100 * (rank / (length(Q.ft.s) + 1)))

ggplot(combined_years, aes(x = P, y = Q.ft.s, color = as.factor(year))) +
  geom_line() +
  scale_y_log10() +
  xlab("% Time flow equalled or exceeded") +
  ylab("Q (cfs)")


References

https://vt-hydroinformatics.github.io/fdcs.html



#Flow is negative in rank() to make 
#high flows ranked low (#1)
winooski.df <- winooski.df %>%
  mutate(rank = rank(-Q.ft.s)) %>%
  mutate(P = 100 * (rank / (length(Q.ft.s) + 1)))


winooski.df %>% ggplot(aes(x = P, y = Q.ft.s))+
  geom_line()+
  scale_y_log10()+
  xlab("% Time flow equalled or exceeded")+
  ylab("Q (cfs)")



https://vt-hydroinformatics.github.io/fdcs.html

siteid <- "09521000"
startDate <- "1905-10-01"
endDate <- "1965-10-01"
parameter <- "00060"

WS <- readNWISdv(siteid, parameter, startDate, endDate) %>% 
  renameNWISColumns() %>%
  mutate(year = year(Date)) %>%
  mutate(period = case_when( year <= 1936 ~ "Pre Dam",
                             year > 1936  ~ "Post Dam")) %>%
  group_by(period) %>%
  mutate(rank = rank(-Flow)) %>% 
  mutate(P = 100 * (rank / (length(Flow) + 1)))

flow <- ggplot(WS, aes(Date, Flow))+#, color = period))+
  geom_line()+
  ylab("Q (cfs)")

fdc <- WS %>% ggplot(aes(x = P, y = Flow, color = period))+
  geom_line()+
  #scale_y_log10()+
  xlab("% Time flow equalled or exceeded")+
  ylab("Q (cfs)")


