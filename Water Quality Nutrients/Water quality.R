



# --------------------------------------------------------
# 
# Date: Tue Feb 11 2025 11:35:01
# Pablo E. Gutierrez-Fonseca
# pabloe.gutierrezfonseca@gmail.com
# --------------------------------------------------------


library(dataRetrieval); library(ggplot2) ; library(sf)
library(dplyr); library(ggspatial); library(leaflet); library(EGRET)

#For example, let's see which sites ever measured phosphorus at 
#least 100 times over at least 20 years in Arizona. Water quality 
#data is exclusively found in WQP functions.
VT_sites <- readWQPsummary(
  statecode = "VT",
  siteType = "Stream")

# Filter only for Phosphorus data
vt_phos_summary <- VT_sites |> 
  filter(CharacteristicName == "Phosphorus") |>  # Ensuring we only analyze Phosphorus
  mutate(ResultCount = as.numeric(ResultCount),
         Lat = as.numeric(MonitoringLocationLatitude),
         Lon = as.numeric(MonitoringLocationLongitude)) |> 
  rename(Site = MonitoringLocationIdentifier) |> 
  group_by(Site, Lat, Lon) |> 
  summarise(min_year = min(YearSummarized),
            max_year = max(YearSummarized),
            count = sum(ResultCount)) |> 
  mutate(POR = max_year - min_year) |> 
  filter(count > 100,
         POR >= 20) |> 
  arrange(desc(count)) |> 
  ungroup()



leaflet(data = vt_phos_summary) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~Lon, ~Lat,
                   color = "red", radius = 3, stroke = FALSE,
                   fillOpacity = 0.8, opacity = 0.8,
                   popup = ~Site
  )


https://doi-usgs.github.io/dataRetrieval/articles/tutorial.html#water-quality-portal-wqp


# Nitrate -----------------------------------------------------------------
#https://doi-usgs.github.io/EGRET/articles/EGRET.html#wqa
#Choptank River at Greensboro, MD:
siteNumber <- "01491000" #Choptank River at Greensboro, MD
startDate <- "1979-10-01" # Get earliest date
endDate <- "2011-09-30" # Get latest date
param<-"00631" # Nitrate

Sample <- readNWISSample(siteNumber, param, startDate, endDate)
head(Sample)

# Gather discharge data:
Daily <- readNWISDaily(siteNumber,"00060", startDate, endDate)


INFO <- readNWISInfo(siteNumber,param,interactive=FALSE)
INFO$shortName <- "Choptank River"

# Merge discharge with sample data:
eList <- mergeReport(INFO, Daily, Sample)

# Check sample data:
# Box plot of the water quality data by month
boxConcMonth(eList)

# Two box plots side-by-side, discharge on sample days, 
# and discharge on all days
boxQTwice(eList)

# Plot of Observed Concentration versus Time
plotConcTime(eList)

# Plot of Observed Concentration versus Discharge
plotConcQ(eList)

# Produces a 4 panel plot that gives an overview of 
# the data set prior to any processing
multiPlotDataOverview(eList)

# plotFluxQ(eList, fluxUnit=4) ??? How much nitrogen is 
#being transported by the river? (Total nitrogen load, e.g., kg/day)

# How much total nitrogen is carried by the river (which depends 
#on both concentration and water volume)

# plotConcQ(eList) ??? How concentrated is nitrogen in the water? 
# (Nitrogen per liter, e.g., mg/L)

# How much nitrogen is in each liter of water.

# plotFluxQ(eList, fluxUnit=4) ??? Total nitrogen moving in the river (kg/day).
# plotConcQ(eList) ??? How much nitrogen is in the water (mg/L).

# Flux(kg/day)=Concentration(mg/L)×Discharge(L/day)
# Sample data plot: observed log flux vs log discharge
plotFluxQ(eList, fluxUnit=4)
