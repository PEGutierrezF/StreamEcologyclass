#For example, let’s see which sites ever measured phosphorus at 
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
         POR >= 10) |> 
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



# Site with Phosphotus data in VT -----------------------------------------
num_sites <- whatNWISdata(stateCd = "VT", parameterCd = "00631")
head(num_sites)

# Nitrate -----------------------------------------------------------------
#https://doi-usgs.github.io/EGRET/articles/EGRET.html#wqa
#Choptank River at Greensboro, MD:
siteNumber <- "01491000" 
startDate <- "1979-10-01"
endDate <- "2011-09-30"
param<-"00631" # Nitrate
Daily <- readNWISDaily(siteNumber,"00060",startDate,endDate)


INFO<- readNWISInfo(siteNumber,param,interactive=FALSE)
INFO$shortName <- "Choptank River"

Sample <- readNWISSample(siteNumber,param,startDate,endDate)
eList <- mergeReport(INFO, Daily, Sample)

boxConcMonth(eList)

plotConcTime(eList)
plotConcQ(eList, qUnit=1)

plotFluxQ(eList, fluxUnit=4)

