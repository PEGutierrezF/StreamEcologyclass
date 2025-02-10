

library(dataRetrieval)
library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(leaflet)

# measured phosphorus in Vermont
vt_sites <- whatNWISsites(stateCd = "VT", 
                          parameterCd = "00665")
nrow(vt_sites)
names(vt_sites)

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE),
                crs = 4269)

#crs = 4269: This sets the coordinate reference system (CRS) to 
#EPSG:4269, which corresponds to the "NAD83" (North American Datum 
#1983), a common CRS for mapping the United States.
sf_vt <- st_as_sf(vt_sites, 
                  coords = c("dec_long_va", "dec_lat_va"),
                  crs = 4269)
ggplot() +
  geom_sf(data = usa[ usa$ID == "vermont" ,]) +
  geom_sf(data = sf_vt) + 
  xlab(NULL)+
  ylab(NULL)+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  annotation_scale(location = "br", width_hint = 0.1, unit = "mi")


# Test 2 ------------------------------------------------------------------
phosSites <- whatWQPsites(statecode = "WI",
                          characteristicName = "Phosphorus")


phos_data_available <- whatWQPdata(statecode = "WI",
                                   characteristicName = "Phosphorus")


phos_data_sites_to_get <- phos_data_available %>% 
  filter(resultCount >= 300)

phosData <- readWQPdata(siteNumbers = phos_data_sites_to_get$MonitoringLocationIdentifier,
                        characteristicName = "Phosphorus")

unique(phosData$ResultMeasure.MeasureUnitCode)

siteInfo <- attr(phosData, "siteInfo")

wiSummary <- phosData %>%
  filter(ResultMeasure.MeasureUnitCode %in% 
           c("mg/l","mg/l as P")) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(count=n(),
            max = max(ResultMeasureValue, na.rm = TRUE)) %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier")


# Convert the wiSummary dataframe into an sf object
phos_sf <- st_as_sf(wiSummary, 
                    coords = c("dec_lon_va", "dec_lat_va"), 
                    crs = 4326)  # WGS84 CRS (standard for lat/lon)

# Assuming you have a shapefile or spatial data for Wisconsin or the USA (e.g., usa, sf_az)
# Here's an example using a general map for Wisconsin (replace with your actual spatial data):
library(tigris)

# Get Wisconsin boundaries
wi_sf <- states(cb = TRUE) %>% 
  filter(STUSPS == "WI")

# Create the plot
ggplot() +
  geom_sf(data = wi_sf, fill = "lightgray", color = "black") +  # Wisconsin boundaries
  geom_sf(data = phos_sf, aes(color = as.numeric(max), size = count)) +  # Phosphorus sites with size/color based on max concentration
  scale_color_viridis_c() +  # Color scale for phosphorus concentration
  xlab(NULL) + ylab(NULL) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Maximum Phosphorus Concentration in Wisconsin",
       subtitle = "Phosphorus data points with concentration",
       color = "Max Phosphorus (mg/l)", size = "Data Points Count") +
 # north(wi_sf, symbol = 10, location = "bottomleft") +  # Add a north arrow
  annotation_scale(location = "br", width_hint = 0.1, unit = "mi")




