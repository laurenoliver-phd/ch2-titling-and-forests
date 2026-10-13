library(dplyr)
library(tidyr)
library(ggtext)
library(fixest)
library(purrr)
library(tibble)
library(lmtest)
library(sf)
library(mapview)

### Data Exploration from 9/9/26
## Brought in the titled parcels from the scrape and the PFR admin data to see the overlap
## Was surprised that there wasn't as much overlap between the titled parcels and the PFR parcels
## Also surprising that there were alot of titled parcels adjacent to PFR parcels -- why?
## Made a few interactive maps, as well as file I could load into GEE

# Loading in the PFR Admin Data with all the shapefiles (Luke's original data)

benin_plots <- st_read(
  "data/raw_data/benin_plots_raw/Benin_plots_raw.shp"
)

# Loading in just the parcels from the scrapes that have titles to simplify the join
# Note: As of 9/21/26 the following file includes data from both the treatment AND control villages

load("~/chapter-2/data/processed/2_cadastre_scrape_titled.RData")


#Checking CRS
st_crs(has_nup) #WGS 84 / UTM zone 31N
st_crs(benin_plots) #WGS 84 / UTM zone 31N

st_crs(has_nup) == st_crs(benin_plots) #TRUE 

# Diagnostic Test for Matches

matches <- st_intersects(has_nup, benin_plots)


# Creating zoomable map to see overlap between scraped parcels and PFR parcels

mapview(
  benin_plots,
  alpha.regions = 0,
  color = "grey",
  lwd = 1
  
) +
  mapview(
    has_nup,
    alpha.regions = 0,
    color = "red",
    lwd = 2
  )


# Exporting file for Google Earth Engine (GEE)
has_nup_gee <- has_nup %>%
  st_transform(4326)

st_write(
  has_nup_gee,
  "data/clean_data/titled_parcels.geojson",
  driver = "GeoJSON",
  delete_dsn = TRUE
)






