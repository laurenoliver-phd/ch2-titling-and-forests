library(dplyr)
library(tidyr)
library(ggtext)
library(purrr)
library(sf)
library(mapview)
library(terra)
library(readxl)
library(stringr)

### Goals ###
# Part 1. Bring together parcel data (PFR Plot/Admin Data and Parcel Data from Cadastre scrape)
##  See: Patapov Data Global Land Cover and Land Use Change, 2000-2020
##  https://glad.umd.edu/dataset/GLCLUC2020
# Part 2. Start exploring Patapov data, seeing what variables are there
## 2.1 Load in GLAD "legend" with key to all the land cover codes


#### Part 0. PREAMBLE ####

# Loading in Parcel data

# PFR Plot Data
pfr_plots <- st_read(
  "data/raw_data/benin_plots_raw/Benin_plots_raw.shp")

# Titled Parcel Data
load("~/chapter-2/data/processed/5_titled_parcels_clean.RData")


# Loading GLAD land-cover rasters 
## Note: 10N and 20N tifs needed for each dataset to cover full extent of Benin

glad_2010_10N <- rast("data/raw_data/GLAD/GLAD_2010_10N_000E.tif")
glad_2010_20N <- rast("data/raw_data/GLAD/GLAD_2010_20N_000E.tif")

glad_2015_10N <- rast("data/raw_data/GLAD/GLAD_2015_10N_000E.tif")
glad_2015_20N <- rast("data/raw_data/GLAD/GLAD_2015_20N_000E.tif")

glad_2020_10N <- rast("data/raw_data/GLAD/GLAD_2020_10N_000E.tif")
glad_2020_20N <- rast("data/raw_data/GLAD/GLAD_2020_20N_000E.tif")

glad_00_20_10N <- rast("data/raw_data/GLAD/GLAD_2000-2020_change_10N.tif")
glad_00_20_20N <- rast("data/raw_data/GLAD/GLAD_2000-2020_change_20N.tif")

## Some Descriptives/Exploration

# glad_2010_10N
# freq(glad_2010_10N)
# plot(glad_2010_10N)


#### Part 1: Preparing Data for Analysis ####

## 1.1 Limiting size of tifs to speed up analysis -> creating study area + 1k buffer

# Buffer both parcel datasets by 1 km
titled_buffer <- st_buffer(
  titled_parcels,
  dist = 1000
)

pfr_buffer <- st_buffer(
  pfr_plots,
  dist = 1000
)

# Transform buffered polygons to GLAD CRS
titled_buffer_glad <- st_transform(
  titled_buffer,
  crs(glad_2010_10N)
)

pfr_buffer_glad <- st_transform(
  pfr_buffer,
  crs(glad_2010_10N)
)

# Convert to terra vectors
titled_buffer_v <- vect(titled_buffer_glad)
pfr_buffer_v <- vect(pfr_buffer_glad)

# Create extent containing all buffered parcels
study_extent <- union(
  ext(titled_buffer_v),
  ext(pfr_buffer_v)
)

### 2010 ###

glad_2010_benin <- mosaic(
  crop(glad_2010_10N, study_extent),
  crop(glad_2010_20N, study_extent)
)


### 2015 ###

glad_2015_benin <- mosaic(
  crop(glad_2015_10N, study_extent),
  crop(glad_2015_20N, study_extent)
)


### 2020 ###

glad_2020_benin <- mosaic(
  crop(glad_2020_10N, study_extent),
  crop(glad_2020_20N, study_extent)
)


### 2000-2020 change ###

glad_00_20_change_benin <- mosaic(
  crop(glad_00_20_10N, study_extent),
  crop(glad_00_20_20N, study_extent)
)




### Save cropped/mosaicked GLAD rasters ###

writeRaster(
  glad_2010_benin,
  "data/processed/spatial_data/GLAD_2010_benin.tif",
  overwrite = TRUE
)

writeRaster(
  glad_2015_benin,
  "data/processed/spatial_data/GLAD_2015_benin.tif",
  overwrite = TRUE
)

writeRaster(
  glad_2020_benin,
  "data/processed/spatial_data/GLAD_2020_benin.tif",
  overwrite = TRUE
)

writeRaster(
  glad_00_20_change_benin,
  "data/processed/spatial_data/GLAD_2000-2020_change_benin.tif",
  overwrite = TRUE
)



# Visual Check 

plot(glad_2010_benin)

plot(
  vect(st_transform(pfr_plots, crs(glad_2010_benin))),
  add = TRUE,
  border = "blue",
  lwd = 0.5
)

plot(
  vect(st_transform(titled_parcels, crs(glad_2010_benin))),
  add = TRUE,
  border = "red",
  lwd = 1
)

#### Part 2: Bringing in GLAD Legend ####

legend_annual <- read_excel("data/raw_data/GLAD/GLAD_classification.xlsx", 
                            sheet = "annual_maps")


legend_change <- read_excel("data/raw_data/GLAD/GLAD_classification.xlsx", 
                            sheet = "2000-2020_change")
# Exploration

glad_freq_2010 <- as.data.frame(
  freq(glad_2010_benin)
)

glad_freq_2010 <- glad_freq_2010 %>%
  left_join(
    legend_annual,
    by = c("value" = "map_value")
  )

View(glad_freq_2010)

legend_annual <- legend_annual %>%
  mutate(
    
    # Cropland indicator
    cropland_dummy = class1 == "Cropland",
    
    # Tree-cover indicator
    treecover_dummy = class2 == "Tree cover",
    
    # Extract tree height from strings such as "5m trees"
    tree_height_m = if_else(
      treecover_dummy,
      as.numeric(stringr::str_extract(subclass, "\\d+")),
      NA_real_
    ),
    
    # Extract short vegetation cover from strings such as
    # "83% short vegetation cover"
    shortveg_cover_pct = if_else(
      stringr::str_detect(subclass, "short vegetation cover"),
      as.numeric(stringr::str_extract(subclass, "\\d+")),
      NA_real_
    )
  )

### 1. Identify relevant GLAD codes from classification table ###

# Cropland codes
cropland_codes <- legend_annual %>%
  filter(class1 == "Cropland") %>%
  pull(map_value)

cropland_codes


# Tree-cover codes
treecover_codes <- legend_annual %>%
  filter(class2 == "Tree cover") %>%
  pull(map_value)

treecover_codes


# Binary cropland rasters
cropland_2010 <- glad_2010_benin %in% cropland_codes
cropland_2015 <- glad_2015_benin %in% cropland_codes
cropland_2020 <- glad_2020_benin %in% cropland_codes

plot(
  cropland_2010,
  main = "Cropland, 2010"
)


plot(
  vect(st_transform(titled_parcels, crs(cropland_2010))),
  add = TRUE,
  border = "red",
  lwd = 1
)

# Binary Treecover rasters

treecover_2010 <- glad_2010_benin %in% treecover_codes
treecover_2015 <- glad_2015_benin %in% treecover_codes
treecover_2020 <- glad_2020_benin %in% treecover_codes

plot(
  treecover_2010,
  main = "Treecover, 2010"
)

plot(
  vect(st_transform(titled_parcels, crs(treecover_2010))),
  add = TRUE,
  border = "red",
  lwd = 1
)




