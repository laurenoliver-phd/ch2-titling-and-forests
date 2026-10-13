library(dplyr)
library(tidyr)
library(ggtext)
library(fixest)
library(vtable)
library(modelsummary)
library(broom)
library(stringr)
library(stringi)
library(xtable)
library(lubridate)
library(haven)
library(readxl)
library(ggplot2)
library(lme4)
library(performance)
library(purrr)
library(tibble)
library(sandwich)
library(lmtest)
library(sf)



## Loading in all the dfs from the TREATED scrape (for villages 1 to 290) 


files <- list.files(
  "data/raw_data/cadastre_bj_scrape_treated",
  pattern = "\\.RData$",
  full.names = TRUE
)

cadastre_list <- map(files, function(f) {
  e <- new.env()
  obj <- load(f, envir = e)
  e[[obj]]
})

treated_scrape <- do.call(rbind, cadastre_list) %>%
  select(-VID)

##  Adding in data from scrape of CONTROL villages 

# Folder containing scraped village files
control_scrape_dir <- "data/raw_data/cadastre_bj_scrape_control"

# Get all village .rds files
village_files <- list.files(
  control_scrape_dir,
  pattern = "^village_[0-9]+\\.rds$",
  full.names = TRUE
)

# Read and row-bind all villages
control_scrape <- village_files %>%
  map(readRDS) %>%
  bind_rows() %>%
  select(-VID1)

## Bringing together treated and control

full_scrape <- rbind(treated_scrape, control_scrape)



# Removing duplicates created when scrape sampled the same parcel multiple times (due to overlapping village boundaries)

cadastre_pfr_all <- full_scrape[
  !duplicated(st_as_text(st_geometry(full_scrape))),
]

save(
  cadastre_pfr_all,
  file = "data/processed/2_cadastre_scrape_all_parcels.RData"
)


# Creating df of only parcels with titles (NTS: All parcels that have an nup have a numtf and vice-versa)

titled_parcels <- cadastre_pfr_all %>%
  filter(!is.na(nup))

save(
  titled_parcels,
  file = "data/processed/2_cadastre_scrape_titled.RData"
)


