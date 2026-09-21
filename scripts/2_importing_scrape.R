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

# Loading in all the dfs from the scrape (for villages 1 to 290) and creating one df

files <- list.files(
  "data/raw_data/cadastre_bj_scrape",
  pattern = "\\.RData$",
  full.names = TRUE
)

cadastre_list <- map(files, function(f) {
  e <- new.env()
  obj <- load(f, envir = e)
  e[[obj]]
})

full_scrape <- do.call(rbind, cadastre_list)

# Removing duplicates created when scrape sampled the same parcel multiple times (due to overlapping village boundaries)

cadastre_pfr_all <- all_parcels[
  !duplicated(st_as_text(st_geometry(all_parcels))),
]

save(
  cadastre_pfr_all,
  file = "data/processed/2_cadastre_scrape_all_parcels.RData"
)


# Creating df of only parcels with titles (NTS: All parcels that have an nup have a numtf and vice-versa)

has_nup <- cadastre_pfr_all %>%
  filter(!is.na(nup))

save(
  has_nup,
  file = "data/processed/2_cadastre_scrape_titled.RData"
)
