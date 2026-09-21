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


### Binding Together all the parcel dfs from the scrape 

all_parcels <- rbind(
  all_parcels1_to_20,
  all_parcels21_to_40,
  all_parcels41_to_60,
  all_parcels61_to_70,
  all_parcels71_to_80,
  all_parcels81_to_91,
  all_parcels92_to_,
  all_parcels93_to_100,
  all_parcels101_to_115,
  all_parcels116_to_130,
  all_parcels131_to_150,
  all_parcels151_to_180,
  all_parcels181_to_200,
  all_parcels201_to_230, 
  all_parcels231_to_260,
  all_parcels261_to_290
)

all_parcels_dis <- distinct(all_parcels, geometry)

library(sf)
library(dplyr)

duplicate_parcels <- all_parcels %>%
  mutate(geometry_id = st_as_text(geometry)) %>%
  group_by(geometry_id) %>%
  filter(n() > 1) %>%
  ungroup()


all_parcels_dedup <- all_parcels %>%
  mutate(geometry_id = st_as_text(geometry)) %>%
  distinct(geometry_id, .keep_all = TRUE) %>%
  select(-geometry_id)

all_parcels_dedup <- all_parcels[
  !duplicated(st_as_text(st_geometry(all_parcels))),
]


has_nup <- all_parcels_dedup %>%
  filter(!is.na(nup))
