library(dplyr)
library(tidyr)
library(ggtext)
library(fixest)
library(purrr)
library(tibble)
library(lmtest)
library(sf)
library(mapview)

### Goals ###

# 1. Create new dataset where all titled parcels are attached to a treatment or control village
# 2. Identify parcels that are direct match to a PFR parcel, a partial overlap, or a direct neighbor
# 3. Attach all parcels to a treatment or control village by nearest distance

#### PREAMBLE ####

### Loading data ### 

pfr_plots <- st_read(
  "data/raw_data/benin_plots_raw/Benin_plots_raw.shp")

village_centers <- st_read(
  "data/raw_data/village_centers/village_centers.shp")

load("~/chapter-2/data/processed/2_cadastre_scrape_titled.RData") #titled_parcels

#### Part 1: Matching Parcels ####

### Determining which titled parcels match the PFR plots ###

# Confirm that the two datasets have the same CRS
st_crs(titled_parcels) == st_crs(pfr_plots)


### Identifying degree of overlap between titled parcels and PFR parcels ###

# Identify PFR parcels that intersect each titled parcel
matches <- st_intersects(titled_parcels, pfr_plots)

# Number of titled parcels that intersect at least one PFR parcel
sum(lengths(matches) > 0)

# Distribution of number of intersecting PFR parcels
table(lengths(matches))


### Calculate actual overlap area ###

# Create one row for every titled parcel × PFR parcel intersection
overlaps <- st_intersection(
  titled_parcels %>% select(nup),
  pfr_plots %>% select(PARCELL)
)

# Calculate area of each intersection
overlaps <- overlaps %>%
  mutate(
    overlap_area = st_area(geometry)
  )


### Calculate percentage of each titled parcel that overlaps ###

# Calculate total area of each titled parcel
titled_parcels <- titled_parcels %>%
  mutate(
    parcel_area = st_area(geometry)
  )

# Add titled-parcel area to the overlap data and calculate
# proportion of the titled parcel covered by each PFR parcel
overlaps <- overlaps %>%
  left_join(
    titled_parcels %>%
      st_drop_geometry() %>%
      select(nup, parcel_area),
    by = "nup"
  ) %>%
  mutate(
    pct_overlap = as.numeric(overlap_area / parcel_area)
  )


### Identify strong PFR matches ###

# Define a strong match as a PFR parcel covering >30%
# of the titled parcel
strong_overlap <- overlaps %>%
  filter(pct_overlap > 0.30)

nrow(strong_overlap)


### Join strong PFR matches back to titled parcels ###

# Rank multiple PFR matches from strongest to weakest
strong_overlap_wide <- strong_overlap %>%
  st_drop_geometry() %>%
  group_by(nup) %>%
  arrange(desc(pct_overlap), .by_group = TRUE) %>%
  mutate(
    overlap_num = row_number()
  ) %>%
  ungroup() %>%
  select(nup, overlap_num, PARCELL) %>%
  pivot_wider(
    names_from = overlap_num,
    values_from = PARCELL,
    names_prefix = "pfr_overlap"
  )

# Join PFR matches back to titled parcels
titled_parcels <- titled_parcels %>%
  left_join(
    strong_overlap_wide,
    by = "nup"
  )

#### Part 2: Neighboring Parcels ####

# Create a 10 m buffer around each titled parcel
titled_buffer_10m <- st_buffer(
  titled_parcels,
  dist = 10
)

# Find all PFR parcels that intersect each titled parcel's 10 m buffer
neighbors_10m <- st_intersects(
  titled_buffer_10m,
  pfr_plots
)


### Convert spatial matches to PARCELL IDs ###

# For each titled parcel, get the PARCELL numbers of all PFR parcels
# that intersect its 10 m buffer
neighbor_parcells <- lapply(
  neighbors_10m,
  function(x) pfr_plots$PARCELL[x]
)


### Identify PFR parcels that are strong overlaps ###

# Create a list of strongly overlapping PARCELL IDs for each titled parcel
overlap_parcells <- strong_overlap %>%
  st_drop_geometry() %>%
  group_by(nup) %>%
  summarise(
    overlap_ids = list(PARCELL),
    .groups = "drop"
  )

# Match this list back to the order of titled_parcels
overlap_list <- overlap_parcells$overlap_ids[
  match(titled_parcels$nup, overlap_parcells$nup)
]


### Remove overlapping PFR parcels from neighbor lists ###

pfr_neighbors <- Map(
  function(neighbors, overlaps) {
    
    # If this titled parcel has no strong overlaps,
    # there is nothing to remove
    if (is.null(overlaps)) {
      return(neighbors)
    }
    
    # Otherwise remove its overlapping PFR parcel(s)
    setdiff(neighbors, overlaps)
    
  },
  neighbor_parcells,
  overlap_list
)


### Add neighbor information to titled_parcels ###

titled_parcels <- titled_parcels %>%
  mutate(
    pfr_neighbors = pfr_neighbors,
    n_pfr_neighbors = lengths(pfr_neighbors))
    
    
#### Part 3: Attaching Villages ####
# Steps
# 3.1 Convert each titled parcel polygon to a representative point.
# 3.2 Find the nearest village-center point.
# 3.3 Bring the village variables—especially VID—onto titled_parcels.
# 3.4 Calculate the distance to the assigned village 

# 0. Confirm that the two datasets have the same CRS
st_crs(titled_parcels) == st_crs(village_centers) # FALSE

village_centers <- st_transform(
  village_centers,
  st_crs(titled_parcels)
)

st_crs(titled_parcels) == st_crs(village_centers) #TRUE

# Create a representative point for each titled parcel
titled_points <- st_point_on_surface(titled_parcels)

# Row number of nearest village for each titled parcel
nearest_village <- st_nearest_feature(
  titled_points,
  village_centers
)

# Add village ID and other useful village variables
titled_parcels <- titled_parcels %>%
  mutate(
    VID = village_centers$VID[nearest_village],
    village_treated = village_centers$treated[nearest_village],
    village_name = village_centers$vllg_nm[nearest_village],
    commune = village_centers$NAME_2[nearest_village]
  )

titled_parcels$distance_to_village <- st_distance(
  titled_points,
  village_centers[nearest_village, ],
  by_element = TRUE
)

titled_parcels <- titled_parcels %>%
  mutate(
    distance_to_village_m = as.numeric(distance_to_village)
  ) %>%
  select(-distance_to_village)  

summary(titled_parcels$distance_to_village_m)

### Join village information onto titled parcels ###

titled_parcels <- titled_parcels %>%
  left_join(
    village_centers %>%
      st_drop_geometry() %>%
      select(VID, treated, NAME_1, NAME_2),
    by = "VID"
  )


table(titled_parcels$treated) # n_control = 675, n_treated = 654

w_neighbors <- titled_parcels %>%
  filter(n_pfr_neighbors>0)

save(
  titled_parcels,
  file = "data/processed/5_titled_parcels_clean.RData"
)

