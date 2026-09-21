library(sf)
library(dplyr)
library(purrr)
library(httr2)

# Assumes you already defined:
# - villages (sf polygons), id_col (e.g., "VID")
# - grid_points_in_poly(poly, spacing)
# - get_info(x, y)
# - to_sf(x)  -> returns an sf row with surface, nup, numtf, geometry, etc.

setwd("~/Library/CloudStorage/Dropbox-YSE")

villages <- st_read("~/Library/CloudStorage/Dropbox-YSE/Lauren Oliver/Benin_land_titles/Lauren/data/00_manually-downloaded/villages/TreatedVillagesCircles.shp") %>%
  st_transform(, 32631) %>%
  mutate(VID = row_number())

villages_all <- villages


#####

# villages_32631 is your tile polygons
villages_32631 <- villages_all
bad_villages <- which(!st_is_valid(villages_32631))

length(bad_villages)
bad_villages
villages_32631[bad_villages, ]


##### Running the scraping loop:

id_col <- "VID"                                   

spacing <- 200     # meters
sleep_s <- 0.05    # rate limit

# --- API call (same as yours, but add timeout/retry to avoid hangs) ---
get_info <- function(x, y) {
  url <- paste0("https://cadastre.bj/back-api/search/location?lat=", y, "&lon=", x, "&type=0")
  r <- request(url) |>
    req_headers(Accept="application/json") |>
    req_timeout(8) |>
    req_retry(max_tries = 3) |>
    req_perform()
  if (resp_status(r) != 200) return(NULL)
  resp_body_json(r)
}

to_sf <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  
  rec <- x[[1]]  # API returns [ { record } ]
  
  coords <- rec$geom$geometry$coordinates
  if (is.null(coords) || length(coords) == 0) return(NULL)
  
  ring <- coords[[1]][[1]]
  if (is.null(ring) || length(ring) < 4) return(NULL)
  
  mat <- do.call(rbind, lapply(ring, function(pt) as.numeric(pt)[1:2]))
  if (!all(mat[1,] == mat[nrow(mat),])) mat <- rbind(mat, mat[1,])
  
  poly <- st_polygon(list(mat))
  
  st_sf(
    nup = null_to_na_chr(rec$nup),
    numtf = null_to_na_chr(rec$numtf),
    departement = null_to_na_chr(rec$departement),
    commune = null_to_na_chr(rec$commune),
    arrondissement = null_to_na_chr(rec$arrondissement),
    quartier = null_to_na_chr(rec$quartier),
    ilot = null_to_na_chr(rec$ilot),
    parcelle = null_to_na_chr(rec$parcelle),
    surface = null_to_na_num(rec$surface),
    geometry = st_sfc(poly, crs = 32631)
  )
}

grid_points_in_poly <- function(poly, spacing = 75) {
  b <- st_bbox(poly)
  xs <- seq(b["xmin"], b["xmax"], by = spacing)
  ys <- seq(b["ymin"], b["ymax"], by = spacing)
  
  pts <- expand.grid(x = xs, y = ys)
  pts_sf <- st_as_sf(pts, coords = c("x", "y"), crs = 32631)
  
  inside <- st_within(pts_sf, poly, sparse = FALSE)[,1]
  pts_sf[inside, ]
}

null_to_na_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)[1]
}

null_to_na_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x)[1]
}

fix_geom <- function(x) {
  x <- tryCatch(st_make_valid(x), error = function(e) x)
  x <- tryCatch(st_buffer(x, 0), error = function(e) x)  # common GEOS fix
  x
}

safe_wkt <- function(sfobj) {
  # returns character WKT, with NA for geometries that still error
  sapply(seq_len(nrow(sfobj)), function(i) {
    g <- st_geometry(sfobj[i, ])
    g <- tryCatch(fix_geom(g), error = function(e) NULL)
    if (is.null(g)) return(NA_character_)
    tryCatch(st_as_text(g)[1], error = function(e) NA_character_)
  })
}

###

all_list <- vector("list", nrow(villages_all))

### Test code with short list (delete later)

villages <- villages_all[20:272,]
villages<- st_transform(villages, 32631)

for (i in seq_len(nrow(villages))) {
  v_id <- villages[[id_col]][i]
  message(sprintf("Village %d / %d (%s=%s)", i, nrow(villages), id_col, v_id))
  
  poly <- villages[i, ]
  
  pts_sf <- grid_points_in_poly(poly, spacing)
  if (nrow(pts_sf) == 0) {
    message("  no grid points inside polygon; skipping")
    next
  }
  
  xy <- st_coordinates(pts_sf)
  
  res <- tryCatch(
    map2(xy[,1], xy[,2], function(x, y) {
      Sys.sleep(sleep_s)
      get_info(x, y)
    }),
    error = function(e) {
      message("  API error: ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(res)) next
  
  sf_list <- map(res, to_sf) |> compact()
  if (length(sf_list) == 0) {
    message("  no geometries returned; skipping")
    next
  }
  
  parcels_raw <- do.call(rbind, sf_list)
  
  # Repair invalid parcel geometries before any geometry-based dedupe
  parcels_raw <- parcels_raw %>%
    mutate(geometry = fix_geom(st_geometry(.)))
  
  # Robust dedupe key: surface + geometry (WKT fingerprint)
  parcels_i <- parcels_raw %>%
    mutate(
      has_nup = !is.na(nup) & nup != "",
      geom_wkt = safe_wkt(.)
    ) %>%
    filter(!is.na(geom_wkt)) %>%                    # drop the truly broken ones
    distinct(surface, geom_wkt, .keep_all = TRUE) %>%
    select(-geom_wkt) %>%
    mutate(!!id_col := v_id)
  
  message("  kept ", nrow(parcels_i), " unique features (surface+geometry)")
  all_list[[i]] <- parcels_i
}

# rbind everything at the end

all_parcels <- all_list |> compact() |> (\(x) do.call(rbind, x))()

# optional: one more global dedupe across villages (same key)
all_parcels <- all_parcels %>%
  mutate(geom_wkt = st_as_text(st_geometry(.))) %>%
  distinct(surface, geom_wkt, .keep_all = TRUE) %>%
  select(-geom_wkt)

nrow(all_parcels)

# st_write(all_parcels, "cadastre_all_parcels_surface_geom.gpkg", delete_dsn = TRUE)