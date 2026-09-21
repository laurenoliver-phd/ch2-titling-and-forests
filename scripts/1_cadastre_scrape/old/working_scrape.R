library(sf)
library(httr2)
library(dplyr)
library(purrr)
library(janitor)

# Example bbox around your clicked area (meters, EPSG:32631)
bbox <- c(458000, 1046000, 459000, 1047000)  # xmin, ymin, xmax, ymax

get_info <- function(x, y) {
  url <- paste0(
    "https://cadastre.bj/back-api/search/location",
    "?lat=", y, "&lon=", x, "&type=0"
  )
  r <- request(url) |>
    req_headers(Accept="application/json") |>
    req_perform()
  
  if (resp_status(r) != 200) return(NULL)
  resp_body_json(r)
}

# grid spacing in meters (smaller = more complete coverage, slower)
spacing <- 75  

xs <- seq(bbox[1], bbox[3], by = spacing)
ys <- seq(bbox[2], bbox[4], by = spacing)

grid_pts <- expand.grid(x = xs, y = ys)

res <- pmap(grid_pts, function(x, y) {
  Sys.sleep(0.05)  # rate limit
  get_info(x, y)
})

##
null_to_na_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  as.character(x)[1]
}

null_to_na_num <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  as.numeric(x)[1]
}

to_sf <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  
  rec <- x[[1]]
  
  coords <- rec$geom$geometry$coordinates
  if (is.null(coords) || length(coords) == 0) return(NULL)
  
  ring <- coords[[1]][[1]]
  if (is.null(ring) || length(ring) < 4) return(NULL)
  
  mat <- do.call(rbind, lapply(ring, function(pt) as.numeric(pt)[1:2]))
  
  # close ring if needed
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



##

sf_list <- map(res, to_sf) |> purrr::compact()
length(sf_list)

parcels_raw <- do.call(rbind, sf_list)

parcels <- parcels_raw %>%
  filter(!is.na(nup)) %>%
  distinct(nup, .keep_all = TRUE)

nrow(parcels)
