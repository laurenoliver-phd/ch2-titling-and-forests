library(terra)

base_url <- "https://storage.googleapis.com/earthenginepartners-hansen/GLCLU2000-2020/v2"

# Folder where you want the rasters stored
#dir.create("data/raw_data/GLAD", recursive = TRUE, showWarnings = FALSE)

load_glclu <- function(year, tile) {
  
  url <- paste0(
    base_url, "/",
    year, "/",
    tile, ".tif"
  )
  
  local_file <- file.path(
    "data/raw_data/GLAD",
    paste0("GLAD_", year, "_", tile, ".tif")
  )
  
  # Only download if we haven't already downloaded it
  if (!file.exists(local_file)) {
    download.file(
      url,
      destfile = local_file,
      mode = "wb"
    )
  }
  
  terra::rast(local_file)
}

# -----------------------------
# Load Benin tiles
# -----------------------------

# Southern Benin: 10N, 0E
glad_2010_south <- load_glclu(2010, "10N_000E")
glad_2015_south <- load_glclu(2015, "10N_000E")
glad_2020_south <- load_glclu(2020, "10N_000E")

# Northern Benin: 20N, 0E
glad_2010_north <- load_glclu(2010, "20N_000E")
glad_2015_north <- load_glclu(2015, "20N_000E")
glad_2020_north <- load_glclu(2020, "20N_000E")