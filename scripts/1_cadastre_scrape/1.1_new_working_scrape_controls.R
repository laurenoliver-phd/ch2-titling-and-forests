library(sf)
library(dplyr)
library(purrr)
library(httr2)



# Load Voronoi village polygons
voronoi_villages <- st_read(
  "data/raw_data/voronoi_villages/voronoi_villages.shp"
)


output_dir <- "data/raw_data/cadastre_bj_scrape_control"

control_voronoi_villages <- voronoi_villages %>%
  filter(treated == 0) %>%
  mutate(VID1 = VID+290) 


# ============================================================
# SETTINGS
# ============================================================

id_col <- "VID1"

# Grid spacing in meters
spacing <- 300

# Folder for completed village files
output_dir <- "data/raw_data/cadastre_bj_scrape_control"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)

# Number of attempts for each point during first pass
max_attempts <- 3

# Number of attempts for points that are still unresolved
# during the second pass
retry_attempts <- 3

# Timeout for each individual HTTP request
timeout_seconds <- 15


# ============================================================
# HELPER FUNCTIONS
# ============================================================

null_to_na_chr <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  
  as.character(x)[1]
}


null_to_na_num <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_real_)
  }
  
  as.numeric(x)[1]
}


fix_geom <- function(x) {
  
  x <- tryCatch(
    st_make_valid(x),
    error = function(e) x
  )
  
  x <- tryCatch(
    st_buffer(x, 0),
    error = function(e) x
  )
  
  x
}


safe_wkt <- function(sfobj) {
  
  sapply(seq_len(nrow(sfobj)), function(i) {
    
    g <- st_geometry(sfobj[i, ])
    
    g <- tryCatch(
      fix_geom(g),
      error = function(e) NULL
    )
    
    if (is.null(g)) {
      return(NA_character_)
    }
    
    tryCatch(
      st_as_text(g)[1],
      error = function(e) NA_character_
    )
  })
}


# ============================================================
# SINGLE API REQUEST
# ============================================================
#
# This performs ONE request only.
#
# It returns:
#
# success = TRUE
#   Request successfully reached the API.
#
# success = FALSE
#   Request failed because of timeout, connection error, etc.
#
# This distinction is important because an empty API response
# is NOT the same thing as a failed request.

single_request <- function(x, y) {
  
  url <- paste0(
    "https://cadastre.bj/back-api/search/location?",
    "lat=", y,
    "&lon=", x,
    "&type=0"
  )
  
  tryCatch({
    
    r <- request(url) |>
      req_headers(
        Accept = "application/json"
      ) |>
      req_timeout(timeout_seconds) |>
      req_perform()
    
    status <- resp_status(r)
    
    if (status != 200) {
      
      return(
        list(
          success = FALSE,
          data = NULL,
          error = paste0("HTTP status ", status)
        )
      )
    }
    
    dat <- resp_body_json(r)
    
    list(
      success = TRUE,
      data = dat,
      error = NA_character_
    )
    
  }, error = function(e) {
    
    list(
      success = FALSE,
      data = NULL,
      error = conditionMessage(e)
    )
  })
}


# ============================================================
# REQUEST WITH EXPLICIT RETRIES
# ============================================================
#
# Unlike req_retry(), this explicitly retries connection
# failures and timeouts.
#
# Wait times increase after each failed attempt.

get_info <- function(
    x,
    y,
    attempts = 3,
    verbose = FALSE
) {
  
  last_error <- NA_character_
  
  for (attempt in seq_len(attempts)) {
    
    # Small random delay before every request
    Sys.sleep(
      runif(
        1,
        min = 0.2,
        max = 0.5
      )
    )
    
    result <- single_request(
      x,
      y
    )
    
    # Successful HTTP request
    if (result$success) {
      
      return(
        list(
          success = TRUE,
          data = result$data,
          attempts = attempt,
          error = NA_character_
        )
      )
    }
    
    last_error <- result$error
    
    if (verbose) {
      
      message(
        "      attempt ",
        attempt,
        "/",
        attempts,
        " failed"
      )
    }
    
    # Backoff before trying again
    if (attempt < attempts) {
      
      wait_seconds <- 2^(attempt - 1) +
        runif(1, 0, 1)
      
      Sys.sleep(
        wait_seconds
      )
    }
  }
  
  
  # All attempts failed
  list(
    success = FALSE,
    data = NULL,
    attempts = attempts,
    error = last_error
  )
}


# ============================================================
# CONVERT API RESPONSE TO SF
# ============================================================

to_sf <- function(x) {
  
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  
  rec <- x[[1]]
  
  if (is.null(rec)) {
    return(NULL)
  }
  
  coords <- rec$geom$geometry$coordinates
  
  if (is.null(coords) || length(coords) == 0) {
    return(NULL)
  }
  
  ring <- coords[[1]][[1]]
  
  if (is.null(ring) || length(ring) < 4) {
    return(NULL)
  }
  
  mat <- do.call(
    rbind,
    lapply(
      ring,
      function(pt) as.numeric(pt)[1:2]
    )
  )
  
  # Make sure polygon is closed
  if (!all(mat[1, ] == mat[nrow(mat), ])) {
    mat <- rbind(
      mat,
      mat[1, ]
    )
  }
  
  poly <- st_polygon(
    list(mat)
  )
  
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
    
    geometry = st_sfc(
      poly,
      crs = 32631
    )
  )
}


# ============================================================
# CREATE GRID POINTS
# ============================================================

grid_points_in_poly <- function(
    poly,
    spacing = 300
) {
  
  b <- st_bbox(poly)
  
  xs <- seq(
    b["xmin"],
    b["xmax"],
    by = spacing
  )
  
  ys <- seq(
    b["ymin"],
    b["ymax"],
    by = spacing
  )
  
  pts <- expand.grid(
    x = xs,
    y = ys
  )
  
  pts_sf <- st_as_sf(
    pts,
    coords = c("x", "y"),
    crs = 32631
  )
  
  inside <- st_within(
    pts_sf,
    poly,
    sparse = FALSE
  )[, 1]
  
  pts_sf[inside, ]
}


# ============================================================
# PREPARE VILLAGES
# ============================================================

# ------------------------------------------------------------
# FULL RUN
# ------------------------------------------------------------

villages <- control_voronoi_villages[29:85,]


# ------------------------------------------------------------
# TEST RUN
# ------------------------------------------------------------
#
# Comment out the full-run line above and uncomment this
# if you want to test village 293 first:
#
# villages <- control_voronoi_villages[
#   control_voronoi_villages$VID1 == 300,
#  ]


villages <- st_transform(
  villages,
  32631
)


# ============================================================
# STORAGE
# ============================================================

all_list <- vector(
  "list",
  nrow(villages)
)


# ============================================================
# MAIN VILLAGE LOOP
# ============================================================

for (i in seq_len(nrow(villages))) {
  
  v_id <- villages[[id_col]][i]
  
  
  message(
    "\n========================================"
  )
  
  message(
    sprintf(
      "Village %d / %d (%s=%s)",
      i,
      nrow(villages),
      id_col,
      v_id
    )
  )
  
  
  # ==========================================================
  # FILE NAMES
  # ==========================================================
  
  village_file <- file.path(
    output_dir,
    paste0(
      "village_",
      v_id,
      ".rds"
    )
  )
  
  
  failed_file <- file.path(
    output_dir,
    paste0(
      "village_",
      v_id,
      "_failed_points.rds"
    )
  )
  
  
  # ==========================================================
  # SKIP ALREADY-COMPLETED VILLAGES
  # ==========================================================
  
  if (file.exists(village_file)) {
    
    message(
      "  already completed; loading saved file"
    )
    
    all_list[[i]] <- readRDS(
      village_file
    )
    
    next
  }
  
  
  # ==========================================================
  # GET VILLAGE POLYGON
  # ==========================================================
  
  poly <- villages[i, ]
  
  
  # ==========================================================
  # GENERATE QUERY GRID
  # ==========================================================
  
  pts_sf <- grid_points_in_poly(
    poly,
    spacing
  )
  
  
  if (nrow(pts_sf) == 0) {
    
    message(
      "  no grid points inside polygon; skipping"
    )
    
    next
  }
  
  
  xy <- st_coordinates(
    pts_sf
  )
  
  
  n_points <- nrow(xy)
  
  
  message(
    "  querying ",
    n_points,
    " grid points"
  )
  
  
  # ==========================================================
  # FIRST PASS
  # ==========================================================
  
  results <- vector(
    "list",
    n_points
  )
  
  
  failed_indices <- integer(0)
  
  
  for (j in seq_len(n_points)) {
    
    result <- get_info(
      x = xy[j, 1],
      y = xy[j, 2],
      attempts = max_attempts
    )
    
    
    results[[j]] <- result
    
    
    # Record genuine request failures
    if (!result$success) {
      
      failed_indices <- c(
        failed_indices,
        j
      )
      
      message(
        "    failed point ",
        j,
        "/",
        n_points,
        " (",
        round(xy[j, 1], 1),
        ", ",
        round(xy[j, 2], 1),
        ")"
      )
    }
    
    
    # Progress report every 50 points
    if (
      j %% 50 == 0 ||
      j == n_points
    ) {
      
      message(
        "  progress: ",
        j,
        " / ",
        n_points
      )
    }
  }
  
  
  # ==========================================================
  # FIRST-PASS SUMMARY
  # ==========================================================
  
  n_failed_first <- length(
    failed_indices
  )
  
  n_success_first <- n_points -
    n_failed_first
  
  
  message(
    "\n  first pass complete"
  )
  
  message(
    "  ",
    n_success_first,
    " successful requests"
  )
  
  message(
    "  ",
    n_failed_first,
    " failed requests"
  )
  
  
  # ==========================================================
  # SECOND PASS: RETRY FAILED POINTS
  # ==========================================================
  
  if (n_failed_first > 0) {
    
    message(
      "\n  waiting 10 seconds before retry pass..."
    )
    
    Sys.sleep(10)
    
    
    message(
      "  retrying ",
      n_failed_first,
      " failed points"
    )
    
    
    still_failed <- integer(0)
    
    recovered <- 0
    
    
    for (k in seq_along(failed_indices)) {
      
      j <- failed_indices[k]
      
      
      message(
        "    retry ",
        k,
        "/",
        n_failed_first,
        " — point ",
        j
      )
      
      
      result <- get_info(
        x = xy[j, 1],
        y = xy[j, 2],
        attempts = retry_attempts,
        verbose = FALSE
      )
      
      
      # Replace first-pass result with retry result
      results[[j]] <- result
      
      
      if (result$success) {
        
        recovered <- recovered + 1
        
      } else {
        
        still_failed <- c(
          still_failed,
          j
        )
      }
    }
    
    
    message(
      "\n  retry pass complete"
    )
    
    message(
      "  ",
      recovered,
      " points recovered"
    )
    
    message(
      "  ",
      length(still_failed),
      " points still unresolved"
    )
    
    
  } else {
    
    still_failed <- integer(0)
    
    message(
      "  no retry pass necessary"
    )
  }
  
  
  # ==========================================================
  # SAVE UNRESOLVED COORDINATES
  # ==========================================================
  
  if (length(still_failed) > 0) {
    
    failed_points <- data.frame(
      
      VID1 = v_id,
      
      point_index = still_failed,
      
      x = xy[
        still_failed,
        1
      ],
      
      y = xy[
        still_failed,
        2
      ],
      
      error = sapply(
        still_failed,
        function(j) {
          results[[j]]$error
        }
      )
    )
    
    
    saveRDS(
      failed_points,
      failed_file
    )
    
    
    message(
      "  saved unresolved points: ",
      failed_file
    )
    
    
  } else {
    
    # Remove an old failed-points file if a later run
    # successfully resolves everything
    
    if (file.exists(failed_file)) {
      file.remove(failed_file)
    }
  }
  
  
  # ==========================================================
  # EXTRACT SUCCESSFUL API RESPONSES
  # ==========================================================
  
  successful_results <- results[
    map_lgl(
      results,
      ~ .x$success
    )
  ]
  
  
  api_data <- map(
    successful_results,
    "data"
  )
  
  
  # ==========================================================
  # CONVERT TO SF
  # ==========================================================
  
  sf_list <- map(
    api_data,
    to_sf
  ) |>
    compact()
  
  
  if (length(sf_list) == 0) {
    
    message(
      "  no parcel geometries returned"
    )
    
    # Do NOT create village completion file here.
    # This allows the village to be attempted again later.
    
    next
  }
  
  
  parcels_raw <- do.call(
    rbind,
    sf_list
  )
  
  
  # ==========================================================
  # REPAIR GEOMETRIES
  # ==========================================================
  
  parcels_raw <- parcels_raw %>%
    mutate(
      geometry = fix_geom(
        st_geometry(.)
      )
    )
  
  
  # ==========================================================
  # DEDUPLICATE PARCELS
  # ==========================================================
  
  parcels_i <- parcels_raw %>%
    
    mutate(
      
      has_nup =
        !is.na(nup) &
        nup != "",
      
      geom_wkt =
        safe_wkt(.)
      
    ) %>%
    
    filter(
      !is.na(geom_wkt)
    ) %>%
    
    distinct(
      surface,
      geom_wkt,
      .keep_all = TRUE
    ) %>%
    
    select(
      -geom_wkt
    ) %>%
    
    mutate(
      !!id_col := v_id
    )
  
  
  message(
    "\n  kept ",
    nrow(parcels_i),
    " unique parcels"
  )
  
  
  # ==========================================================
  # SAVE COMPLETED VILLAGE
  # ==========================================================
  
  saveRDS(
    parcels_i,
    village_file
  )
  
  
  message(
    "  saved: ",
    village_file
  )
  
  
  # Also keep it in the current R session
  all_list[[i]] <- parcels_i
}



### VID1 =  291 - no parcel geometries returned
### VID1 =  292 - no parcel geometries returned
### VID1 =  293 - no parcel geometries returned
### VID1 =  294 - no parcel geometries returned
### VID1 =  295 - 7 parcels
### VID1 =  296 - no parcel geometries returned
### VID1 =  298 - no parcel geometries returned
### VID1 =  299 - no parcel geometries returned
### VID1 =  300 - 1 parcel
### VID1 =  301 - 5 parcels
### VID1 =  302 - 16 parcels
### VID1 =  303 - 3 parcels
### VID1 =  304 - 6 parcels
### VID1 =  305 - 19 parcels
### VID1 =  306 - 48 parcels
### VID1 =  307 - no parcel geometries returned
### VID1 =  308 - no parcel geometries returned
### VID1 =  309 - no parcel geometries returned
### VID1 =  310 - no parcel geometries returned
### VID1 =  311 - 1 parcel
### VID1 =  312 - no parcel geometries returned
### VID1 =  313 - no parcel geometries returned
### VID1 =  314 - 8 parcels
### VID1 =  315 - 36 parcels
### VID1 =  316 - no parcel geometries returned
### VID1 =  317 - no parcel geometries returned
### VID1 =  318 - 17 parcels



# ============================================================
# COMBINE ALL COMPLETED VILLAGES
# ============================================================

all_list_complete <- compact(
  all_list
)


if (length(all_list_complete) > 0) {
  
  all_parcels <- do.call(
    rbind,
    all_list_complete
  )
  
  
  message(
    "\n========================================"
  )
  
  message(
    "Finished."
  )
  
  message(
    nrow(all_parcels),
    " total parcel records loaded."
  )
  
  
} else {
  
  all_parcels <- NULL
  
  message(
    "\nNo parcel records were collected."
  )
}