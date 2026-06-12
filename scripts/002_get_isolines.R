
# Using hereR API to get 30 and 15- minute isolines

library(hereR)
library(sf)
library(tidyverse)

# ── 1. Load pharmacy points ──────────────────────────────────
pharmacies <- read_csv("data/pharmacies_geocoded.csv")


# categorizing by pharmacy chain (walgreens, cvs and other (can add more))

pharmacies <- pharmacies %>% mutate(group = case_when(
  grepl("cvs", dba_name, ignore.case = T) ~ "CVS", 
  grepl("walgreen", dba_name, ignore.case = T) ~ "Walgreens", 
  .default = "Other"
))

# Convert to sf in WGS 84 — HERE API requires 4326 for requests
pharmacies_sf <- st_as_sf(
  pharmacies,
  coords = c("lon", "lat"),
  crs    = 4326,
  remove = FALSE          # keep lon/lat columns
)


# Making 15 minute isolines

SLEEP_SEC <- 1
isoline_list <- vector("list", nrow(pharmacies_sf))

for (i in seq_len(nrow(pharmacies_sf))) {
  message(sprintf("[%d/%d] Requesting isoline for: %s",
                  i, nrow(pharmacies_sf), pharmacies_sf$dba_name[i]))
  
  iso <- isoline(
    poi = pharmacies_sf[i, ],
    range = 15 * 60, # or 30, instead of 15
    range_type = "time",
    routing_mode = "fast",
    transport_mode = "car",
    traffic = FALSE,
    optimize = "balanced",
    aggregate = TRUE
  )
  
  # ADD THIS LINE:
  iso$license_number <- pharmacies_sf$license_number[i]
  
  isoline_list[[i]] <- iso
  
  if (i < nrow(pharmacies_sf)) Sys.sleep(SLEEP_SEC)
}

isolines <- do.call(rbind, isoline_list)

# ── Joining other data ──
isolines <- isolines %>%
  left_join(
    pharmacies_sf %>%
      st_drop_geometry() %>%
      select(license_number, dba_name, full_address, lon, lat, group),
    by = "license_number"
  ) %>%
  mutate(drive_min = 30)

# ── Verify it worked ──
if(any(is.na(isolines$dba_name))) {
  warning("Some isochrones failed to join!")
} else {
  message("✓ All isochrones successfully joined with pharmacy data")
}


# Saving output 

st_write(isolines, "data/isolines_15_min.geojson", delete_dsn = TRUE)

st_write(isolines, "data/isolines_30_min.geojson", delete_dsn = TRUE)

