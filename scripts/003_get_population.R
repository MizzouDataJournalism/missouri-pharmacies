
# Getting MO population (block groups) from the Census API
# Getting land area from tigris
# Getting county outlines for mapping

library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)


# ── Get Census Population Data in block groups ─────────────────────────

# Get Missouri population by census block group: this requires a free Census API key, stored in your environment (https://api.census.gov/data/key_signup.html)
# Using 2024 ACS 5-year estimates for most recent data

mo_population_data <- get_acs(
  geography = "block group",
  variables = "B01003_001",  # Total population
  state = "MO",
  year = 2024,
  geometry = FALSE,  # We will get geometry from tigris
)


# LAND AREA DATA

# ── 6b. Get Land Area data for the block groups ─────────────────────────
# using tigris for the geometry because tigris includes ALAND and AWATER columns
mo_block_groups <- block_groups(
  state = "MO",
  year = 2024,
  class = "sf"
)


# ── 6c. Join population to block groups ──────────────────────

mo_population <- mo_block_groups %>%
  left_join(
     select(mo_population_data, GEOID, estimate, moe),
    by = "GEOID"
  ) %>%
  rename(value = estimate) %>% 
  # Calculate area metrics
  mutate(
    # Convert ALAND and AWATER to sf units for consistency
    land_area_sqm = as.numeric(ALAND),
    water_area_sqm = as.numeric(AWATER),
    total_area_sqm = land_area_sqm + water_area_sqm,
    
    # Calculate percentage of block group that is water
    pct_water = water_area_sqm / total_area_sqm,
    
    # Convert to proper units for spatial calculations
    land_area = units::set_units(land_area_sqm, "m^2")
  )



# ── Export files to GeoJSON ─────────────────────────────────────

st_write(mo_population, "data/mo_population_block.geojson", delete_dsn = TRUE)



# ── Getting county outlines for mapping ───────────────────────── 

mo_counties <- counties(
  state = "MO",
  year = 2024,
  class = "sf"
)

st_write(mo_counties, "data/mo_counties.geojson", delete_dsn = TRUE)


