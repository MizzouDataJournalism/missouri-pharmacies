
# Calculations (the same script for 30- and 15-minute isolines)

library(sf)
library(tidyverse)

# --- 1. Importing geocoded data -----------------------

mo_population <- st_read("data/mo_population_block.geojson")


#isolines <- st_read("data/isolines_30_min.geojson")

# or

isolines <- st_read("data/isolines_15_min.geojson")

# --- 2. Transform to same projection ──────────────────────────
# Use a projection appropriate for Missouri (NAD83 / Missouri Central)
mo_population <- st_transform(mo_population, 26997)
isolines <- st_transform(isolines, 26997)


# Update land_area to projected units
mo_population <- mo_population %>%
  mutate(
    land_area = units::set_units(land_area_sqm, "m^2")
  )


# ── 3. Combine all isochrones into one coverage area ─────────

combined_coverage <- isolines %>%
  st_union() %>%
  st_sf()


# ── 4. Calculate coverage USING LAND AREA ───────────────────

# Find intersections
intersections <- st_intersection(mo_population, combined_coverage)


# Calculate the area of each intersection
intersections <- intersections %>%
  mutate(covered_area_size = st_area(geometry))

# Summarize by GEOID (in case a block group is split)
coverage_summary <- intersections %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(covered_area_size = sum(covered_area_size))

# Join back to original data
mo_population <- mo_population %>%
  left_join(coverage_summary, by = "GEOID")

# Handle missing coverage
if(!"covered_area_size" %in% names(mo_population)) {
  message("WARNING: No intersections found! Adding covered_area_size column with zeros.")
  mo_population$covered_area_size <- units::set_units(0, "m^2")
}

# Calculate coverage percentages and population estimates
mo_population <- mo_population %>%
  mutate(
    # Replace NA (no coverage) with 0
    covered_area_size = replace_na(covered_area_size, units::set_units(0, "m^2")),
    
    # CRITICAL: Calculate percentage using LAND AREA, not total area
    # This excludes water bodies from the calculation
    pct_covered = as.numeric(covered_area_size / land_area),
    
    # Ensure percentage is between 0 and 1
    pct_covered = pmin(pct_covered, 1),
    pct_covered = pmax(pct_covered, 0),
    
    # Proportional allocation of population
    pop_covered = value * pct_covered,
    pop_not_covered = value * (1 - pct_covered),
    
    # Also calculate what the OLD method (total area) would have given
    # (for comparison purposes)
    total_geom_area = st_area(geometry),
    pct_covered_old_method = as.numeric(covered_area_size / total_geom_area),
    pct_covered_old_method = pmin(pct_covered_old_method, 1),
    pop_not_covered_old_method = value * (1 - pct_covered_old_method)
  )

# ── 5. Aggregate numbers ──────────────────────────────────────

results <- mo_population %>%
  st_drop_geometry() %>%
  summarise(
    total_population = sum(value, na.rm = TRUE),
    population_covered = sum(pop_covered, na.rm = TRUE),
    population_not_covered = sum(pop_not_covered, na.rm = TRUE),
    pct_not_covered = (population_not_covered / total_population) * 100,
    
    # Coverage statistics
    block_groups_total = n(),
    block_groups_fully_covered = sum(pct_covered == 1, na.rm = TRUE),
    block_groups_partially_covered = sum(pct_covered > 0 & pct_covered < 1, na.rm = TRUE),
    block_groups_not_covered = sum(pct_covered == 0, na.rm = TRUE)
  )


# RESULTS:

# 30-min isolines: estimated pop_not_covered = 55,019.46 (0.89% of total pop)
# 15-min isolines: estimated pop_not_covered = 808,024.8 (13.05% of total pop)


# ── 5. Exporting detailed results ──────────────────────────────────────


st_write(mo_population, "data/detailed_results_30_min.geojson", delete_dsn = TRUE)

st_write(mo_population, "data/detailed_results_15_min.geojson", delete_dsn = TRUE)



# --- 6. Knowing that block groups can have a high MOE, we checked the minimum calculations for pct covered / uncovered: 

# Calculate coverage percentages and population estimates for 
# MIN VALUE
mo_population_min_test <- mo_population %>%
  mutate(value_min = value - moe, .after = moe) %>% 
  mutate(
    # Proportional allocation of population
    pop_covered = value_min * pct_covered,
    pop_not_covered = value_min * (1 - pct_covered),
    ) %>%
  st_drop_geometry() %>%
  summarise(
    total_population = sum(value_min, na.rm = TRUE),
    population_covered = sum(pop_covered, na.rm = TRUE),
    population_not_covered = sum(pop_not_covered, na.rm = TRUE),
    pct_not_covered = (population_not_covered / total_population) * 100,
    
    # Coverage statistics
    block_groups_total = n(),
    block_groups_fully_covered = sum(pct_covered == 1, na.rm = TRUE),
    block_groups_partially_covered = sum(pct_covered > 0 & pct_covered < 1, na.rm = TRUE),
    block_groups_not_covered = sum(pct_covered == 0, na.rm = TRUE)
  )

# Note that the total pop is obviously well under the actual state population, and the uncovered is still ~600,000 for 15-minute
