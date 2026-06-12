library(sf)
library(tidyverse)
library(ggplot2)
library(rmapshaper)


# --- 1. Importing detailed results and county outlines -----------------------

results_30 <- st_read("data/detailed_results_30_min.geojson")

results_15 <- st_read("data/detailed_results_15_min.geojson")

county <- st_read("data/mo_counties.geojson")
county <- st_transform(county, 26997)


# --- 2. Selecting necessary info -----------------------

results_30_map <- results_30 %>% select(geometry, GEOID, NAMELSAD, pop_not_covered)

results_30_map <- results_30_map %>% mutate(county = str_sub(GEOID, 1, 5))

id <- county %>% select(GEOID, NAMELSAD) %>% st_drop_geometry() 

id <- id %>% rename(name = NAMELSAD)

results_30_map <- results_30_map %>% left_join(id, by=c("county" = "GEOID"))

results_30_map <- results_30_map %>% 
  unite("NAME", name, NAMELSAD, sep=", ") %>% 
  select(-county)

results_30_map <- results_30_map %>% mutate(pop_not_covered = round(pop_not_covered))


# --- 3. Making a map -----------------------

# Create categories based on increments of 350
results_30_map <- results_30_map %>%
  mutate(
    pop_category = case_when(
      pop_not_covered == 0 ~ "0",
      pop_not_covered >= 1 & pop_not_covered <= 349 ~ "1-349",
      pop_not_covered >= 350 & pop_not_covered <= 699 ~ "350-699",
      pop_not_covered >= 700 & pop_not_covered <= 1049 ~ "700-1049",
      pop_not_covered >= 1050 ~ "1050-1399"
    )
  )



# Map with your exact colors
ggplot() +
  geom_sf(data = results_30_map, aes(fill = pop_category), color = NA) +
  geom_sf(data = county, fill = NA, color = "white", size = 1) +
  scale_fill_manual(
    values = c(
      "0" = "#d1d3d4",
      "1-349" = "#eeb09a",
      "350-699" = "#e58475",
      "700-1049" = "#dd5951",
      "1050-1399" = "#d42d2c"
    )
  ) +
  theme_void() 

ggsave("mapfor30.svg", width = 12, height = 10)



# repeating the process for 15 minute isolines

# --- 2. Selecting necessary info -----------------------

results_15_map <- results_15 %>% select(geometry, GEOID, NAMELSAD, pop_not_covered)

results_15_map <- results_15_map %>% mutate(county = str_sub(GEOID, 1, 5))

id <- county %>% select(GEOID, NAMELSAD) %>% st_drop_geometry() 

id <- id %>% rename(name = NAMELSAD)

results_15_map <- results_15_map %>% left_join(id, by=c("county" = "GEOID"))

results_15_map <- results_15_map %>% 
  unite("NAME", name, NAMELSAD, sep=", ") %>% 
  select(-county)

results_15_map <- results_15_map %>% mutate(pop_not_covered = round(pop_not_covered))


# --- 3. Making a map -----------------------

# Create categories based on increments of 350
results_15_map <- results_15_map %>%
  mutate(
    pop_category = case_when(
      pop_not_covered == 0 ~ "0",
      pop_not_covered >= 1 & pop_not_covered <= 1549 ~ "1-1549",
      pop_not_covered >= 1550 & pop_not_covered <= 3099 ~ "1550-3099",
      pop_not_covered >= 3100 & pop_not_covered <= 4649 ~ "3100-4649",
      pop_not_covered >= 4650 ~ "4650-6200"
    )
  )

results_15_map %>% distinct(pop_category)


# Map with your exact colors
ggplot() +
  geom_sf(data = results_15_map, aes(fill = pop_category), color = NA) +
  geom_sf(data = county, fill = NA, color = "white", size = 1) +
  scale_fill_manual(
    values = c(
      "0" = "#d1d3d4",
      "1-1549" = "#eeb09a",
      "1550-3099" = "#e58475",
      "3100-4649" = "#dd5951",
      "4650-6200" = "#d42d2c"
    )
  ) +
  theme_void() 

ggsave("mapfor15.svg", width = 12, height = 10)
