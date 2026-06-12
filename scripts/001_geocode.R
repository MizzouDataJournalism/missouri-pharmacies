library(tidyverse)
library(readxl)
library(janitor)
library(ggmap)
library(maps)

### Class A pharmacies file provided by the state, current as of April 23, 2026:

pharmacies <- read_excel("original-data/Pharmacy - Active Pharmacies Class A-2026-04-23-11-49-00.xlsx") %>% 
  clean_names()

# Combine address fields into a single full address for geocoding:
pharmacies <- pharmacies %>%
  mutate(full_address = str_c(account_business_street, account_business_city, account_business_state_province, account_business_zip_postal_code, sep = ", "))

# Geocode addresses
# This step creates columns `lon` and `lat` using ggmap's `mutate_geocode()` which requires a Geocoding API from Google. 
pharmacies_geocoded <- pharmacies %>%
  mutate_geocode(full_address)

# This process created several warnings with imprecise lat/lon. 
# We stored these warnings and then checked (by hand) all of these pharmacies, updating their coordinates (when necessary) manually:  
warnings <- warnings()
writeLines(capture.output(warnings), "warnings.txt")

# Also checked for NAs in lat/lon
pharmacies_geocoded %>% filter(is.na(lat) | is.na(lon))

pharmacies_geocoded <- pharmacies_geocoded %>%
  mutate(
    lat = case_when(
      license_number == "2017006073" ~ 37.1078953500947,
      license_number == "2017040311" ~ 37.15593696138575,
      license_number == "2022041911" ~ 36.64551803673904,
      license_number == "2020032962" ~ 38.81857499690073,
      license_number == "2017035330" ~ 39.209851898045514,
      license_number == "2017040311" ~ 37.15593696138575,
      license_number == "2022041911" ~ 36.64551803673904,
      license_number == "2020032962" ~ 38.81857499690073,
      license_number == "2017035330" ~ 39.209851898045514,
      license_number == "2021026359" ~ 37.816750309215394,
      license_number == "001693" ~ 38.636556099562696,
      license_number == "005741" ~ 36.88433688111297,
      license_number == "003756" ~ 38.25117313092361,
      license_number == "2001019081" ~ 38.95343544869547,
      license_number == "2006038493" ~ 39.04507349214287,
      license_number == "2001000061" ~ 39.068189708636005,
      license_number == "2002009442" ~ 37.63451138264696,
      license_number == "2006016043" ~ 38.996789045141604,
      license_number == "2012025803" ~ 38.75746749302948,
      license_number == "2015043628" ~ 38.620198013834965,
      .default = lat),
    lon = case_when(
      license_number == "2017006073" ~ -92.58146194706495,
      license_number == "2017040311" ~ -93.41208991924601,
      license_number == "2022041911" ~ -94.45217630518886,
      license_number == "2020032962" ~ -91.14238183388706,
      license_number == "2017035330" ~ -94.68598502036922,
      license_number == "2021026359" ~ -92.21861007627967,
      license_number == "001693" ~ -90.26083555481107,
      license_number == "005741" ~ -89.57930963797918,
      license_number == "003756" ~ -93.36728804741603,
      license_number == "2001019081" ~ -92.383634432027,
      license_number == "2006038493" ~ -94.44290286509268,
      license_number == "2001000061" ~ -94.58093840927071,
      license_number == "2002009442" ~ -91.54605047443994,
      license_number == "2006016043" ~ -94.47809259763427,
      license_number == "2012025803" ~ -90.75551339312598,
      license_number == "2015043628" ~ -90.51691961353556,
      .default = lon))

write_csv(pharmacies_geocoded, "data/pharmacies_geocoded.csv")
