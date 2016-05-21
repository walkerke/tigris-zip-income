library(readr)
library(stringr)
library(dplyr)
library(tigris)

# Read in the IRS data
irs_data <- 'https://www.irs.gov/pub/irs-soi/13zpallnoagi.csv'

df <- read_csv(irs_data) %>%
  mutate(zip_str = str_pad(as.character(ZIPCODE), width = 5, side = 'left', pad = '0'), 
         incpr = A02650 / N02650) %>%
  select(zip_str, incpr)

zips <- zctas(cb = TRUE)

metros <- core_based_statistical_areas(cb = TRUE)

metros_metro <- metros[metros$LSAD == 'M1' & !grepl('PR', metros$NAME), ]

zips_joined <- geo_join(zips, df, "ZCTA5CE10", "zip_str")

saveRDS(metros_metro, 'metros.rds')

saveRDS(zips_joined, 'zips.rds')
