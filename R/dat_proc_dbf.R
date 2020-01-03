library(tidyverse)
library(here)
library(foreign)

prj <- 4326

fluccs <- tibble(
  descriptor = c('Mangrove Swamps', 'Salt Marshes', 'Salt Barrens', 'Streams and Waterways', 'Lakes', 'Wetland Hardwood Forests', 'Wetland Coniferous Forests',
                 'Wetland Forested Mixed', 'Vegetated Non-Forested Wetlands', 'Dry Prairie', 'Shrub and Brushland', 'Mixed Rangeland', 'Upland Coniferous Forests',
                 'Upland Hardwood Forests', 'Upland Hardwood Forests'),
  category = c('Tidal Wetlands', 'Tidal Wetlands', 'Tidal Wetlands', 'Freshwater Wetlands', 'Freshwater Wetlands', 'Freshwater Wetlands', 'Freshwater Wetlands',
               'Freshwater Wetlands', 'Freshwater Wetlands', 'Native Uplands', 'Native Uplands', 'Native Uplands', 'Native Uplands', 'Native Uplands',
               'Native Uplands'),
  FLUCCSCODE = c('6120', '6420', '6600', '5100', '5200', '6100', '6200', '6300', '6400', '3100', '3200', '3300', '4100', '4200', '4300')
)

# https://data-swfwmd.opendata.arcgis.com/search?groupIds=880fc95697ce45c3a8b078bb752faf40
dbfs <- list(
  `2017` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/2017_LULC_Clipped_Dissolve.dbf',
  `2014` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/2014_LULC_Clipped_Dissolve.dbf',
  `2011` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/2011_LULC_Clipped_Dissolve.dbf',
  `2007` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/2007_LULC_Clipped_Dissolve.dbf',
  `2004` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/2004_LULC_Clipped_Dissolve.dbf',
  `1999` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/1999_LULC_Clipped_Dissolve.dbf',
  `1995` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/1995_LULC_Clipped_Dissolve.dbf',
  `1990` = 'T:/04_STAFF/ED/04_DATA/2020_HMPU_Analysis/Reprojected/1990_LULC_Clipped_Dissolve.dbf'
)

# process through each year
acresdbf <- dbfs %>%
  enframe %>%
  mutate(
    ests = purrr::map(value, function(x){
      
      cat(x, '\n')
      dat_raw <- read.dbf(x)
      
      # get area
      dat_are <- dat_raw %>%
        filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>% 
        group_by(FLUCCSCODE) %>% 
        group_by(FLUCCSCODE) %>%
        summarise(
          Acres = sum(Acres)
        )
      
      return(dat_are)
      
    })
  ) %>% 
  select(-value) %>% 
  unnest(ests)

save(acresdbf, file = here('data', 'acresdbf.RData'), compress = 'xz')
