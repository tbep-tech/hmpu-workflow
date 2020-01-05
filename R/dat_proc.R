# process geojson --------------------------------------------------------

library(sf)
library(tidyverse)
library(patchwork)
library(here)

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

shed <- st_read('T:/05_GIS/BOUNDARIES/TBEP_Watershed_Correct_Projection.shp') %>%
  st_transform(crs = prj)

# https://data-swfwmd.opendata.arcgis.com/search?groupIds=880fc95697ce45c3a8b078bb752faf40
urls <- list(
  `2017` = 'https://opendata.arcgis.com/datasets/bedb342c692d4be6891b899e4cf7f4a6_1.geojson',
  `2014` = 'https://opendata.arcgis.com/datasets/e8cefca7d0d94fbaaa6b8dfa5403d984_0.geojson',
  `2011` = 'https://opendata.arcgis.com/datasets/f325a3417c92444d9cba838154d6fa0d_11.geojson',
  `2007` = 'https://opendata.arcgis.com/datasets/bec356d2a2204e15ac2385e80a1dd198_7.geojson',
  `2004` = 'https://opendata.arcgis.com/datasets/c6c5b47e178b44ff90ea777c57748a1f_4.geojson',
  `1999` = 'https://opendata.arcgis.com/datasets/f3352c512a904eb694d3a3e04fc275a1_3.geojson',
  `1995` = 'https://opendata.arcgis.com/datasets/ca1b5d1bff5c4459a28e93dc92d39413_2.geojson',
  `1990` = 'https://opendata.arcgis.com/datasets/5be9f0bf951f45379e43466198988673_1.geojson'
)

# process through each year
acres <- urls %>%
  enframe %>%
  mutate(
    ests = purrr::map(value, function(x){
      
      cat(x, '\n')
      dat_raw <- st_read(x)
      
      # crop by watershed and select fluccs
      dat_crp <- dat_raw %>%
        st_transform(crs = prj) %>%
        select(FLUCCSCODE) %>%
        filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>%
        st_intersection(shed)
      
      # get area
      dat_are <- dat_crp %>%
        mutate(
          aream2 = st_area(.),
          aream2 = as.numeric(aream2),
          areaac = aream2 / 4047
        ) %>%
        st_set_geometry(NULL) %>%
        group_by(FLUCCSCODE) %>%
        summarise(
          areaac = sum(areaac)
        )
      
      return(dat_are)
      
    })
  )

save(acres, file = here('data', 'acres.RData'), compress = 'xz')

# process local dbf -------------------------------------------------------

library(tidyverse)
library(here)
library(foreign)

prj <- 4326

fluccs <- read.csv(here('data-raw', 'FLUCCShabsclass.csv'), stringsAsFactors = F)
                   
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
        select(FLUCCSCODE, Acres) %>% 
        filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>% 
        left_join(fluccs, by = 'FLUCCSCODE') %>% 
        select(-FLUCCSCODE, -FIRST_FLUC) %>% 
        gather('var', 'val', -Acres) %>% 
        group_by(var, val) %>% 
        summarise(
          areaac = sum(Acres)
        )

      return(dat_are)
      
    })
  ) %>% 
  select(-value) %>% 
  unnest(ests)

save(acresdbf, file = here('data', 'acresdbf.RData'), compress = 'xz')

