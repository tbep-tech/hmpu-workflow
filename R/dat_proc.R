# process geojson --------------------------------------------------------

library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(tbeptools)
library(foreign)

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
  enframe 

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

res <- foreach(i = 1:nrow(acres), .packages = c('tidyverse', 'sf', 'here', 'tbeptools')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(acres), '\n')
  print(Sys.time()-strt)
  sink()
 
  prj <- 4326
  
  fluccs <- read.csv(here('data-raw', 'FLUCCShabsclass.csv'), stringsAsFactors = F)
  
  # import file
  dat_raw <- acres[i, ] %>% 
    pull(value) %>% 
    .[[1]] %>% 
    st_read
  
  # crop by watershed and select fluccs
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    select(FLUCCSCODE) %>%
    filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>%
    st_intersection(tbshed)
  
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
      Acres = sum(areaac)
    )
  
  # summarise by categories, remove subtidal first for watershed area
  dat_out <- dat_are %>% 
    left_join(fluccs, by = 'FLUCCSCODE') %>% 
    filter(!HMPU_DESCRIPTOR %in% c('Algae', 'Continuous_Seagrass', 'Hard_Bottom', 'Estuary', 'Oyster_Bars', 'Patchy_Seagrass', 'Subtidal', 'Tidal_Flats')) %>% 
    select(-FLUCCSCODE, -FIRST_FLUC) %>% 
    gather('var', 'val', -Acres) %>% 
    group_by(var, val) %>% 
    summarise(
      areaac = sum(Acres)
    )
  
  return(dat_out)
      
}

acresjso <- res %>% 
  enframe %>% 
  bind_cols(acres, .) %>% 
  select(name, value1) %>% 
  unnest(value1)

# manually add salt barren ests from ESA
sltbrn <- tibble(
  name = c('1990', '1995', '1999'), 
  var = 'HMPU_DESCRIPTOR', 
  val = 'Salt_Barrens', 
  areaac = c(468,479, 492)
)

acresjso <- acresjso %>% 
  bind_rows(sltbrn) %>% 
  arrange(name, var, val)

save(acresjso, file = here('data', 'acresjso.RData'), compress = 'xz')

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
        filter(!HMPU_DESCRIPTOR %in% c('Algae', 'Continuous_Seagrass', 'Hard_Bottom', 'Estuary', 'Oyster_Bars', 'Patchy_Seagrass', 'Subtidal', 'Tidal_Flats')) %>% 
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

# manually add salt barren ests from ESA
sltbrn <- tibble(
  name = c('1990', '1995', '1999'), 
  var = 'HMPU_DESCRIPTOR', 
  val = 'Salt_Barrens', 
  areaac = c(468,479, 492)
)

acresdbf <- acresdbf %>% 
  bind_rows(sltbrn) %>% 
  arrange(name, var, val)

save(acresdbf, file = here('data', 'acresdbf.RData'), compress = 'xz')

# save change data dbf from network ---------------------------------------

chgdat <- read.dbf('T:/05_GIS/HMPU/comp1990v2017/TBEP_dbasinsg_LU9017.dbf')
save(chgdat, file = 'data/chgdat.RData', compress = 'xz')