
library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)

prj <- 4326

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

# watershed ---------------------------------------------------------------

tbshed <- st_read('~/Desktop/ConvertedPolygon/Model_Input/TBEP_Watershed_Correct_Projection.shp') %>% 
  st_transform(prj)

save(tbshed, file = here('data', 'tbshed.RData'), compress = 'xz')

# coastal stratum ---------------------------------------------------------

# strat <- st_read('~/Desktop/ConvertedPolygon/Model_Input/Reservation.shp')

# soils -------------------------------------------------------------------

soils <- st_read('~/Desktop/ConvertedPolygon/Model_Input/SoilsForRestoration.shp') %>% 
  st_transform(prj)

save(soils, file = here('data', 'soils.RData'), compress = 'xz')

# salinity ----------------------------------------------------------------

salin <- st_read('~/Desktop/ConvertedPolygon/Model_Input/SalinityKrigMerge.shp') %>% 
  st_transform(prj)

save(salin, file = here('data', 'salin.RData'), compress = 'xz')

# import each lulc layer, crop by tbshed, save ----------------------------

data (tbshed)

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
  ) %>%
  enframe 

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

foreach(i = 1:nrow(urls), .packages = c('tidyverse', 'sf', 'here', 'tbeptools')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(urls), '\n')
  print(Sys.time()-strt)
  sink()
  
  # import file
  dat_raw <- urls[i, ] %>% 
    pull(value) %>% 
    .[[1]] %>% 
    st_read
  
  # crop by watershed and select fluccs
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    select(FLUCCSCODE) %>%
    filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>%
    st_buffer(dist = 0) %>% 
    st_intersection(tbshed)
  
  nm <- paste0('lulc', urls$name[i])
  assign(nm, dat_crp)
  
  save(list = nm, file = paste0('data/', nm, '.RData'), compress = 'xz')
  
}

# import each subtidal layer, crop by tbshed, save ------------------------

data(tbshed)

# https://data-swfwmd.opendata.arcgis.com/search?groupIds=d9a4213eb9ea4713bb710e03bdcc6648
urls <- list(
  # `2018` = 'https://opendata.arcgis.com/datasets/8d0d473468924423bf0f1682aaca790f_0.geojson',
  # `2016` = 'https://opendata.arcgis.com/datasets/f0ecff0cf0de491685f8fb074adb278b_20.geojson',
  # `2014` = 'https://opendata.arcgis.com/datasets/f530f972ded749adb1c6b20c2651e7f9_18.geojson',
  # `2012` = 'https://opendata.arcgis.com/datasets/619bd267e4c54e70968abd86eb92318e_17.geojson',
  # `2010` = 'https://opendata.arcgis.com/datasets/82153be25a3340a0abdb3ec713425f29_16.geojson',
  # `2008` = 'https://opendata.arcgis.com/datasets/4ddc60a8c9f845a2912c4e7cb14a3b7b_15.geojson',
  # `2006` = 'https://opendata.arcgis.com/datasets/5a72bbd64bc9486696fa0bc47ca4e30c_13.geojson',
  # `2004` = 'https://opendata.arcgis.com/datasets/bb6b117c8eab40209d8125c3c95f6150_12.geojson',
  `2001` = 'https://opendata.arcgis.com/datasets/e2ce063712f34654a4f371240f541479_11.geojson', 
  `1999` = 'https://opendata.arcgis.com/datasets/e27b6e5148514f29a1f1483813297fd7_10.geojson', 
  # `1996` = 'https://opendata.arcgis.com/datasets/38f62dd9b6e5482888b2c0bb51716b6e_9.geojson',
  # `1994` = 'https://opendata.arcgis.com/datasets/a2fb9d100cfd441cbdd24b16a3b0ce53_8.geojson',
  `1992` = 'https://opendata.arcgis.com/datasets/ea9fab53f2f74236b0cba8980dffe363_7.geojson',
  # `1990` = 'https://opendata.arcgis.com/datasets/bcc955216c62468c9a6dafffc0545a40_6.geojson',
  `1988` = 'https://opendata.arcgis.com/datasets/092df867ece945b787557c9a7cf811d8_5.geojson'
  ) %>% 
  enframe 

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

res <- foreach(i = 1:nrow(urls), .packages = c('tidyverse', 'sf', 'here')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(urls), '\n')
  print(Sys.time()-strt)
  sink()
  
  # import file
  dat_raw <- urls[i, ] %>% 
    pull(value) %>% 
    .[[1]] %>% 
    st_read
  
  if('FLUCCS_CODE' %in% names(dat_raw))
    dat_raw <- dat_raw %>% 
      rename(FLUCCSCODE = FLUCCS_CODE)
  
  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    dplyr::select(FLUCCSCODE) %>% 
    filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>%
    st_buffer(dist = 0) %>% 
    st_intersection(tbshed)
  
  # name assignment and save
  flnm <- paste0('sgdat', urls$name[i])
  assign(flnm, dat_crp)
  save(list = flnm, file = here('data', paste0('/', flnm, '.RData')), compress = 'xz')
  
}

# opportunities map from deliverables -------------------------------------
# 
# library(raster)
# library(sf)
# library(tidyverse)
# library(stars)
# 
# oppdat <- raster('~/Desktop/rasters/rasters/HMPU_additivehybrid.tif')
# oppdat <- readAll(oppdat)
# 
# # vals <- getValues(tmp)
# 
# cls <- list(
#   `100` = 'Not-considered Native', # x
#   `101` = 'Protected Native', # 'Existing Conservation Native'
#   `104` = 'Proposed Native', # 'Proposed Consevation Native'
#   `150` = 'Reserve Not-considered Native', 
#   `154` = 'Reserve Proposed Native', # 'Reservation Native'
#   `201` = 'Protected Restorable', # 'Existing Conservation Restorable'
#   `204` = 'Proposed Restorable', # 'Proposed Conservation Restorable'
#   `254` = 'Reserve Proposed Restorable', # 'Reservation Restorable'
#   `300` = 'Developed', # x
#   `400` = 'Open Water', # x
#   `401` = 'Sub-tidal', # x
#   `999` = 'Not-considered' # x
# )
# 
# oppdat[!oppdat[] %in% c(101, 104, 154, 201, 204, 254)] <- NA
# oppdat <- oppdat %>% 
#   st_as_stars %>% 
#   st_as_sf(as_points = FALSE, merge = TRUE) %>% 
#   dplyr::rename(code = HMPU_additivehybrid) %>% 
#   dplyr::mutate(
#     cat = case_when(
#       code == 101 ~ 'Existing Conservation Native', 
#       code == 104 ~ 'Proposed Conservation Native', 
#       code == 150 ~ 'Reservation Not Native', 
#       code == 154 ~ 'Reservation Native', 
#       code == 201 ~ 'Existing Conservation Restorable', 
#       code == 204 ~ 'Proposed Conservation Restorable', 
#       code == 254 ~ 'Reservation Restorable', 
#       T ~ NA_character_
#     )
#   )
# 
# save(oppdat, file= 'data/oppdat.RData', compress = 'xz')



