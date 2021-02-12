library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)

prj <- 4326

# LULC current status -----------------------------------------------------

# # from HMPU deliverables 
# lulcdat <- raster('~/Desktop/rasters/rasters/Full_LULC.tif')
# lulcdat <- readAll(lulcdat)
# 
# dat_crp <- lulcdat %>% 
#   st_as_stars %>% 
#   st_as_sf(as_points = FALSE, merge = TRUE) %>% 
#   st_transform(crs = prj) %>% 
#   rename(FLUCCSCODE = 'Full_LULC')

lulcfl <- 'lulc2017'
subtfl <- 'sgdat2018'

load(here('data', paste0(lulcfl, '.RData')))
load(here('data', paste0(subtfl, '.RData')))

# lulc area, all categories
lulc <- lulcfl %>%
  get %>% 
  mutate(
    aream2 = st_area(.),
    aream2 = as.numeric(aream2),
    areaac = aream2 / 4047
  ) %>%
  left_join(fluccs, by = 'FLUCCSCODE')

# lulc summarize, table categories
lulcsum <- lulc %>%
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Acres = sum(areaac))

# subtidal area, all categories
subt <- subtfl %>%
  get %>% 
  mutate(
    aream2 = st_area(.),
    aream2 = as.numeric(aream2),
    areaac = aream2 / 4047
  ) %>%
  left_join(fluccs, by = 'FLUCCSCODE')

# subtidal summarize, table categories
subtsum <- subt %>%
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Acres = sum(areaac))

# # use existing lulc