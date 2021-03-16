library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)

source('R/funcs.R')

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)
strata <- read.csv(here('data', 'strata.csv'), stringsAsFactors = F)

data(hard)
data(arti)
data(tidt)
data(livs)
data(strats)

lulcfl <- 'lulc2017'
subtfl <- 'sgdat2018'

load(here('data', paste0(lulcfl, '.RData')))
load(here('data', paste0(subtfl, '.RData')))

# LULC summarize ----------------------------------------------------------

# # from HMPU deliverables 
# lulcdat <- raster('~/Desktop/rasters/rasters/Full_LULC.tif')
# lulcdat <- readAll(lulcdat)
# 
# dat_crp <- lulcdat %>% 
#   st_as_stars %>% 
#   st_as_sf(as_points = FALSE, merge = TRUE) %>% 
#   rename(FLUCCSCODE = 'Full_LULC')

# coastal stratum
coastal <- strats %>% 
  dplyr::filter(Stratum %in% 'Coastal') %>% 
  st_buffer(dist = 0)

# lulc area, all categories
lulcsum <- get(lulcfl) %>% 
  add_coast_up(coastal, fluccs) %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, 'Acres'),
    Acres = as.numeric(Acres)
  ) %>% 
  st_set_geometry(NULL) %>% 
  group_by(Category) %>% 
  summarise(
    Acres = sum(Acres)
  ) %>% 
  select(HMPU_TARGETS = Category, Acres) %>% 
  arrange(HMPU_TARGETS)

  
# subtidal summarize ------------------------------------------------------

# subtidal area, all categories
subt <- subtfl %>%
  get %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, 'acres'),
    Acres = as.numeric(Acres)
  ) %>%
  left_join(fluccs, by = 'FLUCCSCODE')

# subtidal summarize, table categories
subtsum <- subt %>%
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres))

# miscellaneous sum -------------------------------------------------------

hardsum <- hard %>% 
  mutate(
    HMPU_TARGETS = 'Hard Bottom'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres))

artisum <- arti %>% 
  mutate(
    HMPU_TARGETS = 'Artificial Reefs'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres))

tidtsum <- tidt %>% 
  mutate(
    HMPU_TARGETS = 'Tidal Tributaries'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Miles = sum(Miles))

livssum <- livs %>% 
  mutate(
    HMPU_TARGETS = 'Living Shorelines'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Miles = sum(Miles))

# combined current extent --------------------------------------------------

curex <- bind_rows(lulcsum, subtsum, artisum, tidtsum, livssum) %>% 
  mutate(
    unis = case_when(
      is.na(Acres) ~ 'mi', 
      is.na(Miles) ~ 'ac'
    ), 
    `Current Extent` = case_when(
      is.na(Acres) ~ Miles, 
      is.na(Miles) ~ Acres
    )
  ) %>%
  inner_join(strata, by = 'HMPU_TARGETS') %>% 
  select(Category, HMPU_TARGETS, unis, `Current Extent`) %>% 
  arrange(Category, HMPU_TARGETS)


# existing and proposed conservation --------------------------------------

data(prop)
data(cons)

prop <- st_cast(prop, 'POLYGON')
cons <- st_cast(cons, 'POLYGON')

# coastal stratum
coastal <- strats %>% 
  dplyr::filter(Stratum %in% 'Coastal') %>% 
  st_buffer(dist = 0)

# lulc
lulc <- get(lulcfl) %>% 
  add_coast_up(coastal, fluccs) %>% 
  filter(!Category %in% c('Developed', 'Open Water'))

categories <- unique(lulc$Category)

propopp <- NULL
consopp <- NULL
for(cats in categories){
  
  cat(cats, '\n')
  
  tmp <- lulc %>% 
    filter(Category %in% cats) %>% 
    st_geometry() %>% 
    st_union() %>%
    st_cast('POLYGON')
  
  propout <- st_intersection(tmp, prop) %>% 
    st_sf(geometry = .) %>% 
    mutate(
      Category = cats
    )
  
  consout <- st_intersection(tmp, cons) %>% 
    st_sf(geometry = .) %>% 
    mutate(
      Category = cats
    )
  
  propopp <- bind_rows(propopp, propout)
  consout <- bind_rows(consopp, consout)
  
}

# uplands <- lulc %>% 
#   dplyr::filter(HMPU_TARGETS %in% 'Native Uplands') %>% 
#   st_buffer(dist = 0)
# 
# coastal_uplands <- st_intersection(uplands, coastal) %>% 
#   st_union() %>% 
#   st_geometry() %>% 
#   st_buffer(dist = 0) 
# 
# 
# tmp2 <- st_cast(coastal_uplands, 'POLYGON')
# 
# coastal_uplands_p <- st_intersection(tmp1, tmp2)
# 
# mapview(prop, col.regions = 'green') + mapview(coastal_uplands, col.regions = 'red') + mapview(tmp)

