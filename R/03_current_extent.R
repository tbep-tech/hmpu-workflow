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
data(prop)
data(exst)
data(coastal)
data(soils)
data(salin)

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

# lulc area, all categories
lulcsum <- get(lulcfl) %>% 
  lulc_est(coastal, fluccs)
  
# subtidal summarize ------------------------------------------------------

# subtidal area, all categories
subtsum <- subtfl %>%
  get %>% 
  subt_est(fluccs)

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

curex <- bind_rows(lulcsum, subtsum, hardsum, artisum, tidtsum, livssum) %>% 
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


# existing, proposed conservation and restoration opps --------------------

# lulc
lulc <- get(lulcfl) %>% 
  add_coast_up(coastal, fluccs) %>% 
  filter(!HMPU_TARGETS %in% c('Developed', 'Open Water'))

categories <- unique(lulc$HMPU_TARGETS)

propall <- NULL
exstall <- NULL
for(cats in categories){
  
  cat(cats, '\n')
  
  tmp <- lulc %>% 
    filter(HMPU_TARGETS %in% cats) %>% 
    st_geometry() %>% 
    st_union() %>%
    st_cast('POLYGON')
  
  propout <- st_intersection(tmp, prop) %>% 
    st_sf(geometry = .) %>% 
    mutate(
      HMPU_TARGETS = cats, 
      typ = 'Proposed'
    )
  
  exstout <- st_intersection(tmp, exst) %>% 
    st_sf(geometry = .) %>% 
    mutate(
      HMPU_TARGETS = cats, 
      typ = 'Existing'
    )
  
  propall <- bind_rows(propall, propout)
  exstall <- bind_rows(exstall, exstout)
  
}

# layer 
nativelyr <- bind_rows(propall, exstall) %>% 
  filter(!HMPU_TARGETS %in% 'Restorable') %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, acres), 
    Acres = as.numeric(Acres)
  ) 

# summary
nativesum <- nativelyr %>% 
  st_set_geometry(NULL) %>%
  group_by(typ, HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres), .groups = 'drop') %>% 
  arrange(typ, HMPU_TARGETS) %>% 
  spread(typ, Acres)

## restorable

restorable <- bind_rows(propall, exstall) %>% 
  filter(HMPU_TARGETS %in% 'Restorable') 

# xeric soils
soilsforest <- soils %>% 
  filter(gridcode == '100') %>% 
  st_union() %>% 
  st_geometry() %>% 
  st_cast('POLYGON') %>% 
  st_buffer(dist = 0)

# mesic/hydric soils
soilswetland <- soils %>% 
  filter(!gridcode == '100') %>% 
  st_union() %>% 
  st_geometry() %>% 
  st_cast('POLYGON') %>% 
  st_buffer(dist = 0)

opps <- NULL
for(typ in c('Proposed', 'Existing')){

  cat(typ, '\n')
  
  ##
  # subset proposed, existing restorable habitats
  tmp <- restorable %>% 
    filter(typ %in% !!typ) %>% 
    st_union() %>%
    st_geometry() %>%
    st_cast('POLYGON') %>% 
    dplyr::select(typ) %>% 
    st_buffer(dist = 0)
  
  ##
  # create
  
  # restorable in xeric soils
  uplands <- st_intersection(tmp, soilsforest)
  
  # restorable in xeric soils, in coastal stratum
  coastal_uplands <- st_intersection(uplands, coastal)
    
  # remove coastal uplands from uplands
  uplands <- st_difference(uplands, coastal_uplands)
  
  # wetlands
  wetlands <- st_intersection(tmp, soilswetland)
  
  # tidal wetlands
  tidal_wetlands <- st_intersection(wetlands, coastal)
  
  # remove tidal wetlands from wetlands
  wetlands <- st_difference(wetlands, tidal_wetlands)
  
  ##
  # add attributes
  
  uplands <- uplands %>% 
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Native Uplands')
  
  coastal_uplands <- coastal_uplands %>% 
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Coastal Uplands')
  
  wetlands <- wetlands %>% 
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Freshwater Wetlands')
  
  tidal_wetlands <- tidal_wetlands %>% 
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Salt Marshes')
  
  out <- bind_rows(uplands, coastal_uplands, wetlands, tidal_wetlands) %>% 
    mutate(typ = !!typ)
  
  opp <- rbind(opp, out)
  
}



# combine all -------------------------------------------------------------

cursum <- curex %>% 
  left_join(native, by = 'HMPU_TARGETS')
