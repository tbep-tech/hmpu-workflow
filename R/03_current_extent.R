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
data(coastal)

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


# existing and proposed conservation --------------------------------------

data(prop)
data(exst)
data(coastal)

# lulc
lulc <- get(lulcfl) %>% 
  add_coast_up(coastal, fluccs) %>% 
  filter(!HMPU_TARGETS %in% c('Developed', 'Open Water'))

categories <- unique(lulc$HMPU_TARGETS)

propopp <- NULL
exstopp <- NULL
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
  
  propopp <- bind_rows(propopp, propout)
  exstout <- bind_rows(exstopp, exstout)
  
}

natvopp <- bind_rows(propopp, exstopp)
