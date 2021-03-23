library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)

source(here('R', 'funcs.R'))

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

# from 01_inputs
data(hard)
data(arti)
data(tidt)
data(livs)

# from 03_current_layers
data(nativelyr)
data(restorelyr)

# current lulc summary ----------------------------------------------------

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

# subtidal area, all categories
subtsum <- subtfl %>%
  get %>% 
  subt_est(fluccs)

# hard bottom
hardsum <- hard %>% 
  mutate(
    HMPU_TARGETS = 'Hard Bottom'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres))

# artificial reefs
artisum <- arti %>% 
  mutate(
    HMPU_TARGETS = 'Artificial Reefs'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres))

# tidal tributaries
tidtsum <- tidt %>% 
  mutate(
    HMPU_TARGETS = 'Tidal Tributaries'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Miles = sum(Miles))

# living shorelines
livssum <- livs %>% 
  mutate(
    HMPU_TARGETS = 'Living Shorelines'
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(HMPU_TARGETS) %>%
  summarise(Miles = sum(Miles))

# current summary
cursum <- bind_rows(lulcsum, subtsum, hardsum, artisum, tidtsum, livssum) %>% 
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


# native summary ----------------------------------------------------------

# native summary
nativesum <- nativelyr %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, acres), 
    Acres = as.numeric(Acres)
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(typ, HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres), .groups = 'drop') %>% 
  arrange(typ, HMPU_TARGETS) %>% 
  spread(typ, Acres)

# restorable summary ------------------------------------------------------

# restorable summary
restoresum <- restorelyr %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, acres),
    Acres = as.numeric(Acres)
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(typ, HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres), .groups = 'drop') %>% 
  arrange(typ, HMPU_TARGETS) %>% 
  spread(typ, Acres)

# combine all for table ---------------------------------------------------

# all summary
allsum <- cursum %>% 
  left_join(nativesum, by = 'HMPU_TARGETS')