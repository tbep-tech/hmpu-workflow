library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)
library(flextable)

source(here('R', 'funcs.R'))

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

data(hard)
data(arti)
data(tidt)
data(livs)
data(prop)
data(exst)
data(coastal)
data(soils)
data(salin)
data(strata)

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

nativesum <- nativelyr %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, acres), 
    Acres = as.numeric(Acres), 
    typ = paste('native', typ)
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(typ, HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres), .groups = 'drop') %>% 
  arrange(typ, HMPU_TARGETS) %>% 
  spread(typ, Acres)

# restorable summary ------------------------------------------------------

restoresum <- restorelyr %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, acres),
    Acres = as.numeric(Acres),
    typ = paste('restorable', typ)
  ) %>% 
  st_set_geometry(NULL) %>%
  group_by(typ, HMPU_TARGETS) %>%
  summarise(Acres = sum(Acres), .groups = 'drop') %>% 
  arrange(typ, HMPU_TARGETS)

# create duplicate rows for non-specific targets
duplab1 <- 'Mangrove Forests/Salt Barrens'
dups1 <- restoresum %>% 
  filter(HMPU_TARGETS %in% !!duplab1) %>% 
  mutate(HMPU_TARGETS = 'Mangrove Forests')
duplab2 <- 'Freshwater Wetlands'
dups2 <- restoresum %>% 
  filter(HMPU_TARGETS %in% !!duplab2) %>% 
  mutate(HMPU_TARGETS = 'Non-Forested Freshwater Wetlands')

restoresum <- restoresum %>% 
  bind_rows(dups1) %>%
  bind_rows(dups2) %>% 
  mutate(
    HMPU_TARGETS = case_when(
      HMPU_TARGETS %in% !!duplab1 ~ 'Salt Barrens',
      HMPU_TARGETS %in% !!duplab2 ~ 'Forested Freshwater Wetlands', 
      T ~ HMPU_TARGETS
    )
  ) %>% 
  spread(typ, Acres) %>% 
  mutate(
    `total restorable` = `restorable Existing` + `restorable Proposed`
  )

# combine all for table ---------------------------------------------------

# all summary
allsum <- cursum %>% 
  left_join(nativesum, by = 'HMPU_TARGETS') %>% 
  left_join(restoresum, by = 'HMPU_TARGETS') %>% 
  gather('var', 'val', -Category, -HMPU_TARGETS, -unis) %>% 
  mutate(
    val = case_when(
      !is.na(val) ~ paste(prettyNum(round(val, 0), big.mark = ','), unis),
      T ~ NA_character_
    ), 
    Category = factor(Category, levels = c('Subtidal', 'Intertidal', 'Supratidal')), 
    HMPU_TARGETS = factor(HMPU_TARGETS, levels = levels(strata$HMPU_TARGETS))
  ) %>% 
  spread(var, val) %>% 
  dplyr::select(-unis) %>% 
  mutate(
    `native Existing` = case_when(
      Category == 'Subtidal' ~ `Current Extent`, 
      HMPU_TARGETS == 'Living Shorelines' ~ 'LSSM',
      T ~ `native Existing`
    ), 
    `total restorable` = case_when(
      HMPU_TARGETS == 'Seagrasses' ~ '14,131 ac', 
      HMPU_TARGETS %in% c('Tidal Flats', 'Oyster Bars') ~ 'I/D',
      HMPU_TARGETS %in% c('Living Shorelines', 'Tidal Tributaries') ~ 'LSSM', 
      T ~ `total restorable`
    ),
    `restorable Existing` = case_when(
      HMPU_TARGETS == 'Seagrasses' ~ '14,131 ac', 
      HMPU_TARGETS %in% c('Tidal Flats', 'Oyster Bars') ~ 'I/D',
      T ~ `restorable Existing`
    )
  ) %>% 
  select(
    `Habitat Type` = HMPU_TARGETS, 
    `Current Extent`, 
    `Existing Conservation Lands` = `native Existing`, 
    `Proposed Conservation Lands` = `native Proposed`, 
    `Total Restoration Opportunity` = `total restorable`, 
    `Existing Conservation Lands Restoration Opportunity` = `restorable Existing`, 
    `Proposed Conservation Lands Restoration Opportunity` = `restorable Proposed`
  )

flextable(allsum) %>% 
  merge_at(i = 7:8, j = 5, part = 'body') %>% 
  merge_at(i = 7:8, j = 6, part = 'body') %>%
  merge_at(i = 7:8, j = 7, part = 'body') %>%
  merge_at(i = 12:13, j = 5, part = 'body') %>% 
  merge_at(i = 12:13, j = 6, part = 'body') %>%
  merge_at(i = 12:13, j = 7, part = 'body') %>%
  # add_header_row(colwidths = 14)
  border_inner_h() %>% 
  border_inner_v()
