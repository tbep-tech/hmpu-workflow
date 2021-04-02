library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)
library(mapview)
library(grid)
library(htmlwidgets)

source(here('R', 'funcs.R'))

# opportunities map -------------------------------------------------------

# from 03_current_layers.R
data(restorelyr)

restdat <- restorelyr %>% 
  filter(typ %in% 'Existing') %>% 
  mutate(
    HMPU_TARGETS = case_when(
      HMPU_TARGETS %in% c('Mangrove Forests/Salt Barrens', 'Salt Marshes') ~ 'Tidal Wetlands', 
      T ~ HMPU_TARGETS
    )
  ) %>% 
  select(HMPU_TARGETS)
    
cols <- list(
  `Coastal Uplands` = 'brown4', 
  `Freshwater Wetlands` = 'orange', 
  `Native Uplands` = 'darkgreen', 
  `Tidal Wetlands` = 'yellow'
  ) %>% 
  unlist

m <- mapview(restdat, zcol = 'HMPU_TARGETS', col.regions = cols, lwd = 0, homebutton = F, layer.name = '')

# save as html, takes about ten minutes and maxes out memory, but it works
mapshot(m, url = 'docs/rstmap.html', remove_controls = NULL)

