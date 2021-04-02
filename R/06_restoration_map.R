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

# from 03_current_layers.R
data(restorelyr)

# boundaries, form 01_current_layers
data(stpete)

cols <- list(
  `Coastal Uplands` = 'brown4', 
  `Freshwater Wetlands` = 'orange', 
  `Native Uplands` = 'darkgreen', 
  `Tidal Wetlands` = 'yellow'
  ) %>% 
  unlist

# opportunities map -------------------------------------------------------

restdat <- restdat_fun(restorelyr)

m <- mapview(restdat, zcol = 'HMPU_TARGETS', col.regions = cols, lwd = 0, homebutton = F, layer.name = '')

# save as html, takes about ten minutes and maxes out memory, but it works
mapshot(m, url = 'docs/rstmap.html', remove_controls = NULL)

# areas <- restdat %>%
#   mutate(
#     acres = st_area(.),
#     acres = set_units(acres, 'acres')
#   ) %>%
#   st_set_geometry(NULL) %>%
#   group_by(HMPU_TARGETS) %>%
#   summarise(
#     acres = sum(acres, na.rm = T)
#   )

# st pete only ------------------------------------------------------------

restdat <- restdat_fun(restorelyr, stpete)

m <- mapview(restdat, zcol = 'HMPU_TARGETS', col.regions = cols, lwd = 0, homebutton = F, layer.name = '')

# save as html, takes about ten minutes and maxes out memory, but it works
mapshot(m, url = 'docs/rstmap_stpete.html', remove_controls = NULL)
