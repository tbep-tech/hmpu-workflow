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
data(nativelyr)
data(restorersrv)
data(nativersrv)
data(coastal)

# boundaries, form 01_current_layers
data(stpete)

cols <- list(
  `Existing Conservation Native` = 'yellowgreen', 
  `Existing Conservation Restorable` = 'green4', 
  `Proposed Conservation Native` = 'dodgerblue1', 
  `Proposed Conservation Restorable` = 'dodgerblue4', 
  `Restoration Native` = 'violetred1', 
  `Restoration Restorable` = 'violetred3'
  ) %>% 
  unlist

# complete watershed ------------------------------------------------------

oppdat <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal)

m <- mapview(oppdat, zcol = 'cat', col.regions = cols, lwd = 0, homebutton = F, layer.name = '')

# save as html, takes about ten minutes and maxes out memory, but it works
mapshot(m, url = 'docs/oppmap.html', remove_controls = NULL)

# areas <- oppdat %>% 
#   mutate(
#     acres = st_area(.), 
#     acres = set_units(acres, 'acres')
#   ) %>% 
#   st_set_geometry(NULL) %>% 
#   group_by(cat) %>% 
#   summarise(
#     acres = sum(acres, na.rm = T)
#   )

# st pete only ------------------------------------------------------------

oppdat <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, stpete)

m <- mapview(oppdat, zcol = 'cat', col.regions = cols, lwd = 0, homebutton = F, layer.name = '')

# save as html, takes about ten minutes and maxes out memory, but it works
mapshot(m, url = 'docs/oppmap_stpete.html', remove_controls = NULL)

# # opportunities map from deliverables -------------------------------------
# 
# library(raster)
# library(sf)
# library(tidyverse)
# library(stars)
# 
# oppdat <- raster('~/Desktop/TBEP/HMPU/GIS/rasters/rasters/HMPU_additivehybrid.tif')
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
# oppdatold <- oppdat
# save(oppdatold, file= 'data/oppdatold.RData', compress = 'xz')
# 
# areasold <- oppdatold %>% 
#   mutate(
#     acres = st_area(.), 
#     acres = set_units(acres, 'acres')
#   ) %>% 
#   st_set_geometry(NULL) %>% 
#   group_by(cat) %>% 
#   summarise(
#     acres = sum(acres, na.rm = T)
#   )
# 
# plot(acres.x ~ acres.y, data = toplo)
# abline(0, 1)