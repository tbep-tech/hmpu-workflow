library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)
library(mapview)
library(grid)
library(htmlwidgets)
library(googledrive)

source(here('R', 'funcs.R'))

# opportunities map -------------------------------------------------------

# from 03_current_layers.R
data(restorelyr)
data(nativelyr)
data(restorersrv)
data(nativersrv)
data(coastal)

# boundaries, form 01_current_layers
data(tampa)
data(stpet)
data(hilco)
data(pinco)
data(manco)
data(pasco)

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

st_write(oppdat, 'data/shapefiles/oppmap.shp', delete_layer = TRUE)

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

# tampa only --------------------------------------------------------------

oppdat_tampa <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, tampa)

st_write(oppdat_tampa, 'data/shapefiles/oppmap_tampa.shp', delete_layer = TRUE)

# st pete only ------------------------------------------------------------

oppdat_stpet <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, stpet)

st_write(oppdat_stpet, 'data/shapefiles/oppmap_stpet.shp', delete_layer = TRUE)

# hillsborough co only ----------------------------------------------------

oppdat_hilco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, hilco)

st_write(oppdat_hilco, 'data/shapefiles/oppmap_hilco.shp', delete_layer = TRUE)

# pinellas co only --------------------------------------------------------

oppdat_pinco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, pinco)

st_write(oppdat_pinco, 'data/shapefiles/oppmap_pinco.shp', delete_layer = TRUE)

# manatee co only ---------------------------------------------------------

oppdat_manco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, manco)

st_write(oppdat_manco, 'data/shapefiles/oppmap_manco.shp', delete_layer = TRUE)

# pasco co only -----------------------------------------------------------

oppdat_pasco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, pasco)

st_write(oppdat_pasco, 'data/shapefiles/oppmap_pasco.shp', delete_layer = TRUE)

# zip all shapefiles and upload to google drive ---------------------------

fls <- list.files('data/shapefiles/', pattern = '^oppmap', full.names = T)
zip('data/shapefiles/opportunity-layers', fls)

drive_upload('data/shapefiles/opportunity-layers.zip', 
             path = 'https://drive.google.com/drive/folders/1CsuYXCpFzSJAPdHHfdKDZgYkXFeHes6y?usp=sharing/', overwrite = T)
