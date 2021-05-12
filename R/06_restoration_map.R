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

# from 03_current_layers.R
data(restorelyr)

# boundaries, form 01_current_layers
data(tampa)
data(stpet)
data(clrwt)
data(hilco)
data(pinco)
data(manco)
data(pasco)

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

st_write(restdat, 'data/shapefiles/restmap.shp', delete_layer = TRUE)

# tampa only --------------------------------------------------------------

restdat_tampa <- restdat_fun(restorelyr, tampa)

st_write(restdat_tampa, 'data/shapefiles/restmap_tampa.shp', delete_layer = TRUE)

# st pete only ------------------------------------------------------------

restdat_stpet <- restdat_fun(restorelyr, stpet)

st_write(restdat_stpet, 'data/shapefiles/restmap_stpet.shp', delete_layer = TRUE)

# clearwater only ---------------------------------------------------------

restdat_clrwt <- restdat_fun(restorelyr, clrwt)

st_write(restdat_clrwt, 'data/shapefiles/restmap_clrwt.shp', delete_layer = TRUE)

# hilco only --------------------------------------------------------------

restdat_hilco <- restdat_fun(restorelyr, hilco)

st_write(restdat_hilco, 'data/shapefiles/restmap_hilco.shp', delete_layer = TRUE)

# pinco only --------------------------------------------------------------

restdat_pinco <- restdat_fun(restorelyr, pinco)

st_write(restdat_pinco, 'data/shapefiles/restmap_pinco.shp', delete_layer = TRUE)

# manco only --------------------------------------------------------------

restdat_manco <- restdat_fun(restorelyr, manco)

st_write(restdat_manco, 'data/shapefiles/restmap_manco.shp', delete_layer = TRUE)

# pasco only --------------------------------------------------------------

restdat_pasco <- restdat_fun(restorelyr, pasco)

st_write(restdat_pasco, 'data/shapefiles/restmap_pasco.shp', delete_layer = TRUE)

# zip all shapefiles and upload to google drive ---------------------------

fls <- list.files('data/shapefiles/', pattern = '^restmap', full.names = T)
zip('data/shapefiles/restoration-layers', fls)

drive_upload('data/shapefiles/restoration-layers.zip', 
             path = 'https://drive.google.com/drive/folders/1CsuYXCpFzSJAPdHHfdKDZgYkXFeHes6y?usp=sharing/', overwrite = T)

