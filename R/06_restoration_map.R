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
library(ggmap)
library(ggsn)
library(tbeptools)

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

# m <- mapview(restdat, zcol = 'HMPU_TARGETS', col.regions = cols, lwd = 0, homebutton = F, layer.name = '')
# 
# # save as html, takes about ten minutes and maxes out memory, but it works
# mapshot(m, url = 'docs/rstmap.html', remove_controls = NULL)

st_write(restdat, 'data/shapefiles/restmap.shp', delete_layer = TRUE)

restdat <- st_read('data/shapefiles/restmap.shp')

p <- restmap_fun(restdat, tbshed, ttl = 'Tampa Bay Watershed Restoration Potential', northloc = 'topright', scaleloc = 'topleft', 
                stsz = 2, buffdist = 0.04, scldst = 6, stht = 0.01)

png('docs/restmap.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# tampa only --------------------------------------------------------------

restdat_tampa <- restdat_fun(restorelyr, tampa)

st_write(restdat_tampa, 'data/shapefiles/restmap_tampa.shp', delete_layer = TRUE)

restdat_tampa <- st_read('data/shapefiles/restmap_tampa.shp')

p <- restmap_fun(restdat_tampa, tampa, ttl = 'Tampa Restoration Potential', northloc = 'topleft', scaleloc = 'bottomright', 
                stsz = 2, buffdist = 0.025, scldst = 3, stht = 0.02)

png('docs/restmap_tampa.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# st pete only ------------------------------------------------------------

restdat_stpet <- restdat_fun(restorelyr, stpet)

st_write(restdat_stpet, 'data/shapefiles/restmap_stpet.shp', delete_layer = TRUE)

restdat_stpet <- st_read('data/shapefiles/restmap_stpet.shp')

p <- restmap_fun(restdat_stpet, stpet, 'St. Petersburg Restoration Potential', northloc = 'topleft', scaleloc = 'bottomright',
                stsz = 2, buffdist = 0.02, scldst = 3, stht = 0.02)

png('docs/restmap_stpet.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# clearwater only ---------------------------------------------------------

restdat_clrwt <- restdat_fun(restorelyr, clrwt)

st_write(restdat_clrwt, 'data/shapefiles/restmap_clrwt.shp', delete_layer = TRUE)

restdat_clrwt <- st_read('data/shapefiles/restmap_clrwt.shp')

p <- restmap_fun(restdat_clrwt, clrwt, 'Clearwater Restoration Potential', northloc = 'topleft', scaleloc = 'bottomright', 
                stsz = 2.5, buffdist = 0.01, scldst = 3, stht = 0.02)

png('docs/restmap_clrwt.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# hilco only --------------------------------------------------------------

restdat_hilco <- restdat_fun(restorelyr, hilco)

st_write(restdat_hilco, 'data/shapefiles/restmap_hilco.shp', delete_layer = TRUE)

restdat_hilco <- st_read('data/shapefiles/restmap_hilco.shp')

p <- restmap_fun(restdat_hilco, hilco, ttl = 'Hillsborough Co. Restoration Potential', northloc = 'bottomright', scaleloc = 'topleft', 
                stsz = 2, buffdist = 0.025, scldst = 5, stht = 0.01)

png('docs/restmap_hilco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# pinco only --------------------------------------------------------------

restdat_pinco <- restdat_fun(restorelyr, pinco)

st_write(restdat_pinco, 'data/shapefiles/restmap_pinco.shp', delete_layer = TRUE)

restdat_pinco <- st_read('data/shapefiles/restmap_pinco.shp')

p <- restmap_fun(restdat_pinco, pinco, ttl = 'Pinellas Co. Restoration Potential', northloc = 'bottomleft', scaleloc = 'topright', 
                stsz = 1.8, buffdist = 0.02, scldst = 3, stht = 0.01)

png('docs/restmap_pinco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# manco only --------------------------------------------------------------

restdat_manco <- restdat_fun(restorelyr, manco)

st_write(restdat_manco, 'data/shapefiles/restmap_manco.shp', delete_layer = TRUE)

restdat_manco <- st_read('data/shapefiles/restmap_manco.shp')

p <- restmap_fun(restdat_manco, manco, ttl = 'Manatee Co. Restoration Potential', northloc = 'bottomleft', scaleloc = 'topleft', 
                stsz = 2.5, buffdist = 0.025, scldst = 3, stht = 0.015)

png('docs/restmap_manco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# pasco only --------------------------------------------------------------

restdat_pasco <- restdat_fun(restorelyr, pasco)

st_write(restdat_pasco, 'data/shapefiles/restmap_pasco.shp', delete_layer = TRUE)

restdat_pasco <- st_read('data/shapefiles/restmap_pasco.shp')

p <- restmap_fun(restdat_pasco, pasco, ttl = 'Pasco Co. Restoration Potential', northloc = 'topleft', scaleloc = 'topright', 
                stsz = 1.8, buffdist = 0.04, scldst = 5, stht = 0.02)

png('docs/restmap_pasco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# zip all shapefiles and upload to google drive ---------------------------

fls <- list.files('data/shapefiles/', pattern = '^restmap', full.names = T)
zip('data/shapefiles/restoration-layers', fls)

drive_upload('data/shapefiles/restoration-layers.zip', 
             path = 'https://drive.google.com/drive/folders/1CsuYXCpFzSJAPdHHfdKDZgYkXFeHes6y?usp=sharing/', overwrite = T)

