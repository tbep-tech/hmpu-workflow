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
library(maptiles)
library(tidyterra)
library(ggspatial)
library(tbeptools)

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
data(clrwt)
data(hilco)
data(pinco)
data(manco)
data(pasco)

cols <- list(
  `Existing Conservation Native` = 'yellowgreen', 
  `Existing Conservation Restorable` = 'green4', 
  `Proposed Conservation Native` = 'dodgerblue1', 
  `Proposed Conservation Restorable` = 'dodgerblue4', 
  `Reservation Native` = 'violetred1', 
  `Reservation Restorable` = 'violetred3'
  ) %>% 
  unlist

# complete watershed ------------------------------------------------------

oppdat <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal)

# m <- mapview(oppdat, zcol = 'cat', col.regions = cols, lwd = 0, homebutton = F, layer.name = '')
# 
# # save as html, takes about ten minutes and maxes out memory, but it works
# mapshot(m, url = 'docs/oppmap.html', remove_controls = NULL)

st_write(oppdat, 'data/shapefiles/oppmap.shp', delete_layer = TRUE)

oppdat <- st_read('data/shapefiles/oppmap.shp')

p <- oppmap_fun(oppdat, tbshed, ttl = 'Tampa Bay Watershed Combined Opportunities', northloc = 'tr', scaleloc = 'tl')

png('docs/oppmap.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# tampa only --------------------------------------------------------------

oppdat_tampa <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, tampa)

st_write(oppdat_tampa, 'data/shapefiles/oppmap_tampa.shp', delete_layer = TRUE)

oppdat_tampa <- st_read('data/shapefiles/oppmap_tampa.shp')

p <- oppmap_fun(oppdat_tampa, tampa, ttl = 'Tampa Combined Opportunities', northloc = 'tl', scaleloc = 'br')

png('docs/oppmap_tampa.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# st pete only ------------------------------------------------------------

oppdat_stpet <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, stpet)

st_write(oppdat_stpet, 'data/shapefiles/oppmap_stpet.shp', delete_layer = TRUE)

oppdat_stpet <- st_read('data/shapefiles/oppmap_stpet.shp')

p <- oppmap_fun(oppdat_stpet, stpet, 'St. Petersburg Combined Opportunities', northloc = 'tl', scaleloc = 'br')

png('docs/oppmap_stpet.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# clearwater only ---------------------------------------------------------

oppdat_clrwt <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, clrwt)

st_write(oppdat_clrwt, 'data/shapefiles/oppmap_clrwt.shp', delete_layer = TRUE)

oppdat_clrwt <- st_read('data/shapefiles/oppmap_clrwt.shp')

p <- oppmap_fun(oppdat_clrwt, clrwt, 'Clearwater Combined Opportunities', northloc = 'tl', scaleloc = 'br')

png('docs/oppmap_clrwt.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# hillsborough co only ----------------------------------------------------

oppdat_hilco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, hilco)

st_write(oppdat_hilco, 'data/shapefiles/oppmap_hilco.shp', delete_layer = TRUE)

oppdat_hilco <- st_read('data/shapefiles/oppmap_hilco.shp')

p <- oppmap_fun(oppdat_hilco, hilco, ttl = 'Hillsborough Co. Combined Opportunities', northloc = 'tl', scaleloc = 'br', buffdist = 2e4)

png('docs/oppmap_hilco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# pinellas co only --------------------------------------------------------

oppdat_pinco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, pinco)

st_write(oppdat_pinco, 'data/shapefiles/oppmap_pinco.shp', delete_layer = TRUE)

oppdat_pinco <- st_read('data/shapefiles/oppmap_pinco.shp')

p <- oppmap_fun(oppdat_pinco, pinco, ttl = 'Pinellas Co. Combined Opportunities', northloc = 'bl', scaleloc = 'br')

png('docs/oppmap_pinco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# manatee co only ---------------------------------------------------------

oppdat_manco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, manco)

st_write(oppdat_manco, 'data/shapefiles/oppmap_manco.shp', delete_layer = TRUE)

oppdat_manco <- st_read('data/shapefiles/oppmap_manco.shp')

p <- oppmap_fun(oppdat_manco, manco, ttl = 'Manatee Co. Combined Opportunities', northloc = 'bl', scaleloc = 'tl', buffdist = 2e4)

png('docs/oppmap_manco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# pasco co only -----------------------------------------------------------

oppdat_pasco <- oppdat_fun(nativersrv, restorersrv, nativelyr, restorelyr, coastal, pasco)

st_write(oppdat_pasco, 'data/shapefiles/oppmap_pasco.shp', delete_layer = TRUE)

oppdat_pasco <- st_read('data/shapefiles/oppmap_pasco.shp')

p <- oppmap_fun(oppdat_pasco, pasco, ttl = 'Pasco Co. Combined Opportunities', northloc = 'tl', scaleloc = 'tr', buffdist = 2e4)

png('docs/oppmap_pasco.png', height = 5, width = 8, res = 300, units = 'in')
p
dev.off()

# zip all shapefiles and upload to google drive ---------------------------

fls <- list.files('data/shapefiles/', pattern = '^oppmap', full.names = T)
zip('data/shapefiles/opportunity-layers', fls)

# https://drive.google.com/drive/folders/1CsuYXCpFzSJAPdHHfdKDZgYkXFeHes6y?usp=sharing/
# not working as of 5/8/25, uploaded manually
drive_upload('data/shapefiles/opportunity-layers.zip', 
             path = '1CsuYXCpFzSJAPdHHfdKDZgYkXFeHes6y', overwrite = T)
