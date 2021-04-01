library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)

source(here('R', 'funcs.R'))

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

data(prop)
data(exst)
data(coastal)
data(soils)
data(salin)
data(strata)

# current year
lulcfl <- 'lulc2017'
load(here('data', paste0(lulcfl, '.RData')))

# existing, proposed conservation and restoration opps --------------------

# lulc
lulc <- get(lulcfl) %>% 
  add_coast_up(coastal, fluccs) %>% 
  filter(!HMPU_TARGETS %in% c('Developed', 'Open Water'))

categories <- unique(lulc$HMPU_TARGETS)

propall <- NULL
exstall <- NULL
for(cats in categories){
  
  cat(cats, '\n')
  
  tmp <- lulc %>% 
    filter(HMPU_TARGETS %in% cats)  %>% 
    fixgeo
  
  propout <- st_intersection(tmp, prop) %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(
      HMPU_TARGETS = cats, 
      typ = 'Proposed'
    )
  
  exstout <- st_intersection(tmp, exst) %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(
      HMPU_TARGETS = cats, 
      typ = 'Existing'
    )
  
  propall <- bind_rows(propall, propout)
  exstall <- bind_rows(exstall, exstout)
  
}

# native existing, proposed layer -----------------------------------------

# native existing, proposed conservation layer 
nativelyr <- bind_rows(propall, exstall) %>% 
  filter(!HMPU_TARGETS %in% 'Restorable')

# restorable existing, proposed layer -------------------------------------

restorable <- bind_rows(propall, exstall) %>% 
  filter(HMPU_TARGETS %in% 'Restorable') 

# xeric soils
soilsforest <- soils %>% 
  filter(gridcode == '100') %>% 
  fixgeo

# mesic/hydric soils
soilswetland <- soils %>% 
  filter(!gridcode == '100') %>% 
  fixgeo

# low salinity (Juncus)
salinlo <- salin %>% 
  filter(Descrip == '0.5-18') %>% 
  fixgeo

restorelyr <- NULL
for(typ in c('Proposed', 'Existing')){

  cat(typ, '\n')
  
  ##
  # subset proposed, existing restorable habitats
  tmp <- restorable %>% 
    filter(typ %in% !!typ) %>% 
    fixgeo
  
  ##
  # create
  
  # restorable in xeric soils
  uplands <- st_intersection(tmp, soilsforest)
  
  # restorable in xeric soils, in coastal stratum
  coastal_uplands <- st_intersection(uplands, coastal)
    
  # remove coastal uplands from uplands
  uplands <- coastal_uplands %>% 
    st_union() %>% 
    st_difference(uplands, .)
  
  # wetlands
  wetlands <- st_intersection(tmp, soilswetland)
  
  # tidal wetlands
  tidal_wetlands <- st_intersection(wetlands, coastal)
  
  # remove tidal wetlands from wetlands
  wetlands <- tidal_wetlands %>% 
    st_union() %>% 
    st_difference(wetlands, .)
  
  # salt marshes (low salinity juncus)
  salt_marshes <- st_intersection(tidal_wetlands, salinlo)
  
  # remove salt marshes from tidal wetlands
  tidal_wetlands <- st_difference(tidal_wetlands, salt_marshes)
  
  ##
  # add attributes, fix geometries
  
  uplands <- uplands %>% 
    fixgeo() %>% 
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Native Uplands')
  
  coastal_uplands <- coastal_uplands %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Coastal Uplands')
  
  wetlands <- wetlands  %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Freshwater Wetlands')
  
  tidal_wetlands <- tidal_wetlands %>% 
    fixgeo %>%  
    st_sf(geometry = .) %>% 
    mutate(HMPU_TARGETS = 'Mangrove Forests/Salt Barrens')
  
  salt_marshes <- salt_marshes %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>%  
    mutate(HMPU_TARGETS = 'Salt Marshes')
  
  out <- bind_rows(uplands, coastal_uplands, wetlands, tidal_wetlands, salt_marshes) %>% 
    st_zm(drop = TRUE) %>% 
    mutate(typ = !!typ)
  
  restorelyr <- bind_rows(restorelyr, out)
  
}

# reservation layers ------------------------------------------------------

# layers in coastal stratum that are native or restorable and currently not in existing conservation
# this includes lands in proposed conservation or unprotected status

# existing conservation (native and restorable), unioned for difference
uniexstall <- exstall %>% 
  st_union()

# native currently proposed
nativersrv <- nativelyr %>% 
  filter(typ %in% 'Proposed') %>% 
  st_intersection(., coastal) %>% 
  fixgeo 

# restorable currently proposed
restorersrv <- restorelyr %>% 
  filter(typ %in% 'Proposed') %>% 
  st_intersection(., coastal) %>% 
  fixgeo 

# unprotected native in coastal
nativeunpro <- lulc %>% 
  filter(!HMPU_TARGETS %in% 'Restorable') %>% 
  st_intersection(., coastal) %>% 
  fixgeo %>% 
  st_difference(., uniexstall) %>% 
  fixgeo

# unprotected reservation in coastal
restoreunpro <- lulc %>%
  filter(HMPU_TARGETS %in% 'Restorable') %>% 
  st_intersection(., coastal) %>% 
  fixgeo %>% 
  st_difference(., uniexstall) %>% 
  fixgeo

restorersrv <- c(st_make_valid(restorersrv), restoreunpro)
restorersrv <- fixgeo(restorersrv)
nativersrv <- c(st_make_valid(nativersrv), nativeunpro)
nativersrv <- fixgeo(nativersrv)

# mapview(restorersrv, col.regions = 'violetred3', lwd = 0) + mapview(nativersrv, col.regions = 'violetred1', lwd = 0)

save(nativelyr, file = 'data/nativelyr.RData', compress = 'xz')
save(restorelyr, file = 'data/restorelyr.RData', compress = 'xz')
save(nativersrv, file = 'data/nativersrv.RData')
save(restorersrv, file = 'data/restorersrv.RData')
