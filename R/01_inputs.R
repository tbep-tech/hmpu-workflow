library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(esri2sf) # yonghah/esri2sf on github
library(tbeptools)
library(units)

source(here('R', 'funcs.R'))

# NAD83(2011) / Florida West (ftUS)
# this is the projection used in original report
prj <- 6443

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

# current subtidal year for fwc oyster difference
subtfl <- 'sgdat2022'

# restoration targets lookup table ----------------------------------------

trgs <- data.frame(
  Category  = c("Subtidal", "Subtidal", "Subtidal", "Subtidal", "Subtidal",
                "Intertidal", "Intertidal", "Intertidal", "Intertidal", "Intertidal", "Intertidal", 
                "Supratidal", "Supratidal", "Supratidal", "Supratidal"),
  HMPU_TARGETS = c("Hard Bottom", "Artificial Reefs", "Tidal Flats", "Seagrasses", "Oyster Bars",
                   "Living Shorelines", "Total Intertidal", "Mangrove Forests", "Salt Barrens", "Salt Marshes", "Tidal Tributaries", 
                   "Coastal Uplands", "Non-Forested Freshwater Wetlands", "Forested Freshwater Wetlands", "Native Uplands"),
  `Target2030` = c(423, 166, 16220, 40000, 221,
                   21.3, 21353, 15300, 546, 4807, 391,
                   3769, 68937, 152282, 141050),
  `Goal2050` = c(423, 166, 16220, 40000, 471,
                   56.3, 23803, 15300, 796, 5457, 405,
                   4219, 71787, 152732, 142100),
  rationale = c('Protect existing hard bottom; continue to identify new hard bottom area using proven mapping techniques',
               'Protect existing artificial reefs; enhance habitat complexity where feasible; expand reef area to promote fish and wildlife benefits',
               'Identify and protect existing persistent tidal flats; assess restoration potential of other non-vegetated subtidal areas',
               'Protect existing seagrasses; establish new HMPU lower limit of 40,000 acres; assess restoration potential of non-vegetated subtidal areas',
               '2030: Protect existing oysters + restore 50 acres; increase target by 50 acres each out-decade; consider filtration rate to refine long-term goal; an oyster habitat suitability index (HSI) will inform opportunity space',
               '2030: Construct 1 mile of LS each year; includes privately owned seawalls; need better definition of opportunity areas; increase target to 1.5 & 2 miles per year for out decades',
               '2030: Protect existing intertidal mosaic + restore 1,000 acres (based on hydric soils); increase target by 150 acres each out-decade; includes the mosaic of mangrove, salt barren, and salt marsh habitats',
               'Protect existing mangrove forests; restore opportunistically within the intertidal mosaic',
               '2030: Protect existing salt barrens + restore 50 acres; increase target by 50 acres per out decade',
               '2030: Protect existing low salinity salt marshes + restore 250 acres; increase target by 50 acres each out-decade; significant land acquisition and/or Public Private Partnerships required to achieve this 2030 target and 2050 goal',
               'Inventory mapped tidal tributaries and assess/rank restoration potential; restore ~4 miles (1%) of urban tidal creek habitat where feasible; increase target by 2 miles per out decade',
               '2030: Protect existing coastal uplands + specifically restore 150 acres (upland restoration total = 600 acres); increase target by 50 acres each out decade',
               '2030: Protect existing non-forested freshwater wetlands + restore 1,350 acres; increase target by 50 acres each out decade',
               '2030: Protect existing forested freshwater wetlands + restore 150 acres; increase target by 50 acres each out decade',
               '2030: Protect existing native uplands + specifically restore 450 acres (upland restoration total = 600 acres); increase target by 50 acres each out decade; focus on pine flatwoods and protect current extent (56,717 acres)'
               ),
  stringsAsFactors = F
  ) %>% 
  mutate(
    Category = factor(Category, levels = c("Subtidal", "Intertidal", "Supratidal")), 
    HMPU_TARGETS = factor(HMPU_TARGETS, levels = HMPU_TARGETS)
  )

save(trgs, file = here('data', 'trgs.RData'), version = 2)

# stratification lookup table ---------------------------------------------

strata <- data.frame(
  Category  = c("Subtidal", "Subtidal", "Subtidal", "Subtidal", "Subtidal",
                "Intertidal", "Intertidal", "Intertidal", "Intertidal", "Intertidal", "Intertidal", 
                "Supratidal", "Supratidal", "Supratidal", "Supratidal"),
  HMPU_TARGETS = c("Hard Bottom", "Artificial Reefs", "Tidal Flats", "Seagrasses", "Oyster Bars", 
                   "Total Intertidal", "Mangrove Forests", "Salt Barrens", "Salt Marshes", "Living Shorelines", "Tidal Tributaries", 
                   "Coastal Uplands", "Non-Forested Freshwater Wetlands", "Forested Freshwater Wetlands", "Native Uplands"), 
  stringsAsFactors = F
  ) %>% 
  mutate(
    Category = factor(Category, levels = c("Subtidal", "Intertidal", "Supratidal")), 
    HMPU_TARGETS = factor(HMPU_TARGETS, levels = HMPU_TARGETS)
  )

save(strata, file = here('data', 'strata.RData'), version = 2)

# watershed ---------------------------------------------------------------

# tbshed <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/16') %>%
tbshed <- st_read('https://opendata.arcgis.com/datasets/537fc3e84ccf4f54b441fc4bc03b8b00_16.geojson') %>%
  st_transform(prj) %>% 
  st_union() %>% 
  st_buffer(dist = 0)

save(tbshed, file = here('data', 'tbshed.RData'), compress = 'xz')

# coastal stratum ---------------------------------------------------------

# coastal <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/14') %>% 
coastal <- st_read('https://opendata.arcgis.com/datasets/1ff3afee627b4f6883f462f4313a3b88_14.geojson') %>%
  dplyr::select(Stratum) %>% 
  st_transform(prj) %>% 
  dplyr::filter(Stratum %in% 'Coastal') %>% 
  st_buffer(dist = 0) %>% 
  st_geometry() %>% 
  st_union() %>% 
  st_cast('POLYGON')

save(coastal, file = here('data', 'coastal.RData'), compress = 'xz')

# soils -------------------------------------------------------------------

# soils <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/20') %>% 
soils <- st_read('https://opendata.arcgis.com/datasets/7b75ca38c1364b178f7ce41ef4195ed4_20.geojson') %>% 
  st_transform(prj)

save(soils, file = here('data', 'soils.RData'), compress = 'xz')

# salinity ----------------------------------------------------------------

# salin <- st_read('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/19') %>% 
salin <- st_read('https://opendata.arcgis.com/datasets/caed84eb6ece4c0db8dc5b5aff8ba0a6_19.geojson') %>% 
  st_transform(prj)

save(salin, file = here('data', 'salin.RData'), compress = 'xz')

# proposed and conservation lands ------------------------------------------

data(tbshed)

bbox <- sf::st_bbox(tbshed)

### conservation 

## from FNAI website

# URL links for these layers are from here: https://www.fnai.org/gisdata.cfm
# layers are also available through ESRI servers: https://geodata.fnai.org/

# FLMA, florida conservation lands
# remove Macdill (custom polygon from original layer)
flma <- esri2sf('https://services.arcgis.com/9Jk4Zl9KofTtvg3x/arcgis/rest/services/FL_Conservation_Lands_web/FeatureServer/0',
        bbox = bbox) %>% 
  dplyr::filter(!MANAME %in% 'MacDill Air Force Base') %>% 
  st_transform(st_crs(tbshed)) %>% 
  st_buffer(dist = 0) %>% 
  st_intersection(tbshed) %>% 
  st_union 

# FFBOT, future forever board of trustees projects
ffbot <- esri2sf('https://services.arcgis.com/9Jk4Zl9KofTtvg3x/arcgis/rest/services/Florida_Forever_BOT_Projects/FeatureServer/0', 
                 bbox = bbox) %>% 
  st_transform(st_crs(tbshed)) %>% 
  st_buffer(dist = 0) %>% 
  st_intersection(tbshed) %>% 
  st_union 

# FFA, future forever acquisitions
ffa <- esri2sf('https://services.arcgis.com/9Jk4Zl9KofTtvg3x/arcgis/rest/services/Florida_Forever_Acquisitions/FeatureServer/0', 
               bbox = bbox) %>% 
  st_transform(st_crs(tbshed)) %>% 
  st_buffer(dist = 0) %>% 
  st_intersection(tbshed) %>% 
  st_union

## aquatic preserves, from DEP

# from here https://geodata.dep.state.fl.us/datasets/81841412d3984e9aac2c00c21e41d32e_0

# this gets all preserves in watershed, then removes lake tarpon (not in map 3-1)
# note that the original layer has overlapping polygons
aqprs <- st_read('https://opendata.arcgis.com/datasets/81841412d3984e9aac2c00c21e41d32e_0.geojson') %>% 
  st_transform(crs = prj) %>%
  st_buffer(dist = 0) %>% 
  st_intersection(tbshed) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_cast("POLYGON") %>% 
  dplyr::filter(!grepl('\\.', row.names(.))) %>% 
  st_union() %>% 
  st_buffer(dist = 0)

## join existing conservation layers from sources
exst <- st_geometry(flma) %>% 
  st_union(st_geometry(ffbot)) %>% 
  st_union(st_geometry(ffa)) %>% 
  st_union(st_geometry(aqprs)) %>% 
  st_buffer(dist = 0)

## original existing conservation layer
# exstorig <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/3') %>% 
exstorig <- st_read('https://opendata.arcgis.com/datasets/e977c851f6dc49c48d0729b3cd30cc92_3.geojson') %>% 
  st_transform(prj) %>% 
  st_union() %>% 
  st_buffer(dist = 0) 

# union exstorig with exst, assumes exstorig will have nothing that already isn't in exst
exst <- st_union(st_geometry(exst), st_geometry(exstorig)) %>% 
  st_cast('POLYGON') %>% 
  st_union() %>% 
  st_buffer(dist = 0)

### proposed

# the CLIP data are a one off, so I can just use the original layer from HMPU
# original from here https://www.fnai.org/clip.cfm, but HMPU added some by hand

# st_layers(gdb)
# prop <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/10') %>% 
prop <- st_read('https://opendata.arcgis.com/datasets/ba464bae7a1144f09522b459d297d1ef_10.geojson') %>%
  st_transform(prj) %>% 
  st_geometry() %>% 
  st_union() %>% 
  st_buffer(dist = 0) 

# mapview(cons, col.regions = 'green') + mapview(prop, col.regions = 'red')

### correct overlap between conservation and proposed
# assumes anything that's proposed that overlaps with conservation is now conservation

# this takes the overlap of proposed with conservation, puts it in conservation, removes from proposed
# note that this works very fast on a unioned geometry set, very slow if not

a <- exst %>% 
  st_set_precision(1e5)
b <- prop %>% 
  st_set_precision(1e5)

# existing conservation not in proposed
op1 <- st_difference(a, b)

# proposed not in existing conservation
op2 <- st_difference(b, a)

# existing conservation in proposed
op3 <- st_intersection(a, b)

prop <- op2 %>% 
  st_cast('POLYGON')
exst <- st_union(op1, op3) %>% 
  st_geometry() %>% 
  st_cast('POLYGON')

save(prop, file = here('data', 'prop.RData'), version = 2)
save(exst, file = here('data', 'exst.RData'), version = 2)

# import each lulc layer, crop by tbshed, save ----------------------------

data (tbshed)

# https://data-swfwmd.opendata.arcgis.com/search?groupIds=880fc95697ce45c3a8b078bb752faf40
urls <- list(
  `2020` = 'https://opendata.arcgis.com/datasets/6049aef268114f7dab8df36e103ff5c5_2.geojson',
  `2017` = 'https://opendata.arcgis.com/datasets/bedb342c692d4be6891b899e4cf7f4a6_1.geojson',
  `2014` = 'https://opendata.arcgis.com/datasets/e8cefca7d0d94fbaaa6b8dfa5403d984_0.geojson',
  `2011` = 'https://opendata.arcgis.com/datasets/f325a3417c92444d9cba838154d6fa0d_11.geojson',
  `2007` = 'https://opendata.arcgis.com/datasets/bec356d2a2204e15ac2385e80a1dd198_7.geojson',
  `2004` = 'https://opendata.arcgis.com/datasets/c6c5b47e178b44ff90ea777c57748a1f_4.geojson',
  `1999` = 'https://opendata.arcgis.com/datasets/f3352c512a904eb694d3a3e04fc275a1_3.geojson',
  `1995` = 'https://opendata.arcgis.com/datasets/ca1b5d1bff5c4459a28e93dc92d39413_2.geojson',
  `1990` = 'https://opendata.arcgis.com/datasets/5be9f0bf951f45379e43466198988673_1.geojson'
  ) %>%
  enframe 

# setup parallel backend
ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)
strt <- Sys.time()

foreach(i = 1:nrow(urls), .packages = c('tidyverse', 'sf', 'here', 'tbeptools')) %dopar% {
  
  sink('log.txt')
  cat(i, 'of', nrow(urls), '\n')
  print(Sys.time()-strt)
  sink()
  
  # import file
  dat_raw <- urls[i, ] %>% 
    pull(value) %>% 
    .[[1]] %>% 
    st_read
  
  # crop by watershed and select fluccs
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    select(FLUCCSCODE) %>%
    filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>%
    st_buffer(dist = 0) %>% 
    st_intersection(tbshed)
  
  nm <- paste0('lulc', urls$name[i])
  assign(nm, dat_crp)
  
  save(list = nm, file = paste0('data/', nm, '.RData'), compress = 'xz')
  
}

# import each subtidal layer, crop by tbshed, save ------------------------

data(swfwmdtbseg)

toint <- swfwmdtbseg %>% 
  st_transform(crs = prj) %>% 
  st_union()

# all zipped files on amazon s3
# downloaded from here https://data-swfwmd.opendata.arcgis.com/
fls <- c('88', '90', '92', '94', '96', '99', '01', '04', '06', '08', '10', '12', '14', '16', '18', '20', '22') %>% 
  paste0('https://swfwmd-seagrass.s3.amazonaws.com/sg', ., '.zip')

# setup parallel backend
strt <- Sys.time()

for(i in 1:length(fls)){
  
  cat(i, 'of', length(fls), '\n')
  print(Sys.time()-strt)
  
  # download from s3, unzip
  tmpdir <- here('data/tmp')
  tmpzip <- here('data/tmp/tmp.zip')
  dir.create(tmpdir)
  download.file(fls[i], destfile = tmpzip)
  unzip(tmpzip, exdir = tmpdir)
  
  # import shapefile
  toimp <- list.files(tmpdir, pattern = '\\.shp$', full.names = T)
  dat_raw <- st_read(toimp, quiet = T)
  
  # delete files
  unlink(tmpdir, recursive = T)
  
  if(any(c('FLUCCS_CODE', 'FLUCCS_COD') %in% names(dat_raw)))
    names(dat_raw) <- gsub('^FLUCCS\\_COD$|^FLUCCS\\_CODE$', 'FLUCCSCODE', names(dat_raw))

  # crop by watershed and select fluccs
  # 9113 is patchy, 9116 is continuous
  dat_crp <- dat_raw %>%
    st_transform(crs = prj) %>%
    dplyr::select(FLUCCSCODE) %>% 
    filter(FLUCCSCODE %in% fluccs$FLUCCSCODE) %>%
    st_buffer(dist = 0) %>% 
    st_intersection(toint)
  
  # name assignment and save
  flnm <- gsub('^sg|\\.zip$', '', basename(fls[i])) %>% 
    as.numeric
  if(flnm > 80){
    flnm <- paste0('19', flnm)
  } else {
    flnm <- paste0('20', sprintf("%02d", flnm))
  }
  flnm <- paste0('sgdat', flnm)
  assign(flnm, dat_crp)
  save(list = flnm, file = here('data', paste0('/', flnm, '.RData')), compress = 'xz')
  
}

# fwc oyster beds in florida ------------------------------------------------------------------

load(file = here('data/tbshed.RData'))

# load current swfwmd subtidal layer
load(here('data', paste0(subtfl, '.RData')))

# most up to date layer for FL, updated 1-2 times a year by FWC
# for merge with swfwmd subtidal oyster estimates
# should be similar to swfwmd with a few extra beds
fwcoysterraw <- st_read('https://atoll.floridamarine.org/arcgis/rest/services/FWC_GIS/OpenData_MarineEco/MapServer/5/query?outFields=*&where=1%3D1&f=geojson')

# clip by tbshed and extract geometry
fwcoyster <- fwcoysterraw %>%
  st_transform(crs = st_crs(tbshed)) %>%
  st_make_valid() %>% 
  st_intersection(tbshed) %>% 
  st_transform(crs = prj) %>% 
  st_union() %>% 
  st_geometry()

# filter swfwmd subtidal by oyster beds fluccs code
swfwmdoys <- filter(sgdat2022, FLUCCSCODE %in% 6540) %>% 
  st_union() %>% 
  st_geometry()

swfwmdarea <- st_area(swfwmdoys) %>% sum() %>% units::set_units('acres') %>% as.numeric()
# fwcarea <- st_area(fwcoyster) %>% sum() %>% units::set_units('acres') %>% as.numeric()

# snap oyster beds to swfwmd using five foot tolerance
# most are similar but offset
fwcsnp <- st_snap(fwcoyster, swfwmdoys, units::set_units(5, 'feet')) %>% 
  st_make_valid()

# check total area, should match swfwmdarea + oysex at
unioys <- st_union(fwcsnp, swfwmdoys)
unioysarea <- unioys %>% st_area() %>% sum() %>% units::set_units('acres') %>% as.numeric()

# get difference
oysdif <- st_difference(fwcsnp, swfwmdoys) %>% 
  st_as_sf() %>% 
  st_collection_extract(c('POLYGON')) %>% 
  mutate(
    acres = st_area(.), 
    acres = as.numeric(units::set_units(acres, 'acres'))
  )

# check sum, should be less than an acre
sliv <- sum(oysdif$acres[as.numeric(oysdif$acres) <= 0.01])

# remove most slivers
oyse <- oysdif[as.numeric(oysdif$acres) >= 0.01, ]

# check similar areas
((sum(oyse$acres) + as.numeric(swfwmdarea)) + sliv)
unioysarea

# save oysters extra
save(oyse, file = 'data/oyse.RData', version = 2)

# habitats not in lulc ------------------------------------------------------

# st_layers(gdb)

# hardbottom
# hard <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/7') %>% 
hard <- st_read('https://opendata.arcgis.com/datasets/130c6747048647398665e195aa321438_7.geojson') %>% 
  st_transform(prj) %>% 
  filter(Hardbottom %in% 'Natural') %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, 'acres'),
    Acres = as.numeric(Acres)
  ) %>% 
  select(Acres)

# additional manatee river hardbottom (~6.86 acres)
# also at /data/raw/manavista.kml
hardmana <- st_read('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/21/query?outFields=*&where=1%3D1&f=geojson') %>% 
  st_collection_extract('POLYGON') %>% 
  st_zm(drop = T) %>% 
  st_transform(prj) %>%
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, 'acres'),
    Acres = as.numeric(Acres)
  ) %>% 
  select(Acres)

hard <- bind_rows(hard, hardmana)

# artificial reefs
# arti <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/0') %>% 
arti <- st_read('https://opendata.arcgis.com/datasets/dccf9329dffd4fb9a0436934c23487bb_0.geojson') %>% 
  st_transform(prj) %>% 
  mutate(
    Acres = st_area(.),
    Acres = set_units(Acres, 'acres'),
    Acres = as.numeric(Acres)
  ) %>% 
  select(Acres)

# tidal tribs
# tidt <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/18') %>%
tidt <- st_read('https://opendata.arcgis.com/datasets/d00e5a7955ba429498c8d096b4d7e908_18.geojson') %>%
  st_transform(prj) %>% 
  mutate(
    Miles = st_length(.),
    Miles = set_units(Miles, 'Miles'),
    Miles = as.numeric(Miles)
  ) %>% 
  select(Miles)

# living shorelines
# livs <- esri2sf('https://gis.waterinstitute.usf.edu/arcgis/rest/services/Maps/TBEP_OpenData/MapServer/2') %>% 
livs <- st_read('https://opendata.arcgis.com/datasets/7099e8a81c1847bc95ee4edfc44cfec9_2.geojson') %>% 
  st_transform(prj) %>% 
  mutate(
    Miles = st_length(.),
    Miles = set_units(Miles, 'Miles'),
    Miles = as.numeric(Miles)
  ) %>% 
  select(Miles)

save(hard, file = 'data/hard.RData', version = 2)
save(arti, file = 'data/arti.RData', version = 2)
save(tidt, file = 'data/tidt.RData', version = 2)
save(livs, file = 'data/livs.RData', version = 2)

# municipal boundaries ----------------------------------------------------

# these are one offs that don't need to be remotely accessed

# city of tampa
tampa <- st_read('~/Desktop/TBEP/HMPU/GIS/boundaries/Tampa_Boundary.shp') %>% 
  st_transform(crs = prj) %>% 
  st_geometry() %>% 
  st_union()

# city of st pete
stpet <- st_read('~/Desktop/TBEP/HMPU/GIS/boundaries/City_Limits_St._Petersburg.shp') %>% 
  st_transform(crs = prj) %>% 
  st_geometry() %>% 
  st_union()

# city of clearwater
clrwt <- st_read('~/Desktop/TBEP/HMPU/GIS/boundaries/clearwater_bnds.shp') %>% 
  st_transform(crs = prj) %>% 
  st_geometry() %>% 
  st_union()

# hillsborough county
hilco <- st_read('~/Desktop/TBEP/HMPU/GIS/boundaries/HillsFromFDEP.shp') %>% 
  st_transform(crs = prj) %>% 
  st_geometry() %>% 
  st_union()

# pinellas country
pinco <- st_read('~/Desktop/TBEP/HMPU/GIS/boundaries/PinellasGRfromFDEP.shp') %>% 
  st_transform(crs = prj) %>% 
  st_geometry() %>% 
  st_union()

# manatee country
manco <- st_read('~/Desktop/TBEP/HMPU/GIS/boundaries/ManateeFromFDEP.shp') %>% 
  st_transform(crs = prj) %>% 
  st_geometry() %>% 
  st_union()

# pasco country
pasco <- st_read('~/Desktop/TBEP/HMPU/GIS/boundaries/PascoCountyFDEP.shp') %>% 
  st_transform(crs = prj) %>% 
  st_geometry() %>% 
  st_union()

save(tampa, file = 'data/tampa.RData', version = 2)
save(stpet, file = 'data/stpet.RData', version = 2)
save(clrwt, file = 'data/clrwt.RData', version = 2)
save(hilco, file = 'data/hilco.RData', version = 2)
save(pinco, file = 'data/pinco.RData', version = 2)
save(manco, file = 'data/manco.RData', version = 2)
save(pasco, file = 'data/pasco.RData', version = 2)

