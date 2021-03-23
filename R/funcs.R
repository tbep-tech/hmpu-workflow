# estimate lulc area in acres for available HMPU targets 
#
# lulcin in lulc rdata file for a given year
# coastal is coastal stratum from strats
# flucss is lookup table
# sumout logical if summary of acreage returned, otherwise
lulc_est <- function(lulcin, coastal, fluccs, sumout = T){

  # FLUCCS codes to remove, these are all subtidal and irrelevant for lulc
  # in order: bays and estuaries, major bodies of water, gulf of mexico, tidal flats, oyster bars, sand other than beaches submerged
  # patchy seagrass, continuous seagrass, attached algae, hardbottom x 6
  cds <- c(5400, 5700, 5720, 6510, 6540, 7210, 9113, 9116, 9121, 9510, 9511, 9512, 9513, 9514, 9515)

  # lulc area, all categories
  # remove open water and subtidal habitats (open water area changes between layers and subtidal not consistently collected)
  out <- lulcin %>%
    filter(!FLUCCSCODE %in% cds) %>% 
    add_coast_up(coastal, fluccs) 
  
  if(!sumout)
    return(out)
  
  out <- out %>% 
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres), 
      Acres = as.numeric(Acres)
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    arrange(HMPU_TARGETS)
  
  return(out)
  
}

# estimate subtidal area in acres for available HMPU targets 
#
# subtin in seagrass rdata file for a given year
# flucss is lookup table
subt_est <- function(subtin, fluccs){
  
  # subtidal area, all categories
  out <- subtin %>%
    mutate(
      FLUCCSCODE = as.integer(FLUCCSCODE)
    ) %>% 
    left_join(fluccs, by = 'FLUCCSCODE') %>% 
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres),
      Acres = as.numeric(Acres)
    ) %>%
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    arrange(HMPU_TARGETS)
  
  return(out)
  
}

# Add coastal uplands to a lulc layer 
#
# lulcin lulc rdata file for a given year
# coastal is coastal stratum from strats
# flucss is lookup table
add_coast_up <- function(lulcin, coastal, fluccs){

  lulc <- lulcin %>% 
    left_join(fluccs, by = 'FLUCCSCODE') %>%
    select(HMPU_TARGETS) 

  # get uplands geometry
  uplands <- lulc %>% 
    dplyr::filter(HMPU_TARGETS == 'Native Uplands') %>% 
    st_geometry() %>% 
    st_union() %>% 
    st_cast('POLYGON')
  
  # get coastal uplands
  coastal_uplands <- uplands %>% 
    st_intersection(coastal) %>% 
    st_union() %>% 
    st_cast('POLYGON') %>% 
    st_sf(geometry = .) %>%
    mutate(
      HMPU_TARGETS = 'Coastal Uplands'
    ) %>% 
    dplyr::select(HMPU_TARGETS) %>% 
    st_zm()

  # lulc not in coastal uplands
  lulcdiff <- st_difference(lulc, st_geometry(st_union(coastal_uplands)))
  
  # join op1 with coastal uplands
  out <- bind_rows(lulcdiff, coastal_uplands)
  
  return(out)
  
}

# get conservation layer based on url input
# 
# url is zip path on fnai website, https://www.fnai.org/gisdata.cfm
# prj is epsg number
# tbshed is tbshed sf object 
get_cons <- function(url, prj, tbshed){
  
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  
  download.file(url, destfile = tmp1, method = 'libcurl')
  unzip(tmp1, exdir = tmp2)
  
  shp <- list.files(tmp2, pattern = '\\.shp$', full.names = T)
  out <- st_read(shp) %>%
    st_transform(crs = prj) %>%
    st_buffer(dist = 0) %>%
    st_intersection(tbshed) 
  
  file.remove(list.files(tmp1, full.names = T))
  file.remove(list.files(tmp2, full.names = T))
  
  return(out)
  
}

# fix geometries by union, cast to polygon, buffer by zero
#
# dat is input sf object with no attributes
fixgeo <- function(dat){
    
  out <- dat %>% 
    st_union() %>% 
    st_buffer(dist = 0) %>% 
    st_geometry() %>%
    st_cast('POLYGON') %>% 
    st_buffer(dist = 0) 
  
  return(out)

}