# estimate lulc area in acres for available HMPU targets 
#
# lulcin in lulc rdata file for a given year
# coastal is coastal stratum from strats
# flucss is lookup table
lulc_est <- function(lulcin, coastal, fluccs){

  # add fluccs
  lulc <- lulcin %>%
    left_join(fluccs, by = 'FLUCCSCODE')

  # get coastal uplands
  coastal_uplands <- lulc %>% 
    dplyr::select(HMPU_TARGETS) %>% 
    dplyr::filter(HMPU_TARGETS %in% 'Native Uplands') %>% 
    st_geometry() %>% 
    st_intersection(coastal, .) %>% 
    st_area %>% 
    sum %>% 
    set_units(acres) %>% 
    tibble(
      HMPU_TARGETS = 'Coastal uplands', 
      Acres = .
    ) 

  # lulc area, all categories
  lulc <- lulc %>%
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres)
    ) 
  
  # lulc summarize, table categories
  out <- lulc %>%
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres)) %>% 
    bind_rows(coastal_uplands) %>% 
    mutate(
      Acres = case_when(
        HMPU_TARGETS == 'Native Uplands' ~ Acres - coastal_uplands$Acres, 
        T ~ Acres
      ), 
      Acres = as.numeric(Acres)
    ) %>% 
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
    select(Category = HMPU_TARGETS) 

  # get coastal stratum
  uplands <- lulc %>% 
    dplyr::filter(Category == 'Native Uplands') %>% 
    group_by(Category) %>% 
    summarise() 
  
  # get coastal uplands
  coastal_uplands <- uplands %>% 
    st_intersection(coastal) %>% 
    st_sf(geometry = .) %>%
    mutate(
      Category = 'Coastal Uplands'
    ) %>% 
    dplyr::select(Category) %>% 
    st_cast('POLYGON') %>% 
    st_zm()
  
  # true union
  op1 <- st_difference(lulc, st_union(coastal_uplands))
  op2 <- st_difference(coastal_uplands, st_union(lulc)) %>%
    rename(Category.1 = Category)
  op3 <- st_intersection(uplands, coastal_uplands)
  
  out <- bind_rows(op1, op2, op3) %>% 
    mutate(
      Category = case_when(
        Category.1 == 'Coastal Uplands' ~ 'Coastal Uplands', 
        T ~ Category
      )
    ) %>% 
    dplyr::select(Category)

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