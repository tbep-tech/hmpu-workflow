# estimate lulc area in acres for avialable HMPU targets 
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