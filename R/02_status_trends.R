library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)

prj <- 4326

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

# LULC trends -------------------------------------------------------------

res <- list.files('data', '^lulc') %>% 
  enframe() %>% 
  group_by(value) %>% 
  nest %>% 
  mutate(
    acres = purrr::map(value, function(x){
      
      cat(x, '\t')
      
      # import file
      load(file = here(paste0('data/', x)))
      dat <- get(gsub('\\.RData', '', x))
      
      # get area
      dat_are <- dat %>%
        mutate(
          aream2 = st_area(.),
          aream2 = as.numeric(aream2),
          areaac = aream2 / 4047
        ) %>%
        st_set_geometry(NULL) %>%
        group_by(FLUCCSCODE) %>%
        summarise(
          Acres = sum(areaac)
        )
      
      # summarise by categories, remove subtidal first for watershed area
      dat_out <- dat_are %>%
        left_join(fluccs, by = 'FLUCCSCODE') %>%
        filter(!HMPU_DESCRIPTOR %in% c('Algae', 'Continuous_Seagrass', 'Hard_Bottom', 'Estuary', 'Oyster_Bars', 'Patchy_Seagrass', 'Subtidal', 'Tidal_Flats')) %>%
        select(-FLUCCSCODE, -FIRST_FLUC) %>%
        gather('var', 'val', -Acres) %>%
        group_by(var, val) %>%
        summarise(
          areaac = sum(Acres),
          .groups = 'drop'
        )
      
      return(dat_out)
      
    })
  )

acresjso <- res %>% 
  select(name = value, acres) %>% 
  unnest(acres) %>% 
  mutate(name = gsub('^lulc|\\.RData$', '', name))

# manually add salt barren ests from ESA
sltbrn <- tibble(
  name = c('1990', '1995', '1999'), 
  var = 'HMPU_DESCRIPTOR', 
  val = 'Salt_Barrens', 
  areaac = c(468,479, 492)
)

acresjso <- acresjso %>% 
  bind_rows(sltbrn) %>% 
  arrange(name, var, val)

save(acresjso, file = here('data', 'acresjso.RData'), compress = 'xz')

# LULC change analysis ----------------------------------------------------

# chgdat <- read.dbf('T:/05_GIS/HMPU/comp1990v2017/TBEP_dbasinsg_LU9017.dbf')
# save(chgdat, file = 'data/chgdat.RData', compress = 'xz')

# file and year list
inds <- list.files('data', '^lulc') %>% 
  enframe %>% 
  mutate(
    yr = gsub('^lulc|\\.RData$', '', value)
  ) %>% 
  select(yr, data = value)

# get final year 
maxyr <- inds$yr %>% max

# format final year data
maxdat <- inds %>% 
  filter(yr == maxyr) %>% 
  pull(data)
load(file = here('data/', maxdat))
maxdat <- maxdat %>% 
  gsub('\\.RData$', '', .) %>%
  get %>% 
  left_join(fluccs, by = 'FLUCCSCODE') %>%
  st_transform(crs = 26917) %>%
  select(Category = HMPU_TARGETS) %>%
  st_union(by_feature = TRUE) %>%
  mutate(Category = paste0(Category, ', ', maxyr))

# change analysis comparing each year to max
chgdat <- NULL
for(i in 1:nrow(inds)){
  
  # year
  yr <- inds[i, ] %>% pull(yr)
  
  if(yr == '2017')
    next()
  
  cat(yr, '\t')
  
  a <- inds %>% 
    filter(yr == !!yr) %>% 
    pull(data)
  load(file = here('data/', a))
  a <- a %>% 
    gsub('\\.RData$', '', .) %>%
    get %>% 
    left_join(fluccs, by = 'FLUCCSCODE') %>%
    st_transform(crs = 26917) %>%
    select(Category = HMPU_TARGETS) %>%
    st_union(by_feature = TRUE) %>%
    mutate(Category = paste0(Category, ', ', yr))
  b <- maxdat
  
  st_agr(a) = "constant"
  st_agr(b) = "constant"
  
  # get full union
  op1 <- st_difference(a, st_union(b))
  op2 <- st_difference(b, st_union(a)) %>%
    rename(Category.1 = Category)
  op3 <- st_intersection(a, b)
  
  union <- bind_rows(op1, op2, op3) %>%
    mutate(
      yr = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category))),
      yr.1 = unique(na.omit(sub('^.*\\,\\s([0-9]+)$', '\\1', Category.1))),
      Category = ifelse(is.na(Category), paste0('other, ', yr), as.character(Category)),
      Category.1 = ifelse(is.na(Category.1), paste0('other, ', yr.1), as.character(Category.1)),
      Acres = st_area(.),
      Acres = set_units(Acres, 'acres'),
      Acres = as.numeric(Acres)
    ) %>%
    select(-yr, -yr.1) %>%
    st_set_geometry(NULL) %>%
    select(Category.1, Category, Acres) %>%
    group_by(Category.1, Category) %>%
    summarise(Acres = sum(Acres)) %>%
    ungroup %>%
    select(source = Category, target = Category.1, value = Acres) %>%
    data.frame(stringsAsFactors = F)
  
  chgdat <- bind_rows(chgdat, union)
  
}

save(chgout, file = here('data', 'chgout.RData'), compress = 'xz')