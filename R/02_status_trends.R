library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)

source(here('R', 'funcs.R'))

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

# LULC trends -------------------------------------------------------------

# do not do this with subtidal because years don't match with lulc

data(strats)

# get coastal stratum
coastal <- strats %>% 
  dplyr::filter(Stratum %in% 'Coastal') %>% 
  st_buffer(dist = 0) %>% 
  st_geometry()

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
      
      dat_out <- lulc_est(dat, coastal, fluccs)
      
      return(dat_out)
      
    })
  )

acres <- res %>% 
  select(name = value, acres) %>% 
  unnest(acres) %>% 
  mutate(name = gsub('^lulc|\\.RData$', '', name))

# manually add salt barren ests from ESA, these are not in first three years
sltbrn <- tibble(
  name = c('1990', '1995', '1999'),
  HMPU_TARGETS = 'Salt Barrens', 
  Acres = c(468, 479, 492)
)

acres <- acres %>% 
  bind_rows(sltbrn) %>% 
  arrange(name, HMPU_TARGETS, Acres)

save(acres, file = here('data', 'acres.RData'), compress = 'xz')

# LULC change analysis ----------------------------------------------------

data(strats)

# codes to remove, all subtidal 
cds <- c(5400, 5700, 5720, 6510, 6540, 7210, 9113, 9114, 9121, 9510, 9511, 9512, 9513, 9514, 9515)

# get coastal stratum
coastal <- strats %>% 
  dplyr::filter(Stratum %in% 'Coastal') %>% 
  st_buffer(dist = 0) %>% 
  st_geometry() %>% 
  st_union()

# file and year list
inds <- list.files('data', '^lulc') %>% 
  enframe %>% 
  mutate(
    yr = gsub('^lulc|\\.RData$', '', value)
  ) %>% 
  select(yr, data = value)

# get final year 
maxyr <- inds$yr %>% max

# get final year data, add coastal uplands
maxdat <- inds %>% 
  filter(yr == maxyr) %>% 
  pull(data)
load(file = here('data/', maxdat))
maxdat <- maxdat %>% 
  gsub('\\.RData$', '', .) %>%
  get %>% 
  filter(!FLUCCSCODE %in% cds) %>% # subtidal codes to remove
  add_coast_up(coastal, fluccs) %>% 
  st_union(by_feature = TRUE) %>%
  mutate(
    Category = paste0(Category, ', ', maxyr)
  )

# change analysis comparing each year to max
strt <- Sys.time()
chgdat <- NULL
for(i in 1:nrow(inds)){
  
  # year
  yr <- inds[i, ] %>% pull(yr)
  
  if(yr == '2017')
    next()
  
  cat(yr, 'importing\n')
  print(Sys.time() - strt)
  
  # current year, add coastal stratum
  a <- inds %>% 
    filter(yr == !!yr) %>% 
    pull(data)
  load(file = here('data/', a))
  a <- a %>% 
    gsub('\\.RData$', '', .) %>%
    get %>% 
    filter(!FLUCCSCODE %in% cds) %>% # subtidal codes to remove
    add_coast_up(coastal, fluccs) %>% 
    st_union(by_feature = TRUE) %>%
    mutate(Category = paste0(Category, ', ', yr))
  b <- maxdat
  
  cat('\tintersecting...\n')
  
  # so intersect doesnt complain about attributes
  st_agr(a) = "constant"
  st_agr(b) = "constant"
  
  # some clean up stuff for slivers
  a <- a %>% 
    st_set_precision(1e5) %>% 
    st_make_valid() %>% 
    st_buffer(dist = 0)
  b <- b %>% 
    st_set_precision(1e5) %>% 
    st_make_valid() %>% 
    st_buffer(dist = 0)
  aunion <- a %>% 
    st_union %>% 
    st_set_precision(1e5) %>% 
    st_make_valid() %>% 
    st_buffer(dist = 0)
  bunion <- b %>% 
    st_union %>% 
    st_set_precision(1e5) %>% 
    st_make_valid() %>% 
    st_buffer(dist = 0)
  
  # get full union
  op1 <- st_difference(a, bunion)
  op2 <- st_difference(b, aunion) %>%
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

save(chgdat, file = here('data', 'chgdat.RData'), compress = 'xz')
