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

data(coastal)

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

save(acres, file = here('data', 'acres.RData'), version = 2)

# subtidal trends ---------------------------------------------------------

res <- list.files('data', '^sgdat') %>% 
  enframe() %>% 
  group_by(value) %>% 
  nest %>% 
  mutate(
    acres = purrr::map(value, function(x){
      
      cat(x, '\t')
      
      # import file
      load(file = here(paste0('data/', x)))
      dat <- get(gsub('\\.RData', '', x))
      
      dat_out <- subt_est(dat, fluccs)
      
      return(dat_out)
      
    })
  )

subtacres <- res %>% 
  select(name = value, acres) %>% 
  unnest(acres) %>% 
  mutate(name = gsub('^sgdat|\\.RData$', '', name)) %>% 
  ungroup()

# load extra fwc oyster
load(file = here('data/oyse.RData'))

# add extra oysters to subtsum
oysesum <- oyse %>% 
  pull(acres) %>%
  sum() %>% 
  as.numeric()
subtacres <-  subtacres %>% 
  mutate(
    Acres = case_when(
      HMPU_TARGETS == 'Oyster Bars' & name == max(as.numeric(name)) ~ Acres + oysesum, 
      TRUE ~ Acres
    )
  )

save(subtacres, file = here('data', 'subtacres.RData'), version = 2)

# LULC change analysis ----------------------------------------------------

data(coastal)

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
  lulc_est(coastal, fluccs, sumout = F) %>% 
  rename(Category = HMPU_TARGETS) %>% 
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
  
  if(yr == '2020')
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
    lulc_est(coastal, fluccs, sumout = F) %>% 
    rename(Category = HMPU_TARGETS) %>% 
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

save(chgdat, file = here('data', 'chgdat.RData'), version = 2)

# LULC change analysis simplified FLUCCS, 1990 to current -------------------------------------

data(coastal)

# file and year list
inds <- list.files('data', '^lulc') %>% 
  enframe %>% 
  mutate(
    yr = gsub('^lulc|\\.RData$', '', value)
  ) %>% 
  select(yr, data = value) %>% 
  filter(yr %in% range(yr))

# get final year 
maxyr <- inds$yr %>% max

# get final year data, add coastal uplands, simplify category
maxdat <- inds %>% 
  filter(yr == maxyr) %>% 
  pull(data)
load(file = here('data/', maxdat))
maxdat <- maxdat %>% 
  gsub('\\.RData$', '', .) %>%
  get %>% 
  lulc_est(coastal, fluccs, sumout = F) %>% 
  rename(Category = HMPU_TARGETS) %>% 
  st_union(by_feature = TRUE) %>%
  mutate(
    Category = paste0(Category, ', ', maxyr),
    Category = case_when(
      grepl('Mangrove|Salt', Category) ~ paste0('Coastal Wetlands, ', maxyr),
      grepl('Wetlands', Category) ~ paste0('Freshwater Wetlands, ', maxyr), 
      grepl('Uplands', Category) ~ paste0('Forested, ', maxyr), 
      T ~ Category
    )
  )

# change analysis comparing each year to max
strt <- Sys.time()
chgdatsimp <- NULL
for(i in 1:nrow(inds)){
  
  # year
  yr <- inds[i, ] %>% pull(yr)
  
  if(yr == '2020')
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
    lulc_est(coastal, fluccs, sumout = F) %>% 
    rename(Category = HMPU_TARGETS) %>% 
    st_union(by_feature = TRUE) %>%
    mutate(
      Category = paste0(Category, ', ', yr),
      Category = case_when(
        grepl('Mangrove|Salt', Category) ~ paste0('Coastal Wetlands, ', yr),
        grepl('Wetlands', Category) ~ paste0('Freshwater Wetlands, ', yr), 
        grepl('Uplands', Category) ~ paste0('Forested, ', yr), 
        T ~ Category
      )
    )
    
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
  
  chgdatsimp <- bind_rows(chgdatsimp, union)
  
}

save(chgdatsimp, file = here('data', 'chgdatsimp.RData'), version = 2)

# subtidal change analysis ------------------------------------------------

# takes about a day to run

# file and year list
inds <- list.files('data', '^sgdat') %>% 
  enframe %>% 
  mutate(
    yr = gsub('^sgdat|\\.RData$', '', value)
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
  mutate(
    FLUCCSCODE = as.integer(FLUCCSCODE)
  ) %>% 
  left_join(fluccs, by = 'FLUCCSCODE') %>%
  select(Category = HMPU_TARGETS) %>% 
  st_union(by_feature = TRUE) %>%
  mutate(
    Category = paste0(Category, ', ', maxyr)
  )

# change analysis comparing each year to max
strt <- Sys.time()
subtchgdat <- NULL
for(i in 1:nrow(inds)){
  
  # year
  yr <- inds[i, ] %>% pull(yr)
  
  if(yr == '2022')
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
    mutate(
      FLUCCSCODE = as.integer(FLUCCSCODE)
    ) %>% 
    left_join(fluccs, by = 'FLUCCSCODE') %>%
    select(Category = HMPU_TARGETS) %>% 
    st_union(by_feature = TRUE) %>%
    mutate(
      Category = paste0(Category, ', ', yr)
    )
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
  
  subtchgdat <- bind_rows(subtchgdat, union)
  
}

save(subtchgdat, file = here('data', 'subtchgdat.RData'), version = 2)
