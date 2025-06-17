library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)
library(flextable)

source(here('R', 'funcs.R'))

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

lulcfl <- 'lulc2023'
subtfl <- 'sgdat2024'

# from 01_inputs
load(here('data', paste0(lulcfl, '.RData')))
load(here('data', paste0(subtfl, '.RData')))
lulc <- get(lulcfl)
subt <- get(subtfl)
data(hard)
data(arti)
data(tidt)
data(livs)
data(oyse)
data(coastal)
data(strata)
data(trgs)

# from 03_current_layers
data(restorelyr)

# current lulc summary ----------------------------------------------------

cap <- 'Summary of the Recommended 2030 Targets and 2050 Goals'

# complete
tab <- target_fun(lulc, subt, hard, arti, tidt, livs, oyse, coastal, fluccs, strata, restorelyr, trgs, cap, stratsel = 'All')

save_as_html(tab, path = 'docs/target_table.html', title = NULL)

# simple
tabsimp <- target_fun(lulc, subt, hard, arti, tidt, livs, oyse, coastal, fluccs, strata, restorelyr, trgs, cap, stratsel = 'All', simple = T)

# fix dimensions for simple approximate to tab
bodrow <- (length(ncol(tabsimp)$heights) - 1)
tabsimp <- tabsimp %>% 
  width(j = 1:ncol_keys(.), width = flextable_dim(tab)$widths / ncol_keys(.)) %>% 
  height(i = 1:bodrow, height = flextable_dim(tab)$heights / bodrow) 
flextable_dim(tabsimp)$aspect_ratio

save_as_html(tabsimp, path = 'docs/target_table_simple.html', title = NULL)

# simple, no total intertidal
tabsimpnotot <- target_fun(lulc, subt, hard, arti, tidt, livs, oyse, coastal, fluccs, strata, restorelyr, trgs, cap, stratsel = 'All', simple = T, totintertid = F, j1width = 1.5)

# # fix dimensions for simple approximate to tab
# bodrow <- (length(ncol(tabsimpnotot)$heights) - 1)
# tabsimpnotot <- tabsimpnotot %>% 
#   width(j = 1:ncol_keys(.), width = flextable_dim(tab)$widths / ncol_keys(.)) %>% 
#   height(i = 1:bodrow, height = flextable_dim(tab)$heights / bodrow) 
# flextable_dim(tabsimpnotot)$aspect_ratio

save_as_html(tabsimpnotot, path = 'docs/target_table_simple_no_total_intertidal.html', title = NULL)
