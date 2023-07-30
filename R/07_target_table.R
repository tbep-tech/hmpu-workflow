library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)
library(flextable)

source(here('R', 'funcs.R'))

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

lulcfl <- 'lulc2020'
subtfl <- 'sgdat2022'

# from 01_inputs
load(here('data', paste0(lulcfl, '.RData')))
load(here('data', paste0(subtfl, '.RData')))
lulc <- get(lulcfl)
subt <- get(subtfl)
data(hard)
data(arti)
data(tidt)
data(livs)
data(coastal)
data(strata)
data(trgs)

# from 03_current_layers
data(restorelyr)

# current lulc summary ----------------------------------------------------

cap <- 'Summary of the Recommended 2030 Targets and 2050 Goals'

tab <- target_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, restorelyr, trgs, cap, stratsel = 'All')
# tab <- targetleg_fun(trgs, strata, cap, stratsel = 'All')

save_as_html(tab, path = 'docs/target_table.html', title = NULL)

