library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)
library(flextable)

source(here('R', 'funcs.R'))

fluccs <- read.csv(here('data', 'FLUCCShabsclass.csv'), stringsAsFactors = F)

lulcfl <- 'lulc2017'
subtfl <- 'sgdat2018'

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

# from 03_current_layers
data(nativelyr)
data(restorelyr)

# boundaries, form 01_current_layers
data(stpete)

# complete watershed ------------------------------------------------------

cap <- 'Summary of the Opportunity Assessment Analysis'
tab <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap)

save_as_html(tab, path = 'docs/current_table.html', title = 'Current Table')

# st pete -----------------------------------------------------------------

cap <- cap <- 'Summary of the Opportunity Assessment Analysis: St. Pete'

tab <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, stpete)

save_as_html(tab, path = 'docs/current_table_stpete.html', title = 'Current Table St. Pete')
