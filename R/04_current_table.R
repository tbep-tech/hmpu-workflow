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

# from 03_current_layers
data(nativelyr)
data(restorelyr)

# boundaries, from 01_current_layers
data(stpet)
data(tampa)
data(clrwt)
data(hilco)
data(pinco)
data(manco)
data(pasco)

# complete watershed ------------------------------------------------------

cap <- 'Summary of the Opportunity Assessment Analysis'
tab <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap)
# tab <- curexleg_fun(strata, cap)

save_as_html(tab, path = 'docs/current_table.html', title = NULL)

# tampa -------------------------------------------------------------------

cap <- 'Summary of the Opportunity Assessment Analysis: Tampa'

tab_tampa <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, tampa)

save_as_html(tab_tampa, path = 'docs/current_table_tampa.html', title = NULL)

# st pete -----------------------------------------------------------------

cap <- cap <- 'Summary of the Opportunity Assessment Analysis: St. Pete'

tab_stpet <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, stpet)

save_as_html(tab_stpet, path = 'docs/current_table_stpet.html', title = NULL)

# clearwater --------------------------------------------------------------

cap <- cap <- 'Summary of the Opportunity Assessment Analysis: Clearwater'

tab_clrwt <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, clrwt)

save_as_html(tab_clrwt, path = 'docs/current_table_clrwt.html', title = NULL)

# hillsborough co ---------------------------------------------------------

cap <- cap <- 'Summary of the Opportunity Assessment Analysis: Hillsborough Co.'

tab_hilco <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, hilco)

save_as_html(tab_hilco, path = 'docs/current_table_hilco.html', title = NULL)

# pinellas co -------------------------------------------------------------

cap <- cap <- 'Summary of the Opportunity Assessment Analysis: Pinellas Co.'

tab_pinco <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, pinco)

save_as_html(tab_pinco, path = 'docs/current_table_pinco.html', title = NULL)

# manatee co --------------------------------------------------------------

cap <- cap <- 'Summary of the Opportunity Assessment Analysis: Manatee Co.'

tab_manco <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, manco)

save_as_html(tab_manco, path = 'docs/current_table_manco.html', title = NULL)

# pasco co ----------------------------------------------------------------

cap <- cap <- 'Summary of the Opportunity Assessment Analysis: Pasco Co.'

tab_pasco <- curex_fun(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, pasco)

save_as_html(tab_pasco, path = 'docs/current_table_pasco.html', title = NULL)
