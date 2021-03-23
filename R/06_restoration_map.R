library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)

source(here('R', 'funcs.R'))

# restoration potential map -----------------------------------------------

data(restorelyr)
# data(reservelyr)
