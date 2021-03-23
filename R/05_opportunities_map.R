library(sf)
library(tidyverse)
library(here)
library(doParallel)
library(foreach)
library(units)

source(here('R', 'funcs.R'))

# opportunities map -------------------------------------------------------

data(restorelyr)
data(nativelyr)

# data(reservelyr)