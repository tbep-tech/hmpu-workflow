library(tidyverse)
library(here)
library(dplyr)
library(haven)

# make this into a funciton, have switch for targets or goals, other aesthetics
# include in tbeptools, also acres, subtacres, targets as supp data

data(trgs)
data(subtacres)
data(acres)

# format datasets
sub <- subtacres %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(
    year = name, 
    metric = HMPU_TARGETS
  )
supra <- acres %>% 
  dplyr::ungroup() %>% 
  dplyr::rename(
    year = name, 
    metric = HMPU_TARGETS
  )
totinter <- supra %>% 
  filter(metric %in% c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes')) %>% 
  group_by(year) %>% 
  summarise(Acres = sum(Acres)) %>% 
  mutate(metric = 'Total Intertidal')
supra <- bind_rows(supra, totinter)
targets <- trgs %>% 
  dplyr::select(
    metric = HMPU_TARGETS,
    category = Category, 
    Target = Target2030, 
    Goal = Target2050
  ) %>% 
  dplyr::mutate(
    metric = as.character(metric), 
    category = gsub('tidal$', '', category)
  ) %>% 
  dplyr::filter(
    !metric %in% c('Living Shoreline', 'Hard Bottom', 'Artificial Reefs')
  ) 

# combine and sort
allacres <- rbind(sub, supra) %>%
  dplyr::arrange(metric, year) %>%
  dplyr::mutate(
    lacres = dplyr::lag(Acres),
    yr = as.numeric(year), 
    lyr = dplyr::lag(yr)
  ) %>%
  dplyr::group_by(metric) %>% 
  dplyr::mutate(
    lacres = ifelse(dplyr::row_number() == 1, NA, lacres),
    lyr = ifelse(dplyr::row_number() == 1, NA, lyr)
  ) %>%
  dplyr::ungroup()

# join with targets
alldat <- allacres %>%
  dplyr::full_join(targets, by = "metric") %>%
  dplyr::mutate(
    acresdiff = Acres - lacres,
    yeardiff = yr - lyr,
    changerate = acresdiff / yeardiff,
    targetrate = (Target - Acres) / (2030 - yr), 
    goalrate = (Goal - Acres) / (2050 - yr),
    prop = Acres / Target, 
    Proportion = round(prop, 2)
  )

# apply conditions
HMPU_scored <- alldat %>%
  dplyr::mutate(
    targeteval = dplyr::case_when(
      Acres > Target & changerate > targetrate ~ 1,
      Acres > Target & !is.na(changerate) & changerate < targetrate ~ 0,
      Acres < Target & changerate > targetrate ~ 0,
      Acres < Target & !is.na(changerate) & changerate < targetrate ~ -1
      ),
    goaleval = dplyr::case_when(
      Acres > Goal & changerate > goalrate ~ 1,
      Acres > Goal & !is.na(changerate) & changerate < goalrate ~ 0,
      Acres < Goal & changerate > goalrate ~ 0,
      Acres < Goal & !is.na(changerate) & changerate < goalrate ~ -1
      ),
    alteval = dplyr::case_when(
      Acres > Target & changerate > targetrate ~ 1,
      Acres > Target & !is.na(changerate) & changerate < targetrate ~ 0,
      Acres < Target & changerate > targetrate ~ 0.5,
      Acres < Target & !is.na(changerate) & changerate < targetrate ~ -1
      )
  ) %>% 
  dplyr::filter(!is.na(changerate))%>%
  dplyr::arrange(category, year, metric)

toplo <- HMPU_scored %>% 
  select(year, metric, Acres, category, Target, Goal, Proportion, alteval) %>% 
  filter(!metric %in% c('Restorable', 'Developed', 'Open Water')) %>% 
  mutate(
    year = as.numeric(year),
    alteval = as.character(alteval),
    metric = factor(metric, 
                    levels = c("Tidal Flats", "Seagrasses", "Oyster Bars",
                               "Total Intertidal", "Mangrove Forests", "Salt Barrens", 
                               "Salt Marshes", "Coastal Uplands", "Non-Forested Freshwater Wetlands", 
                               "Forested Freshwater Wetlands", "Native Uplands")
                    )
    )

cols <- c(`-1` = '#CC3231', `0` = '#E9C318', `0.5` = '#AEFA2F', `1` = '#2DC938')

leglabs <- c('Target not met,\ntrending below', 'Target met,\ntrending below', 'Target not met,\ntrending above', 'Target met,\ntrending above')
ttl <- 'Habitat Master Plan 2030 target report card'

ggplot(toplo, aes(y = year, x = metric, fill = alteval)) + 
  geom_tile(color = 'black') +
  geom_text(aes(label = Proportion), size = 2.5) +
  scale_y_reverse(breaks = seq(min(toplo$year), max(toplo$year)), expand = c(0, 0)) + 
  scale_x_discrete(expand = c(0, 0), position = 'top') + 
  theme_bw() + 
  scale_fill_manual(
    values = cols, 
    labels = leglabs
  ) +
  geom_vline(xintercept = c(3.5, 7.5), linewidth = 0.5) + 
  theme(
    panel.grid = element_blank(), 
    axis.text.x = element_text(angle = 25, hjust = 0, size = 8),
    plot.margin = margin(0, 0, 14, 2, "pt")
  ) + 
  labs(
    x = NULL, 
    y = NULL, 
    fill = NULL, 
    title = ttl
  ) +
  annotate('text', x = 2, y = 2021, label = 'Subtidal') +
  annotate('text', x = 5.5, y = 2021, label = 'Intertidal') +
  annotate('text', x = 9.5, y = 2021, label = 'Supratidal') +
  coord_cartesian(ylim = c(max(toplo$year) + 1, min(toplo$year)) - 0.5, clip = "off")
