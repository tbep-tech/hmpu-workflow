# extract legend from existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# estimate lulc area in acres for available HMPU targets 
#
# lulcin in lulc rdata file for a given year
# coastal is coastal stratum from strats
# flucss is lookup table
# sumout logical if summary of acreage returned, otherwise
lulc_est <- function(lulcin, coastal, fluccs, sumout = T){

  # FLUCCS codes to remove, these are all subtidal and irrelevant for lulc
  # in order: bays and estuaries, major bodies of water, gulf of mexico, tidal flats, oyster bars, sand other than beaches submerged
  # patchy seagrass, continuous seagrass, attached algae, hardbottom x 6
  cds <- c(5400, 5700, 5720, 6510, 6540, 7210, 9113, 9116, 9121, 9510, 9511, 9512, 9513, 9514, 9515)

  # lulc area, all categories
  # remove open water and subtidal habitats (open water area changes between layers and subtidal not consistently collected)
  out <- lulcin %>%
    filter(!FLUCCSCODE %in% cds) %>% 
    add_coast_up(coastal, fluccs) 
  
  if(!sumout)
    return(out)
  
  out <- out %>% 
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres), 
      Acres = as.numeric(Acres)
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    arrange(HMPU_TARGETS)
  
  return(out)
  
}

# estimate subtidal area in acres for available HMPU targets 
#
# subtin in seagrass rdata file for a given year
# flucss is lookup table
subt_est <- function(subtin, fluccs, sumout = TRUE){
  
  # subtidal area, all categories
  out <- subtin %>%
    mutate(
      FLUCCSCODE = as.integer(FLUCCSCODE)
    ) %>% 
    left_join(fluccs, by = 'FLUCCSCODE') %>% 
    select(HMPU_TARGETS)
  
  if(!sumout)
    return(out)
    
  out <- out %>% 
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres),
      Acres = as.numeric(Acres)
    ) %>%
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    arrange(HMPU_TARGETS)
  
  return(out)
  
}

# Add coastal uplands to a lulc layer 
#
# lulcin lulc rdata file for a given year
# coastal is coastal stratum from strats
# flucss is lookup table
add_coast_up <- function(lulcin, coastal, fluccs){

  lulc <- lulcin %>% 
    left_join(fluccs, by = 'FLUCCSCODE') %>%
    dplyr::select(HMPU_TARGETS) 

  # get uplands geometry
  uplands <- lulc %>% 
    dplyr::filter(HMPU_TARGETS == 'Native Uplands') %>% 
    st_geometry() %>% 
    st_union() %>% 
    st_cast('POLYGON')
  
  # get coastal uplands
  coastal_uplands <- uplands %>% 
    st_intersection(coastal) %>% 
    st_union() %>% 
    st_cast('POLYGON') %>% 
    st_sf(geometry = .) %>%
    mutate(
      HMPU_TARGETS = 'Coastal Uplands'
    ) %>% 
    dplyr::select(HMPU_TARGETS) %>% 
    st_zm()

  # exit if no coastal uplands
  if(nrow(coastal_uplands) == 0)
    return(lulc)

  # lulc not in coastal uplands
  lulcdiff <- st_difference(lulc, st_geometry(st_union(st_combine(coastal_uplands))))
  
  # join op1 with coastal uplands
  out <- bind_rows(lulcdiff, coastal_uplands)
  
  return(out)
  
}

# get conservation layer based on url input
# 
# url is zip path on fnai website, https://www.fnai.org/gisdata.cfm
# prj is epsg number
# tbshed is tbshed sf object 
get_cons <- function(url, prj, tbshed){
  
  tmp1 <- tempfile()
  tmp2 <- tempfile()
  
  download.file(url, destfile = tmp1, method = 'libcurl')
  unzip(tmp1, exdir = tmp2)
  
  shp <- list.files(tmp2, pattern = '\\.shp$', full.names = T)
  out <- st_read(shp) %>%
    st_transform(crs = prj) %>%
    st_buffer(dist = 0) %>%
    st_intersection(tbshed) 
  
  file.remove(list.files(tmp1, full.names = T))
  file.remove(list.files(tmp2, full.names = T))
  
  return(out)
  
}

# fix geometries by union, cast to polygon, buffer by zero
#
# dat is input sf object with no attributes
fixgeo <- function(dat){
    
  out <- dat %>% 
    st_union() %>% 
    st_buffer(dist = 0) %>% 
    st_geometry() %>%
    st_cast('POLYGON') %>% 
    st_buffer(dist = 0) 
  
  return(out)

}

# get current extent table
#
# lulc is current lulc sf object
# subt is current subtidal sf object
# hard is current hard bottom sf object
# tidt is current tidal creeks sf object
# coastal is coastal stratum sf object
# fluccs is fluccs data frame
# nativelyr is current existing/proposed native sf object
# restorelyr is current existing/proposed restoration layer
# cap is chr string for caption
# crplyr is optional cropping layer
curex_fun <- function(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, nativelyr, restorelyr, cap, crplyr = NULL){

  # crop all sf objects by optional crop layer
  if(!is.null(crplyr)){
    
    lulc <- st_intersection(lulc, crplyr)
    subt <- st_intersection(subt, crplyr)
    hard <- st_intersection(hard, crplyr)
    arti <- st_intersection(arti, crplyr)
    tidt <- st_intersection(tidt, crplyr)
    livs <- st_intersection(livs, crplyr)
    coastal <- st_intersection(coastal, crplyr)
    nativelyr <- st_intersection(nativelyr, crplyr)
    restorelyr <- st_intersection(restorelyr, crplyr)
    
  }
  
  # current lulc summary

  # # from HMPU deliverables 
  # lulcdat <- raster('~/Desktop/rasters/rasters/Full_LULC.tif')
  # lulcdat <- readAll(lulcdat)
  # 
  # dat_crp <- lulcdat %>% 
  #   st_as_stars %>% 
  #   st_as_sf(as_points = FALSE, merge = TRUE) %>% 
  #   rename(FLUCCSCODE = 'Full_LULC')
    
  # lulc area, all categories
  lulcsum <- lulc %>% 
    lulc_est(coastal, fluccs)

  # subtidal area, all categories
  subtsum <- subt %>% 
    subt_est(fluccs)
  
  # hard bottom
  hardsum <- hard %>% 
    mutate(
      HMPU_TARGETS = 'Hard Bottom'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres))
  
  # artificial reefs
  artisum <- arti %>% 
    mutate(
      HMPU_TARGETS = 'Artificial Reefs'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres))
  
  # tidal tributaries
  tidtsum <- tidt %>% 
    mutate(
      HMPU_TARGETS = 'Tidal Tributaries'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Miles = sum(Miles))
  
  # living shorelines
  livssum <- livs %>% 
    mutate(
      HMPU_TARGETS = 'Living Shorelines'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Miles = sum(Miles))

  # current summary
  cursum <- bind_rows(lulcsum, subtsum, hardsum, artisum, tidtsum, livssum) %>% 
    mutate(
      unis = case_when(
        is.na(Acres) ~ 'mi', 
        is.na(Miles) ~ 'ac'
      ), 
      `Current Extent` = case_when(
        is.na(Acres) ~ Miles, 
        is.na(Miles) ~ Acres
      )
    ) %>%
    left_join(strata, ., by = 'HMPU_TARGETS') %>% 
    filter(!HMPU_TARGETS %in% 'Total Intertidal') %>% 
    mutate(
      unis = case_when(
        is.na(unis) & HMPU_TARGETS %in% c('Living Shorelines', 'Tidal Tributaries') ~ 'mi', 
        is.na(unis) & !HMPU_TARGETS %in% c('Living Shorelines', 'Tidal Tributaries') ~ 'ac', 
        T ~ unis
      ), 
      `Current Extent` = case_when(
        is.na(`Current Extent`) ~ 0, 
        T ~ `Current Extent`
      )
    ) %>% 
    select(Category, HMPU_TARGETS, unis, `Current Extent`) %>% 
    arrange(Category, HMPU_TARGETS)
    
  # native summary
  
  nativesum <- nativelyr %>% 
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres), 
      Acres = as.numeric(Acres), 
      typ = paste('native', typ)
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(typ, HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    arrange(typ, HMPU_TARGETS) %>% 
    spread(typ, Acres)
  
  # restorable summary
  
  restoresum <- restorelyr %>% 
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres),
      Acres = as.numeric(Acres),
      typ = paste('restorable', typ)
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(typ, HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres, na.rm = T), .groups = 'drop') %>% 
    arrange(typ, HMPU_TARGETS)
  
  # create duplicate rows for non-specific targets
  duplab1 <- 'Mangrove Forests/Salt Barrens'
  dups1 <- restoresum %>% 
    filter(HMPU_TARGETS %in% !!duplab1) %>% 
    mutate(HMPU_TARGETS = 'Mangrove Forests')
  duplab2 <- 'Freshwater Wetlands'
  dups2 <- restoresum %>% 
    filter(HMPU_TARGETS %in% !!duplab2) %>% 
    mutate(HMPU_TARGETS = 'Non-Forested Freshwater Wetlands')
  
  restoresum <- restoresum %>% 
    bind_rows(dups1) %>%
    bind_rows(dups2) %>% 
    mutate(
      HMPU_TARGETS = case_when(
        HMPU_TARGETS %in% !!duplab1 ~ 'Salt Barrens',
        HMPU_TARGETS %in% !!duplab2 ~ 'Forested Freshwater Wetlands', 
        T ~ HMPU_TARGETS
      )
    ) %>% 
    spread(typ, Acres, fill = 0) %>% 
    mutate(
      `total restorable` = `restorable Existing` + `restorable Proposed`
    )
  
  # final table
  
  tab <- curexcmp_fun(cursum, nativesum, restoresum, cap)
  
  return(tab)
  
}

# get current extent table with legacy values from HMPU
#
# cap is chr string for caption
curexleg_fun <- function(cap){
  
  # cursum
  cursum <- structure(list(
    Category = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("Subtidal", "Intertidal", "Supratidal"), class = "factor"), 
    HMPU_TARGETS = c("Artificial Reefs",  "Hard Bottom", "Oyster Bars", "Seagrasses", "Tidal Flats", "Living Shorelines", 
                     "Mangrove Forests", "Salt Barrens", "Salt Marshes", "Tidal Tributaries", 
                     "Coastal Uplands", "Forested Freshwater Wetlands", "Native Uplands", 
                     "Non-Forested Freshwater Wetlands"), 
    unis = c("ac", "ac", "ac", "ac", "ac", "mi", "ac", "ac", "ac", "mi", "ac", "ac", "ac", "ac"), 
    `Current Extent` = c(166, 423, 171, 40653, 16220, 11.3, 15300, 496, 4557, 387, 3619, 152132, 140600, 67587)), 
    class = "data.frame", row.names = c(NA, -14L))
  
  # nativesum
  nativesum <- structure(list(
    HMPU_TARGETS = c("Coastal Uplands", "Forested Freshwater Wetlands", 
                     "Mangrove Forests", "Native Uplands", "Non-Forested Freshwater Wetlands", 
                     "Salt Barrens", "Salt Marshes"), 
    `native Existing` = c(1725, 58222, 10864, 64374, 11482, 430, 2104), 
    `native Proposed` = c(1706, 56505, 4078, 52834, 25971, 62, 2316)), 
    row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))
  
  # restoresum
  restoresum <- structure(list(
    HMPU_TARGETS = c("Coastal Uplands", "Forested Freshwater Wetlands", 
                     "Mangrove Forests", "Native Uplands", "Non-Forested Freshwater Wetlands", 
                     "Salt Barrens", "Salt Marshes"), 
    `restorable Existing` = c(311, 27447, 1309, 13265, 27447, 1309, 241), 
    `restorable Proposed` = c(961, 132389, 1448, 30663, 132389, 1448,  851), 
    `total restorable` = c(1272, 159836, 2757, 43928, 159836, 2757, 1092)), 
    row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))
  

  # final table
  
  tab <- curexcmp_fun(cursum, nativesum, restoresum, cap)

  return(tab)
  
}

# final table compilation function for curex_fun, curexleg_fun
curexcmp_fun <- function(cursum, nativesum, restoresum, cap){
  
  # combine all for table
  
  # all summary
  allsum <- cursum %>% 
    left_join(nativesum, by = 'HMPU_TARGETS') %>% 
    left_join(restoresum, by = 'HMPU_TARGETS') %>% 
    gather('var', 'val', -Category, -HMPU_TARGETS, -unis) %>% 
    mutate(
      val = case_when(
        !is.na(val) ~ paste(prettyNum(round(val, 0), big.mark = ','), unis),
        T ~ 'N/A'
      ), 
      val = case_when(
        (HMPU_TARGETS %in% 'Salt Marshes') & (var %in% c('total restorable', 'restorable Existing', 
                                                         'restorable Proposed')) ~ paste(val, '(JU)'), 
        T ~ val
      ),
      Category = factor(Category, levels = c('Subtidal', 'Intertidal', 'Supratidal')), 
      HMPU_TARGETS = factor(HMPU_TARGETS, levels = levels(strata$HMPU_TARGETS))
    ) %>% 
    spread(var, val) %>% 
    dplyr::select(-unis) %>% 
    mutate(
      `native Existing` = case_when(
        Category == 'Subtidal' ~ `Current Extent`, 
        HMPU_TARGETS == c('Living Shorelines') ~ 'LSSM',
        T ~ `native Existing`
      ), 
      `total restorable` = case_when(
        HMPU_TARGETS == 'Seagrasses' ~ '14,131 ac', 
        HMPU_TARGETS %in% c('Tidal Flats', 'Oyster Bars') ~ 'I/D',
        HMPU_TARGETS %in% c('Living Shorelines', 'Tidal Tributaries') ~ 'LSSM', 
        T ~ `total restorable`
      ),
      `restorable Existing` = case_when(
        HMPU_TARGETS == 'Seagrasses' ~ '14,131 ac', 
        HMPU_TARGETS %in% c('Tidal Flats', 'Oyster Bars') ~ 'I/D',
        T ~ `restorable Existing`
      )
    ) %>% 
    select(
      Category,
      HMPU_TARGETS, 
      `Current Extent`, 
      `native Existing`, 
      `native Proposed`, 
      `total restorable`, 
      `restorable Existing`, 
      `restorable Proposed`
    )
  
  # make table
  
  # caption
  cap <- paste0('<h2>', cap, '</h2>')
  
  tab <- as_grouped_data(allsum, groups = 'Category') %>% 
    flextable %>% 
    set_header_labels(
      Category = 'Stratum',
      HMPU_TARGETS = 'Habitat Type',
      `native Existing` = 'Existing Conservation Lands', 
      `native Proposed` = 'Proposed Conservation Lands*',
      `total restorable` = 'Total Restoration Opportunity**', 
      `restorable Existing` = 'Existing Conservation Lands Restoration Opportunity', 
      `restorable Proposed` = 'Proposed Conservation Lands Restoration Opportunity*'
    ) %>% 
    merge_at(i = 1, part = 'body') %>% 
    merge_at(i = 7, part = 'body') %>% 
    merge_at(i = 13, part = 'body') %>% 
    merge_at(i = 9:10, j = 6, part = 'body') %>%
    merge_at(i = 9:10, j = 7, part = 'body') %>%
    merge_at(i = 9:10, j = 8, part = 'body') %>%
    merge_at(i = 15:16, j = 6, part = 'body') %>%
    merge_at(i = 15:16, j = 7, part = 'body') %>%
    merge_at(i = 15:16, j = 8, part = 'body') %>%
    add_header_row(colwidths = c(2, 3, 3), values = c('', 'Native Habitats', 'Restorable Habitats')) %>%
    add_footer_lines(values = "") %>% 
    footnote(i = 1, j = 1, sep = "", value = as_paragraph("N/A - Not Applicable; I/D - Insufficient Data; LSSM - Living Shoreline Suitability Model; JU - Potential"), part = 'body', inline = T, ref_symbols = "") %>% 
    footnote(i = 1, j = 2, sep = " ", value = as_paragraph(as_i("Juncus")), part = "body", inline = T, ref_symbols = "") %>% 
    footnote(i = 1, j = 3, sep = " ", value = as_paragraph("Marsh Opportunity"), inline = T, ref_symbols = "") %>% 
    add_footer_lines(values = "*All lands identified for acquisition by partners, does not represent a 2030 target or 2050 goal") %>%
    add_footer_lines(values = "**Does not account for lands neither currently protected nor currently under consideration for acquisition") %>% 
    fontsize(size = 8, part = 'footer') %>% 
    align(align = "center", part = "header") %>% 
    align(i = c(2:6, 8:12, 14:17), j = 3:8, align = "center", part = "body") %>% 
    bg(i = c(1, 7, 13), bg = 'chartreuse3', part = "body") %>% 
    bg(i = 1, bg = 'grey', part = "header") %>% 
    bg(i = 2, j = 1:2, bg = 'grey', part = "header") %>% 
    border_outer(part = 'body') %>% 
    border_outer(part = 'header') %>% 
    border_inner_h(part = 'body') %>% 
    border_inner_v(part = 'body') %>%  
    border_inner_h(part = 'header') %>% 
    border_inner_v(part = 'header') %>% 
    set_caption(caption = cap, html_escape = F) %>% 
    font(part = 'all', fontname = 'Roboto')
  
  return(tab)
  
}

# get opportunity layers for opportunity map
#
# nativersrv is current native lands in reservation sf object
# restorersrv is current restoration in reservation sf object
# nativelyr is current existing/proposed native sf object
# restorelyr is current existing/proposed restoration layer
# coastal is coastal stratum sf object
# crplyr is optional cropping layer
oppdat_fun <- function(nativersrv, restorersrv, nativelyr, restorelyr, coastal, crplyr = NULL){

  if(!is.null(crplyr)){
    
    coastal <- st_intersection(coastal, crplyr)
    nativersrv <- st_intersection(nativersrv, crplyr)
    restorersrv <- st_intersection(restorersrv, crplyr)
    nativelyr <- st_intersection(nativelyr, crplyr)
    restorelyr <- st_intersection(restorelyr, crplyr)
    
  }
  
  # union coastal for differencing
  unicoastal <- st_union(st_combine(coastal)) %>% st_make_valid()
  
  nativersrv <- nativersrv %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(cat = 'Reservation Native')
  
  restorersrv <- restorersrv %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(cat = 'Reservation Restorable')
  
  nativelyrprop <- nativelyr %>% 
    filter(typ %in% 'Proposed') %>% 
    fixgeo %>% 
    st_difference(., unicoastal) %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(
      cat = 'Proposed Conservation Native'
    ) %>% 
    st_make_valid()
  
  nativelyrexst <- nativelyr %>% 
    filter(typ %in% 'Existing') %>%
    fixgeo %>% 
    st_sf(geometry = .) %>%  
    mutate(
      cat = 'Existing Conservation Native'
    )
  
  restorelyrprop <- restorelyr %>% 
    filter(typ %in% 'Proposed') %>% 
    fixgeo %>% 
    st_difference(., unicoastal) %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(
      cat = 'Proposed Conservation Restorable'
    ) %>% 
    st_make_valid()
  
  restorelyrexst <- restorelyr %>% 
    filter(typ %in% 'Existing') %>% 
    fixgeo %>% 
    st_sf(geometry = .) %>% 
    mutate(
      cat = 'Existing Conservation Restorable'
    )
  
  out <- bind_rows(nativersrv, restorersrv, nativelyrexst, nativelyrprop, restorelyrexst, restorelyrprop)
  
  return(out)
  
}

# create a ggplot of an opportunity map
#
# oppmap is shapefile input created in 05_opportunities_map.R
# bndry is input boundary layer created in 01_inputs.R
# ttle is chr string of plot title
# northloc location of north arrow
# scaleloc location of scale bar
# buffdist is buffer for boundary layer
#
# requires ggsn, ggmap, ggplot, sf
oppmap_fun <- function(oppmap, bndry, ttl, northloc = 'tr', scaleloc = 'tl', buffdist = 0.01){
  
  # colors 
  cols <- list(
    `Existing Conservation Native` = 'yellowgreen', 
    `Existing Conservation Restorable` = 'green4', 
    `Proposed Conservation Native` = 'dodgerblue1', 
    `Proposed Conservation Restorable` = 'dodgerblue4', 
    `Reservation Native` = 'violetred1', 
    `Reservation Restorable` = 'violetred3'
    ) %>% 
    unlist
  
  # transform to wgs to work with ggmap
  tomap <- oppmap %>%
    st_transform(crs = 4326)
  
  # trasnfrom to wgs to work with ggmap
  bndry <- bndry %>% 
    st_transform(crs = 4326)
  
  # layer extent as bbox plus buffer
  dat_ext <- bndry %>% 
    st_bbox %>% 
    st_as_sfc %>% 
    st_buffer(dist = buffdist) %>%
    st_bbox %>% 
    unname

  # reference data for ggsn, MUST have geometry named column
  ggsnref <- bndry %>% 
    st_bbox %>% 
    st_as_sfc %>%
    st_buffer(dist = buffdist / 2) %>% 
    st_as_sf %>%
    st_cast('POINT') %>% 
    rename(geometry = x)
  
  # stamen base map
  bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 12)
  
  # change opacity of basemap
  mapatt <- attributes(bsmap1)
  bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                           alpha.f = 0.2), 
                               nrow = nrow(bsmap1))
  attributes(bsmap1_transparent) <- mapatt

  # plot
  p <- ggmap(bsmap1_transparent) +
    geom_sf(data = tomap, aes(fill = cat), color = NA, inherit.aes = F, alpha = 0.8) +
    geom_sf(data = bndry, fill = NA, color = 'black', inherit.aes = F, size = 0.3) +
    scale_fill_manual(values = cols, drop = F) +
    theme(
      legend.title = element_blank(), 
      panel.grid = element_blank(), 
      axis.title = element_blank(), 
      legend.position  = 'right', 
      legend.justification = 'top',
      axis.text.y = element_text(size = 7), 
      axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
      panel.background = element_rect(fill = 'white'),
      axis.ticks = element_line(colour = 'grey'),
      panel.border = element_rect(colour = 'grey', fill = NA)
    ) + 
    labs(
      title = ttl, 
      caption = 'More info: https://tbep.org/habitat-master-plan-update/'
      ) +
    annotation_scale(location = scaleloc) +
    annotation_north_arrow(location = northloc, which_north = "true", height = grid::unit(0.75, "cm"), 
                           width = grid::unit(0.75, "cm"))
  
  return(p)
  
}

# create a ggplot of a restoration potential map
#
# restmap is shapefile input created in 06_opportunities_map.R
# bndry is input boundary layer created in 01_inputs.R
# ttle is chr string of plot title
# northloc location of north arrow
# scaleloc location of scale bar
# stsz size of text on scale bar
# buffdist is buffer for boundary layer
# scldst is division unit for scale bar
# stht is scalebar height
#
# requires ggsn, ggmap, ggplot, sf
restmap_fun <- function(restmap, bndry, ttl, northloc = 'topright', scaleloc = 'topleft', stsz = 3, buffdist = 0.01, scldst = 3, stht = 0.02){
  
  # colors
  cols <- list(
    `Coastal Uplands` = 'brown4', 
    `Freshwater Wetlands` = 'orange', 
    `Native Uplands` = 'darkgreen', 
    `Tidal Wetlands` = 'yellow'
  ) %>% 
    unlist
  
  # transform to wgs to work with ggmap
  tomap <- restmap %>%
    st_transform(crs = 4326)
  
  # trasnfrom to wgs to work with ggmap
  bndry <- bndry %>% 
    st_transform(crs = 4326)
  
  # layer extent as bbox plus buffer
  dat_ext <- bndry %>% 
    st_bbox %>% 
    st_as_sfc %>% 
    st_buffer(dist = buffdist) %>%
    st_bbox %>% 
    unname
  
  # reference data for ggsn, MUST have geometry named column
  ggsnref <- bndry %>% 
    st_bbox %>% 
    st_as_sfc %>%
    st_buffer(dist = buffdist / 2) %>% 
    st_as_sf %>%
    st_cast('POINT') %>% 
    rename(geometry = x)
  
  # stamen base map
  bsmap1 <- get_stamenmap(bbox = dat_ext, maptype = 'toner-background', zoom = 12)
  
  # change opacity of basemap
  mapatt <- attributes(bsmap1)
  bsmap1_transparent <- matrix(adjustcolor(bsmap1, 
                                           alpha.f = 0.2), 
                               nrow = nrow(bsmap1))
  attributes(bsmap1_transparent) <- mapatt
  
  # plot
  p <- ggmap(bsmap1_transparent) +
    geom_sf(data = tomap, aes(fill = HMPU_TA), color = NA, inherit.aes = F, alpha = 0.8) +
    geom_sf(data = bndry, fill = NA, color = 'black', inherit.aes = F, size = 0.3) +
    scale_fill_manual(values = cols, drop = F) +
    theme(
      legend.title = element_blank(), 
      panel.grid = element_blank(), 
      axis.title = element_blank(), 
      legend.position  = 'right', 
      legend.justification = 'top',
      axis.text.y = element_text(size = 7), 
      axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
      panel.background = element_rect(fill = 'white'),
      axis.ticks = element_line(colour = 'grey'),
      panel.border = element_rect(colour = 'grey', fill = NA)
    ) + 
    labs(
      title = ttl, 
      caption = 'More info: https://tbep.org/habitat-master-plan-update/'
    ) +
    annotation_scale(location = scaleloc) +
    annotation_north_arrow(location = northloc, which_north = "true", height = grid::unit(0.75, "cm"), 
                           width = grid::unit(0.75, "cm"))
  
  return(p)
  
}

# restoration data function for restoration map, assumes salt marshes are included in tidal wetlands
#
# restorelyr is current existing/proposed restoration layer
# crplyr is optional cropping layer
restdat_fun <- function(restorelyr, crplyr = NULL){
  
  if(!is.null(crplyr)){
    
    restorelyr <- crplyr %>% 
      fixgeo %>% 
      st_geometry %>% 
      st_intersection(restorelyr, .)
    
  }

  out <- restorelyr %>% 
    filter(typ %in% 'Existing') %>% 
    mutate(
      HMPU_TARGETS = case_when(
        HMPU_TARGETS %in% c('Mangrove Forests/Salt Barrens', 'Salt Marshes') ~ 'Tidal Wetlands', 
        T ~ HMPU_TARGETS
      )
    ) %>% 
    select(HMPU_TARGETS) %>% 
    group_by(HMPU_TARGETS) %>% 
    nest %>% 
    mutate(
      geometry = purrr::map(data, fixgeo)
    ) %>% 
    select(-data) %>% 
    unnest('geometry') %>% 
    ungroup %>% 
    st_as_sf()
  
  return(out)
  
}

# get target table
#
# lulc is current lulc sf object
# subt is current subtidal sf object
# hard is current hard bottom sf object
# tidt is current tidal creeks sf object
# coastal is coastal stratum sf object
# fluccs is fluccs data frame
# restorelyr is current existing/proposed restoration layer
# trgs is input targets table
# cap is chr string for caption
target_fun <- function(lulc, subt, hard, arti, tidt, livs, coastal, fluccs, strata, restorelyr, trgs, cap){
  
  # lulc area, all categories
  lulcsum <- lulc_est(lulc, coastal, fluccs)
  
  # add total intertidal, this is unique to this table
  intrsum <- lulcsum %>% 
    filter(HMPU_TARGETS %in% c('Mangrove Forests', 'Salt Barrens', 'Salt Marshes')) %>% 
    pull(Acres) %>% 
    sum %>% 
    tibble(
      HMPU_TARGETS = 'Total Intertidal', 
      Acres = .
    )
  
  # subtidal area, all categories
  subtsum <- subt_est(subt, fluccs)
  
  # hard bottom
  hardsum <- hard %>% 
    mutate(
      HMPU_TARGETS = 'Hard Bottom'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres))
  
  # artificial reefs
  artisum <- arti %>% 
    mutate(
      HMPU_TARGETS = 'Artificial Reefs'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres))
  
  # tidal tributaries
  tidtsum <- tidt %>% 
    mutate(
      HMPU_TARGETS = 'Tidal Tributaries'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Miles = sum(Miles))
  
  # living shorelines
  livssum <- livs %>% 
    mutate(
      HMPU_TARGETS = 'Living Shorelines'
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(HMPU_TARGETS) %>%
    summarise(Miles = sum(Miles))
  
  # current summary
  cursum <- bind_rows(lulcsum, intrsum, subtsum, hardsum, artisum, tidtsum, livssum) %>% 
    mutate(
      unis = case_when(
        is.na(Acres) ~ 'mi', 
        is.na(Miles) ~ 'ac'
      ), 
      `Current Extent` = case_when(
        is.na(Acres) ~ Miles, 
        is.na(Miles) ~ Acres
      )
    ) %>%
    inner_join(strata, by = 'HMPU_TARGETS') %>% 
    select(Category, HMPU_TARGETS, unis, `Current Extent`) %>% 
    arrange(Category, HMPU_TARGETS)
  
  # restorable summary
  
  restoresum <- restorelyr %>% 
    mutate(
      Acres = st_area(.),
      Acres = set_units(Acres, acres),
      Acres = as.numeric(Acres),
      typ = paste('restorable', typ)
    ) %>% 
    st_set_geometry(NULL) %>%
    group_by(typ, HMPU_TARGETS) %>%
    summarise(Acres = sum(Acres), .groups = 'drop') %>% 
    arrange(typ, HMPU_TARGETS)
  
  # create duplicate rows for non-specific targets
  duplab1 <- 'Mangrove Forests/Salt Barrens'
  dups1 <- restoresum %>% 
    filter(HMPU_TARGETS %in% !!duplab1) %>% 
    mutate(HMPU_TARGETS = 'Mangrove Forests')
  duplab2 <- 'Freshwater Wetlands'
  dups2 <- restoresum %>% 
    filter(HMPU_TARGETS %in% !!duplab2) %>% 
    mutate(HMPU_TARGETS = 'Non-Forested Freshwater Wetlands')
  
  restoresum <- restoresum %>% 
    bind_rows(dups1) %>%
    bind_rows(dups2) %>% 
    mutate(
      HMPU_TARGETS = case_when(
        HMPU_TARGETS %in% !!duplab1 ~ 'Salt Barrens',
        HMPU_TARGETS %in% !!duplab2 ~ 'Forested Freshwater Wetlands', 
        T ~ HMPU_TARGETS
      )
    ) %>% 
    spread(typ, Acres) %>% 
    mutate(
      `total restorable` = `restorable Existing` + `restorable Proposed`
    ) %>% 
    dplyr::select(HMPU_TARGETS, `total restorable`)
  
  # add total intertidal, this is unique to this table
  intrsum <- restoresum %>% 
    filter(HMPU_TARGETS %in% c('Mangrove Forests', 'Salt Marshes')) %>% # salt barrens is duplicated with mangrove, only pull on
    pull(`total restorable`) %>% 
    sum %>% 
    tibble(
      HMPU_TARGETS = 'Total Intertidal', 
      `total restorable` = .
    )
  
  # add intrsum to restoresum
  restoresum <- bind_rows(restoresum, intrsum)
  
  # final table
  out <- targetcmp_fun(cursum, restoresum, cap)
    
  return(out)
  
}

# get target table with legacy values from HMPU doc
#
# trgs is input targets table
# cap is chr string for caption
targetleg_fun <- function(trgs, cap){
  
  # cursum
  cursum <- structure(list(
    Category = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c("Subtidal", "Intertidal", "Supratidal"), class = "factor"), 
    HMPU_TARGETS = c("Artificial Reefs",  "Hard Bottom", "Oyster Bars", "Seagrasses", "Tidal Flats", "Living Shorelines", 
                     "Total Intertidal", "Mangrove Forests", "Salt Barrens", "Salt Marshes", "Tidal Tributaries", 
                     "Coastal Uplands", "Forested Freshwater Wetlands", "Native Uplands", 
                     "Non-Forested Freshwater Wetlands"), 
    unis = c("ac", "ac", "ac", "ac", "ac", "mi", "ac", "ac", "ac", "ac", "mi", "ac", "ac", "ac", "ac"), 
    `Current Extent` = c(166, 423, 171, 40653, 16220, 11.3, 20353, 15300, 496, 4557, 387, 3619, 152132, 140600, 67587)), 
    class = "data.frame", row.names = c(NA, -15L))
  
  # restoresum
  restoresum <- structure(list(
    HMPU_TARGETS = c("Coastal Uplands", "Forested Freshwater Wetlands", 
                     "Mangrove Forests", "Native Uplands", "Non-Forested Freshwater Wetlands", 
                     "Salt Barrens", "Salt Marshes", "Total Intertidal"), 
    `total restorable` = c(1272, 159836, 2757, 43928, 159836, 2757, 1092, 3849)), 
    row.names = c(NA, -8L), class = c("tbl_df", "tbl", "data.frame"))

  # final table
  out <- targetcmp_fun(cursum, restoresum, cap)
  
  return(out)
  
}

# final table compilation function for target_fun, targetleg_fun
targetcmp_fun <- function(cursum, restoresum, cap){
  
  # all summary
  allsum <- cursum %>% 
    left_join(restoresum, by = 'HMPU_TARGETS') %>% 
    left_join(trgs, ., by = c('Category', 'HMPU_TARGETS')) %>% 
    gather('var', 'val', -Category, -HMPU_TARGETS, -unis, -rationale) %>% 
    mutate(
      val = case_when(
        !is.na(val) ~ paste(prettyNum(round(val, 0), big.mark = ','), unis),
        T ~ 'N/A'
      ), 
      val = case_when(
        var %in% c('Target2030', 'Target2050') & HMPU_TARGETS %in% c('Hard Bottom', 'Artificial Reefs', 'Seagrasses', 'Mangrove Forests') ~ paste0('>', val), 
        T ~ val
      ),
      Category = factor(Category, levels = c('Subtidal', 'Intertidal', 'Supratidal')), 
      HMPU_TARGETS = factor(HMPU_TARGETS, levels = levels(strata$HMPU_TARGETS))
    ) %>% 
    spread(var, val) %>% 
    dplyr::select(-unis) %>% 
    mutate(
      `total restorable` = case_when(
        HMPU_TARGETS == 'Seagrasses' ~ '14,131 ac', 
        HMPU_TARGETS %in% c('Tidal Flats', 'Oyster Bars', 'Tidal Tributaries') ~ 'I/D',
        HMPU_TARGETS %in% c('Living Shorelines') ~ 'LSSM', 
        T ~ `total restorable`
      )
    ) %>% 
    select(
      Category,
      HMPU_TARGETS, 
      `Current Extent`, 
      `total restorable`, 
      Target2030,
      Target2050, 
      rationale
    )
  
  cap <- paste0('<h2>', cap, '</h2>')
  
  tab <- as_grouped_data(allsum, groups = 'Category') %>% 
    flextable %>% 
    set_header_labels(
      Category = 'Stratum',
      HMPU_TARGETS = 'Habitat Type',
      `total restorable` = 'Total Restoration Opportunity*', 
      `Target2030` = '2030 Target', 
      `Target2050` = '2050 Goal', 
      rationale = 'Target Narrative and Restoration and Protection Rationale'
    ) %>% 
    merge_at(i = 1, part = 'body') %>% 
    merge_at(i = 7, part = 'body') %>% 
    merge_at(i = 14, part = 'body') %>% 
    merge_at(i = 10:11, j = 4, part = 'body') %>%
    merge_at(i = 16:17, j = 4, part = 'body') %>%
    add_footer_lines(values = "") %>%
    footnote(i = 1, j = 1, sep = "", value = as_paragraph("N/A - Not Applicable; I/D - Insufficient Data; LSSM - Living Shoreline Suitability Model; JU - Potential"), part = 'body', inline = T, ref_symbols = "") %>%
    footnote(i = 1, j = 2, sep = " ", value = as_paragraph(as_i("Juncus")), part = "body", inline = T, ref_symbols = "") %>%
    footnote(i = 1, j = 3, sep = " ", value = as_paragraph("Marsh Opportunity"), inline = T, ref_symbols = "") %>%
    add_footer_lines(values = "*Does not account for lands neither currently protected nor currently under consideration for acquisition") %>%
    fontsize(size = 8, part = 'footer') %>%
    fontsize(i = c(2:6, 8:13, 15:18), j = 7, size = 8, part = 'body') %>%
    bold(i = 9) %>% 
    width(j = 7, width = 4.5) %>% 
    align(j = c(2:6), align = "center", part = "header") %>%
    align(i = c(2:6, 8:13, 15:18), j = 3:6, align = "center", part = "body") %>%
    bg(i = c(1, 7, 14), bg = 'chartreuse3', part = "body") %>% 
    bg(i = 1, bg = 'grey', part = "header") %>% 
    border_outer(part = 'body') %>% 
    border_outer(part = 'header') %>% 
    border_inner_h(part = 'body') %>% 
    border_inner_v(part = 'body') %>%  
    border_inner_h(part = 'header') %>% 
    border_inner_v(part = 'header') %>% 
    set_caption(caption = cap, html_escape = F) %>% 
    font(part = 'all', fontname = 'Roboto')
  
  return(tab)
  
}