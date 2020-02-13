# reactable table function
rct_fun <- function(sums, colnm, grpby = T){
  
  jsfun <- JS("function(rowInfo) {
    var value = rowInfo.row.chg
    if (value >= 0) {
      var color = '#008000E6'
    } else if (value < 0) {
      var color = '#e00000E6'
    } 
    return { color: color, fontWeight: 'bold' }
    }"
  )
  
  if(grpby)
    out <- reactable(
      sums, 
      groupBy = 'grpval',
      columns = list(
        grpval = colDef(name = '', minWidth = 145),
        val = colDef(name = colnm, footer = 'Total', minWidth = 120), 
        `1990` = colDef(footer = sum(sums$`1990`), aggregate = 'sum'),
        `1995` = colDef(footer = sum(sums$`1995`), aggregate = 'sum'),
        `1999` = colDef(footer = sum(sums$`1999`), aggregate = 'sum'),
        `2004` = colDef(footer = sum(sums$`2004`), aggregate = 'sum'),
        `2007` = colDef(footer = sum(sums$`2007`), aggregate = 'sum'),
        `2011` = colDef(footer = sum(sums$`2011`), aggregate = 'sum'),
        `2014` = colDef(footer = sum(sums$`2014`), aggregate = 'sum'),
        `2017` = colDef(footer = sum(sums$`2017`), aggregate = 'sum'),
        chg = colDef(name = '1990-2017 change', minWidth = 140,
                     style = jsfun
        ), 
        chgper = colDef(name = '% change', minWidth = 85,
                        style = jsfun,
                        format = colFormat(suffix = '%', digits = 0)
                        
        )
      ),
      defaultColDef = colDef(
        footerStyle = list(fontWeight = "bold"),
        format = colFormat(digits = 0, separators = TRUE), 
        minWidth = 75, resizable = TRUE
      ),
      defaultPageSize = nrow(sums),
      showPageSizeOptions = F,
      highlight = T,
      wrap = F
    )
  
  if(!grpby)
    out <- reactable(
      sums, 
      columns = list(
        val = colDef(name = colnm, footer = 'Total', minWidth = 120), 
        `1990` = colDef(footer = sum(sums$`1990`), aggregate = 'sum'),
        `1995` = colDef(footer = sum(sums$`1995`), aggregate = 'sum'),
        `1999` = colDef(footer = sum(sums$`1999`), aggregate = 'sum'),
        `2004` = colDef(footer = sum(sums$`2004`), aggregate = 'sum'),
        `2007` = colDef(footer = sum(sums$`2007`), aggregate = 'sum'),
        `2011` = colDef(footer = sum(sums$`2011`), aggregate = 'sum'),
        `2014` = colDef(footer = sum(sums$`2014`), aggregate = 'sum'),
        `2017` = colDef(footer = sum(sums$`2017`), aggregate = 'sum'),
        chg = colDef(name = '1990-2017 change', minWidth = 140,
                     style = jsfun
        ), 
        chgper = colDef(name = '% change', minWidth = 85,
                        style = jsfun,
                        format = colFormat(suffix = '%', digits = 0)
                        
        )
      ),
      defaultColDef = colDef(
        footerStyle = list(fontWeight = "bold"),
        format = colFormat(digits = 0, separators = TRUE), 
        minWidth = 75, resizable = TRUE
      ),
      defaultPageSize = nrow(sums),
      showPageSizeOptions = F,
      highlight = T,
      wrap = F
    )
  
  return(out)
  
}

# alluvial function
# https://www.data-to-viz.com/graph/sankey.html
alluvout <- function(chgdat, lkup, var = 'HMPU_DESCRIPTOR', height = 1200){
  
  clp <- lkup %>% 
    select(!!var, FLUCCSCODE) %>% 
    group_by(!!var) %>%
    deframe() %>%
    map(as.character)
  
  sumdat <- chgdat %>% 
    select(FLUCCS17, FLUCCS90, Acres) %>% 
    group_by(FLUCCS17, FLUCCS90) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    mutate(
      FLUCCS17 = factor(FLUCCS17, levels = lkup$FLUCCSCODE),
      FLUCCS17 = fct_recode(FLUCCS17, !!!clp),
      FLUCCS90 = factor(FLUCCS90, levels = lkup$FLUCCSCODE),
      FLUCCS90 = fct_recode(FLUCCS90, !!!clp),
    ) %>% 
    na.omit() %>% 
    group_by(FLUCCS17, FLUCCS90) %>% 
    summarise(Acres = sum(Acres)) %>% 
    ungroup %>% 
    select(source = FLUCCS90, target = FLUCCS17, value = Acres) %>% 
    data.frame(stringsAsFactors = F)
  sumdat$target <- paste(sumdat$target, " ", sep="")
  
  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(name=c(as.character(sumdat$source), as.character(sumdat$target)) %>% unique())
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  sumdat$IDsource=match(sumdat$source, nodes$name)-1 
  sumdat$IDtarget=match(sumdat$target, nodes$name)-1
  
  out <- sankeyNetwork(Links = sumdat, Nodes = nodes,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", height = height, width = 800,
                sinksRight=FALSE, units = 'acres', nodeWidth=40, fontSize=13, nodePadding=5)
  
  return(out)
  
}