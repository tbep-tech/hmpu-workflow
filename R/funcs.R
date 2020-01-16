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
        chg = colDef(name = '1990-2017 change', minWidth = 130,
                     style = jsfun
        ), 
        chgper = colDef(name = '% change', minWidth = 100,
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
        chg = colDef(name = '1990-2017 change', minWidth = 130,
                     style = jsfun
        ), 
        chgper = colDef(name = '% change', minWidth = 100,
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