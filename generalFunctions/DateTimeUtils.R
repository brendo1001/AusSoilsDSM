





days.in.month = function(month, year = NULL){
  
  month = as.integer(month)
  
  if (is.null(year))
    year = as.numeric(format(Sys.Date(), '%Y'))
  
  dt = as.Date(paste(year, month, '01', sep = '-'))
  dates = seq(dt, by = 'month', length = 2)
  as.numeric(difftime(dates[2], dates[1], units = 'days'))
}

days.in.year = function(year){
  
  ed = as.Date(paste0(year, '-12-31'))
  sd = as.Date(paste0(year, '-01-01'))
  as.numeric(difftime(ed, sd, units = 'days')) + 1
}

daysSinceDate <- function(sdate, edate){
  as.numeric(as.Date(edate) - as.Date(sdate))
}
