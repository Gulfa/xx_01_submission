
groupByDate <- function(data_table){
  
  return_data =  setkey(data_table, location, date)[, list("observed"=.N), 
                                                    by=.(location, date)][CJ(unique(data_table$location),
                                                                             seq.Date(as.Date("2000-01-01"), as.Date("2010-12-31"), by = 1))
                                                                          ,allow.cartesian= TRUE][is.na(observed), observed:=0]
  return(return_data)
  
  
}

addIsoWeekYear <- function(data){
  data[, year:=isoWeekYear(date)$ISOYear]
  data[, week:=isoWeekYear(date)$ISOWeek]
  return(data)
}

prepareEnrichLastWeeksbyMunicipality <- function(data_by_location_date, municipalities, start_week){

  d = data_by_location_week[year==2010 & week >= start_week, list(observed=sum(observed)), by=.(location)]
  d$county = municipalities$countyName
  d$municipName = municipalities$municipName
  d$population = 10000
  d$mun_number = as.character(as.numeric(substr(d$location,8, 1000)))
  return(d)
}