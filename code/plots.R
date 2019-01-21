prepareDataForPlotting <- function(data_daily, model=NULL){
  data_daily = prepareDataForModel(data_daily)
  if(is.null(model)){
    model = createModel(data_daily)
  }
  tp = splitTrainingProduction(data_daily)
  training = tp$training
  production = tp$production
  data_daily$expected = c(predict(model, training), predict(model, production))
  data = data_daily[,list(expected=sum(expected),observed=sum(observed) ), by=.(year, week)]
  
  # Creating the bands for the plot
  data$std_2_upper =data$expected + 2*sqrt(data$expected)
  data$std_2_lower =data$expected - 2*sqrt(data$expected)
  data$std_3_upper =data$expected + 4*sqrt(data$expected)
  data$std_3_lower =data$expected + 2*sqrt(data$expected)
  data$std_4_upper = max(data$expected + 6*sqrt(data$expected))
  data$std_4_lower =data$expected + 4*sqrt(data$expected)
  
  # Creating dates from weeks for plotting
  data$year_week = paste(paste(data$year, str_pad(data$week, 2, pad="0"), sep="-W"), "-1", sep="")
  data$week_start = as.Date(ISOweek2date(data$year_week))
  
  return(data)
  
}

createOverviewPlot <- function (data_daily, municipality_name, current_year=2010, model=NULL) {
  # Preparing data for plotting
  data = prepareDataForPlotting(data_daily, model=model)
  
  # Plot from last year
  last_year = data[year == eval(current_year)]
  year_plot = createYearlyPlot(last_year, municipality_name)
  
  # Five year plot
  last_five_years = data[year >= current_year - 4]
  last_five_plot = last_five_plot = ggplot(last_five_years, aes(x=week_start, y=observed, group=1)) + geom_point() + geom_line() +
    geom_ribbon(aes(x=week_start, ymin=std_2_lower, ymax=std_2_upper), fill="blue", alpha=0.3)+
    geom_ribbon(aes(x=week_start, ymin=std_3_lower, ymax=std_3_upper), fill="yellow", alpha=0.3)+
    geom_ribbon(aes(x=week_start, ymin=std_4_lower, ymax=std_4_upper), fill="red", alpha=0.3) +
    #scale_x_discrete(breaks=c("2006 Week 1", "2007 Week 1", "2008 Week 1", "2009 Week 1", "2010 Week 1"))+ 
    labs(title=paste("Cases of Disease X in", municipality_name ,"last 5 years"), x="", y="")
  
  return(gridExtra::grid.arrange(year_plot, last_five_plot))
  
}

createYearlyPlot <- function(last_year, municipality_name) {
  
  # Plot from last year
  year_plot = ggplot(last_year, aes(x=week_start, y=observed, group=1)) + geom_point(show.legend = TRUE) + geom_line() +
    geom_ribbon(aes(x=week_start, ymin=std_2_lower, ymax=std_2_upper,  fill="Expected"), alpha=0.3)+
    geom_ribbon(aes(x=week_start, ymin=std_3_lower, ymax=std_3_upper,  fill="Higher than Expected (2-4SD)"), alpha=0.3)+
    geom_ribbon(aes(x=week_start, ymin=std_4_lower, ymax=std_4_upper, fill="Significantly higher than expected (>4SD)"),alpha=0.3) + 
    #scale_colour_manual(name='', values=c("Data" = "black", "Expected" = "transparent", "Higher than Expected" = "transparent", 
    #                                       "Significantly higher than expected" = "transparent")) + 
    scale_fill_manual(name = '',  values=c("Expected" = "blue", "Higher than Expected (2-4SD)" = "yellow", 
                                           "Significantly higher than expected (>4SD)" = "red")) +
    theme(legend.position = "top")+ labs(title=paste("Cases of Disease X in", municipality_name ,"last year"), x="", y="")
  
  return(year_plot)
  
  
  
}

createOverviewPlotZ <- function (data_daily, municipality_name, current_year=2010, model=NULL) {
  # Preparing data for plotting
  data = prepareDataForPlotting(data_daily, model=model)
  last_year = data[data$year == current_year]
  year_plot = createYearlyPlot(last_year, municipality_name)
  data$z = (data$observed - data$expected) / sqrt(data$expected)  
  last_five_years = data[year >= current_year - 4]
  
  last_five_plot = ggplot(last_five_years, aes(x=week_start, y=z, group=1)) + geom_point() + geom_line() +
    geom_ribbon(aes(x=week_start, ymin=-2, ymax=2), fill="blue", alpha=0.3)+
    geom_ribbon(aes(x=week_start, ymin=-4, ymax=-2), fill="yellow", alpha=0.3)+
    geom_ribbon(aes(x=week_start, ymin=2, ymax=4), fill="yellow", alpha=0.3)+
    #geom_ribbon(aes(x=week_start, ymin=std_4_lower, ymax=std_4_upper), fill="red", alpha=0.3) +
    #scale_x_discrete(breaks=c("2006 Week 1", "2007 Week 1", "2008 Week 1", "2009 Week 1", "2010 Week 1"))+ 
    labs(title=paste("Outbreaks in", municipality_name ,"over the last 5 years"), y="Z-score for observed cases", x="")
  
  return(gridExtra::grid.arrange(year_plot, last_five_plot))
  
}

createCountryOverview <- function(data_by_location_date, filename){
  country_level_data = data_by_location_date[, list(observed=sum(observed)), by=.(date, week, year)]

  model = createModel(country_level_data)

  overview_plot = createOverviewPlotZ(country_level_data[1:(nrow(country_level_data) -5)], "Norway", model=model)
  ggsave(filename, plot=overview_plot)
}

createCountryTable <- function(data, filename){
  table = data %>% group_by(county) %>% summarise(Population = sum(population), Cases= sum(observed), "Incidence per 1000" = round(Cases / Population * 1000, 1)) %>% arrange(-Cases)
  write_xlsx(table, filename)
}


createMunicipalityMap <- function(data, filename, weeks){

  geo = readShapePoly("data_raw/municipalities.shp")
  geo = gBuffer(geo, byid=TRUE, width=0)
  geo_data = fortify(geo, region="Kommunenum")

  geo_data = geo_data %>% left_join(. , data, by=c("id"="mun_number"))
  map = ggplot() + geom_polygon(data=geo_data, aes(x=long, y=lat, fill=observed, group=group)) + coord_map() + theme_void() + 
    ggtitle(paste("Cases of disease X by municipality in weeks", weeks))
  ggsave(filename, plot=map)
}