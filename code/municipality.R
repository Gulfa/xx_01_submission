
createMunicipalityOverviewOutbreak <- function(data_by_location_data, municipalities){
  for (i in 1:nrow(municipalities)){
    current_municipality = municipalities[i, "municip"]
    current_municipality_name = municipalities[i, "municipName"]
  
    data = data_by_location_date[location == current_municipality]
    model = createModel(data)
    o = findOutbreaksMunicipality(data, model=model)
    overview_plot = createOverviewPlot(data, current_municipality_name, model=model)
    ggsave(paste("results/municipality_results/overview_M",substring(current_municipality, 2), ".png", sep=""), plot=overview_plot)
    write_xlsx(o, paste("results/municipality_results/outbreaks_M", substring(current_municipality, 2), ".xlsx", sep=""))
  }
}