
createRegressionModel <- function(data){
  
  model <- glm(observed ~ year_i + month , family=poisson(link="identity"), data=data)
  return(model)
}

prepareDataForModel <- function(data){
  data =  data[!(date >= "2005-04-01" & date <= "2005-04-15")] # we are missing this data as can be seen from the CreateFakeData.R file
  data$month = factor(month(data$date))
  data$year_i = data$year - 2000
  return (data)
  
}

createModel <- function(data){
  # prepare data
  data = prepareDataForModel(data)
  tp = splitTrainingProduction(data)
  training = tp$training
  production = tp$production
  # Train on all the training data
  m1 = createRegressionModel(training)
  training$expected = m1$fitted.values
  weekly = training[,list(expected_week=sum(expected),observed_week=sum(observed) ), by=.(year, week)]
  training %>% left_join(., weekly, by=c("year"="year", "week"="week"))
  # SD of poisson distribution is sqrt(lambda)
  #Create new training data by removing the training outbreaks
  new_training = training[observed <= expected + 2*sqrt(expected)]
  
  m2 = createRegressionModel(new_training)
  return(m2) 
  
}

splitTrainingProduction <- function(data, split_year=2010){
  training = data[year(date) < split_year]
  production = data[year(date) >= split_year]
  return(list(training=training, production=production))
}
createPredictionInterval <- function(expected, sd=2){
  stddev = sqrt(expected)
  return(e)
  
}

findOutbreaksMunicipality <- function(data, model = NULL) {
  data = prepareDataForModel(data)
  tp = splitTrainingProduction(data)
  production = tp$production
  if(is.null(model)){
    model = createModel(data)
  }
  production$expected = predict(model, production)
  weekly_data = production[,list(expected=sum(expected),observed=sum(observed) ), by=.(year, week)]
  outbreaks = weekly_data[observed > expected + 2*sqrt(expected)]
  return(outbreaks)

}






