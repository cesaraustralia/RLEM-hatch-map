calc_hatch <- function(TMIN, TMAX, RAIN, hatchrasterfile, 
                       longitude=NULL, ndays=NULL ){
  # Description: 
  #a function that returns the predicted RLEM hatch date based on McDonald 2015
  
  # Inputs:
  # TMIN, TMAX, RAIN are rasters of locations holding daily minimum temperature, maximum temperature, and rainfall respectively ordered by julian day. 
  # hatchrasterfile holds the result of the previous simulation with the date stored on the file name as hatch_day_at_%Y%m%d.tif
  
  # Outputs:  
  # hatch is a list of locations holding the predicted hatch julian day
  
  library(raster)
  
  # build longitude grid
  if(length(longitude)==1) longitude = rep(longitude, length(TMIN[[1]]))
  if(is.null(longitude)) {
    longitude = TMAX[[1]]
    longitude[] = xFromCell(TMAX[[1]], 1:length(TMAX[[1]]))
  }
  
  if(is.null(ndays)) ndays = min(c(
    raster::nlayers(TMIN),
    raster::nlayers(RAIN),
    raster::nlayers(TMAX)))
  
  
  # Threshold in Celsius
  MDTthresh = rep(20.5, length(TMIN[[1]])) # WA 
  MDTthresh[longitude[] > 130] = 16 # not WA
  
  # initialise state variables
  init_grid = rep(0, length(TMIN[[1]]))
  rain5 = init_grid
  
  # check for relevant previous simulations or restart sim for new year
  previous_sim_date = hatchrasterfile |> 
    stringr::str_extract("\\d+(?=.tif)") |>
    as.Date('%Y%m%d')
  getyear = function(date) as.integer(format(date, "%Y"))
  
  if( is.null(hatchrasterfile) ||  
      (getyear(Sys.Date()) > getyear(previous_sim_date))) {
    # restart year 
    prestartday = 1
    startday = 15
    hatch = init_grid
    
  } else {
    # pick up where last simulation left off
    startday = previous_sim_date |> 
      format("%j") |> 
      as.integer() |> 
      sum(1)
    prestartday = startday - 14
    hatch = init_grid
    hatch[] = raster(hatchrasterfile)[]
    hatch[is.na(hatch)] = 0
    
  }

  
  for(day in startday:ndays){
    cat(paste('\ncalc hatch for doy: ',day))
    prev10 = (day-9):day
    
    MDT10 = init_grid
    
    # create penalty of number of day degrees over 19C threshold
    penalty = init_grid
    for (pd in prev10) {

      MDTi = (TMAX[[pd]] - (TMAX[[pd]] - TMIN[[pd]])/4)
      
      MDT10 = MDT10 + MDTi

      penalty[] = penalty[] + pmax(MDTi[] - 19, 0)

    }
    MDT10 = MDT10/length(prev10)
    penalty = penalty/length(prev10) # in contrast to garry who only averaged across the days above 19C
    MDT10[is.na(MDT10)] = 0
    penalty[is.na(penalty)] = 0
    
    # rainfall over past 5 days
    prev5 = (day-10):(day-14)
    rain5 = overlay(RAIN[[prev5]], fun=sum)
    rain5[is.na(rain5)] = 0
    # rain5 = Reduce(`+`, RAIN[prev5])
    
    index<-rain5[]>5 & MDT10[]<MDTthresh & hatch[]==0
    if(any(index)){
      hatch[index]<- day + 8*penalty[index]/10  # see p. 264 mcdonald 2016
    }
  }
  return(hatch)
}
