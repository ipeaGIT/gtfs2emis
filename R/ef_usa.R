#' @title Running exhaust emissions factors from United States (EMFAC2017 model)
#' 
#' @description Returns a vector or data.frame of emission factors for buses based on 
#' values from the [California EMission Factor model (EMFAC2017)](https://arb.ca.gov/emfac/).
#' Estimates expressed in units 'g/km'.
#'
#' @param pollutant Character; Pollutants: Carbon monoxide (CO); Nitrogen oxides (NOx); 
#'   Hydrocarbons as TOG (total organic gases), ROG (reactive organic gases), 
#'   THC (total hydrocarbon), or CH4 (methane); 
#'   Particulate matter as particulate matters 10 microns or less in diameter (PM10), 
#'   and particulate matters 2.5 microns or less in diameter (PM2.5);
#'   Sulfur oxides (SOx); Carbon Dioxide (CO2); 
#'   Nitrous Oxide (N2O), and Methane (CH4).
#' @param calendar_year Numeric; Calendar Year between 2010 - 2020. Year in which the emissions inventory is estimated,
#'  in order to consider the effect of degradation.
#' @param fuel Character; Type of fuel: 'Diesel','Gasoline','Natural Gas'. Default is 'Diesel'.
#' @param model_year Numeric; Model year of vehicle.
#' @param speed Units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as "5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45","45-50"
#'   "50-55","55-60","60-65","65-70","70-75","75-80","80-85","85-90".">90" mph (miles/h).
# @param veh_distribution numeric; proportion of circulating fleet according to model_year. It has to be 
#' the same length as model_year.
# @param aggregate does the emission factor should be aggregated?
#' @return Data.table; Emission factors in units 'g/km' by speed and model_year.
#' @source \url{https://arb.ca.gov/emfac/}
#' @export
ef_usa <- function(pollutant, calendar_year, model_year, speed, fuel = 'Diesel'){
  
  # pollutant = c("CO","PM10")
  # calendar_year = "2019"
  # model_year = total_fleet$year
  # speed = spo_gpslines$speed
  # fuel = "Diesel"
  
  
  
  # Filter calendar and fuel----
  
  temp_emfac <- usa[`Calendar Year` %in% as.character(calendar_year) &
                        Fuel %in% fuel,]
  
  # check units and lengths----
  
  if(class(speed) != "units"){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'")
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("speed need to has 'units' in 'km/h'.")
  }
  speed <- as.numeric(speed)
  
  # Filter pollutant
  
  ef_pol <- lapply(pollutant, function(p){     # p = pollutant[1]
    
    temp_emfac <- temp_emfac[Pollutant %in% p,]
    
    # check condition
    
    if(dim(temp_emfac)[1] == 0){
      stop("Emission Factor do not exist. \n Please check `data(usa)` for valid emission factors.")
    }
    
    
    # Filter Model Year----
    
    temp_ef <- sapply(model_year,function(i){       # i = model_year[1]
      temp_emfac <- temp_emfac[`Model Year` %in% as.character(i),]
      
      # check condition (2)
      if(dim(temp_emfac)[1] == 0){
        stop("Emission Factor do not exist. \n Please check `data(usa)` for valid emission factors.")
      }
      
      # Filter Speed based on emission factor data.table speed interval
      
      temp_speed <- rep(NA,length(speed))
      temp_order <- unique(usa$upper_speed_interval)
      temp_order <- temp_order[order(temp_order,decreasing = TRUE)]
      for(t in temp_order){ # t = temp_order[1]
        temp_speed[which(speed < t)] <- t
      }
      #temp_speed
      temp_model <- temp_emfac[sapply(temp_speed,
                                      function(i){which(upper_speed_interval %in% i)}),EF]
      return(temp_model)
    }) %>% data.table::as.data.table() 
    
    # rename columns
    
    names(temp_ef) <- paste0(p,"_",model_year)
    return(temp_ef)
  }) 
  

  # return in a data.table like format
  
  ef_final <- do.call(cbind,ef_pol)# 
  
  to_units <- function(i){units::set_units(i,"g/km")}
  ef_final <- ef_final[,lapply(.SD, to_units)]
  
  return(ef_final)
}
