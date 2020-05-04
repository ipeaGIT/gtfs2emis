#' @title Running exhaust emissions factors from EMFAC2017 model
#' 
#' @description Emissions that come out of the vehicle tailpipe while the vehicle is traveling on the road. The function returns emissions factor in units
#' 'g/km'
#'
#' @param pol Character; Pollutants: Carbon monoxide (CO); Nitrogen oxides (NOx); 
#'   Hydrocarbons as TOG (total organic gases), ROG (reactive organic gases), 
#'   THC (total hydrocarbon), or CH4 (methane); 
#'   Particulate matter as particulate matters 10 microns or less in diameter (PM10), 
#'   and particulate matters 2.5 microns or less in diameter (PM2.5);
#'   Sulfur oxides (SOx); Carbon Dioxide (CO2); 
#'   Nitrous Oxide (N2O) and Methane (CH4).
#' @param calendar_year Numeric; Calendar Year between 2010 - 2020
#' @param fuel Character; Type of fuel: 'Diesel','Gasoline','Natural Gas'
#' @param model_year Numeric; Model year.
#' @param speed Units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as "5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45","45-50"
#'   "50-55","55-60","60-65","65-70","70-75","75-80","80-85","85-90".">90" mph (miles/h)
#' @return Emission factors in units 'g/km'
#' @source \url{https://arb.ca.gov/emfac/}
#'
#' @export
ef_emfac <- function(pol,calendar_year,model_year,speed,fuel = 'Diesel'){
  # pol = 'COD';calendar_year = 2019;model_year = 2012
  # speed = units::set_units(10,'km/h'); fuel = 'Diesel'
  # Emission factor filter
  temp_emfac <- emfac[`Calendar Year` %in% as.character(calendar_year) &
                        `Model Year` %in% model_year &
                        Pollutant %in% pol &
                        Fuel %in% fuel,]
  if(dim(temp_emfac)[1] == 0){
    message("Emission Factor do not exist. \n Please check `data(emfac)` for valid emission factors.")
    return(NULL)
  }else{
    # Speed filter applied to EF
    ef_subset <- sapply(seq_along(speed),function(i){ # i = 1
      ef <- temp_emfac[lower_speed_interval < speed[i] & 
                         upper_speed_interval >= speed[i], EF]
      return(ef)
    }) %>% units::set_units('g/km')
    if(length(ef_subset) == 0){
      message("Emission Factor do not exist. \n Please check `data(emfac)` for valid emission factors.")
    }else{
      return(ef_subset)
    }
  }
  
}

