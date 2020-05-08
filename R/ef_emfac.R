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
#' @return List; Emission factors in units 'g/km' by speed and model_year
#' @source \url{https://arb.ca.gov/emfac/}
#' @examples
#' ef_emfac(pol = 'CO',calendar_year = 2019,
#' model_year = c(2016,2017,2018),
#' speed = units::set_units(seq(1,100,by = 10),'km/h'))
#' 
#' @export
ef_emfac <- function(pol,calendar_year,model_year,speed,fuel = 'Diesel'){
  # pol = 'CO';calendar_year = 2019;model_year = c('2006','2006')
  # speed = units::set_units(c(10,20,30),'km/h'); fuel = 'Diesel'
  # Emission factor filter
  ef0 <- lapply(model_year,function(i){ # i = model_year[1]
    temp_emfac <- emfac[`Calendar Year` %in% as.character(calendar_year) &
                          `Model Year` %in% as.character(i) &
                          Pollutant %in% pol &
                          Fuel %in% fuel,]
    # 1st condition
    if(dim(temp_emfac)[1] == 0){
      message("Emission Factor do not exist. \n Please check `data(emfac)` for valid emission factors.")
      return(NULL)
    }
    else{
      # Speed filter applied to EF
      ef1 <- sapply(seq_along(speed),function(i){ # i = 140
        ef2 <- temp_emfac[lower_speed_interval < speed[i] & 
                            upper_speed_interval >= speed[i], EF]
        #message(i)
        return(ef2)
      }) %>% units::set_units('g/km')
    }
    return(ef1)
  }) 
  names(ef0) <- c(model_year)
  
  return(ef0)
}

