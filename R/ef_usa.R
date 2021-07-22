#' @title 
#' Running exhaust emissions factors from United States (EMFAC2017 model)
#' 
#' @description 
#' Returns a vector or data.frame of emission factors for buses based on 
#' values from the [California EMission Factor model (EMFAC2017)](https://arb.ca.gov/emfac/).
#' Estimates expressed in units 'g/km'.
#'
#' @param pollutant character; Pollutants: Carbon monoxide (CO), Nitrogen oxides (NOx), 
#'   Hydrocarbons as TOG (total organic gases), ROG (reactive organic gases), 
#'   THC (total hydrocarbon), or CH4 (methane).
#'   Particulate matter as particulate matters 10 microns or less in diameter (PM10), 
#'   and particulate matters 2.5 microns or less in diameter (PM2.5),
#'   Sulfur oxides (SOx), Carbon Dioxide (CO2),
#'   Nitrous Oxide (N2O), and Methane (CH4).
#' @param calendar_year numeric; Calendar Year between 2010 - 2020. Year in which the emissions
#' inventory is estimated, in order to consider the effect of degradation.
#' @param fuel character; Type of fuel: 'D' (Diesel),'G' (Gasoline),'CNG' (Compressed Natural Gas). Default is 'D'.
#' @param model_year numeric; Model year of vehicle.
#' @param speed units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50",
#'   "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85-90", ">90" mph (miles/h).
# @param veh_distribution numeric; proportion of circulating fleet according to model_year.
# It has to be the same length as model_year.
# @param aggregate does the emission factor should be aggregated?
#' @param as_list logical; Returns emission factors as a list, instead of data.table format. Default is TRUE.
#' 
#' @return List. Emission factors in units 'g/km' by speed and model_year.
#' @source \url{https://arb.ca.gov/emfac/}
#' @export
#' 
#' @examples 
#'  ef_usa(pollutant = c("CO","PM10"),
#'         calendar_year = 2019,
#'         model_year = 2015,
#'         speed = units::set_units(1:100,"km/h"),
#'         fuel = "D",
#'         as_list = TRUE)
#'         
ef_usa <- function(pollutant, calendar_year, model_year, speed, fuel = 'Diesel', as_list = TRUE){
  
  # pollutant = c("CO","PM10","CH4")
  # calendar_year = "2019"
  # model_year = c("2014","2013")
  # speed = units::set_units(1:100,"km/h")
  # fuel = c("Diesel","CNG")
  
  # Filter calendar and fuel----
  
  temp_emfac <- usa[`Calendar Year` %in% as.character(unique(calendar_year)) &
                      Fuel %in% unique(fuel), ]
  
  # check units and lengths----
  
  if(class(speed) != "units"){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'")
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("speed need to has 'units' in 'km/h'.")
  }
  speed <- as.numeric(speed)
  
  # fuel
  if(length(model_year) != length(fuel) && length(fuel) == 1){
    fuel <- rep(fuel,length(model_year))
  }
  # model_year
  if(length(model_year) != length(fuel) && length(model_year) == 1){
    model_year <- rep(model_year,length(fuel))
  }
  
  # Filter pollutant
  ef_pol <- lapply(pollutant, function(i){   # i = pollutant[1]
    # Filter Model Year----
    temp_ef0 <- sapply(seq_along(model_year), function(j){   # j = 1
      # model_year
      temp_emfac2 <- data.table::copy(temp_emfac)[Pollutant %in% i & 
                                                    `Model Year` %in% as.character(model_year[j]) & 
                                                    Fuel %in% fuel[j], ]
      # check condition (2)
      if(dim(temp_emfac2)[1] == 0){
        stop("Emission Factor do not exist. \nPlease check `data(usa)` for valid emission factors.")
      }
      return(temp_emfac2[, EF])
    }) 
    temp_ef0 <- data.table::as.data.table(temp_ef0)
    names(temp_ef0) <- paste(i, as.character(model_year),fuel,sep = "_")
    return(temp_ef0)
  }) 
ef_pol
  ef_final <- do.call(cbind, ef_pol)

# Filter Speed based on emission factor data.table speed interval
temp_speed <- rep(NA, length(speed))
temp_order <- unique(usa$upper_speed_interval)
temp_order <- temp_order[order(temp_order, decreasing = TRUE)]
for(t in temp_order){ # t = temp_order[1]
  temp_speed[which(speed < t)] <- t
}
#temp_speed

ef_final <- ef_final[as.integer(as.factor(temp_speed)), ]
ef_final <- ef_final[, lapply(.SD, units::set_units, "g/km")]

# list
if(as_list == TRUE){
  # local test
  ef_final <- list("pollutant" = rep(pollutant,each = length(model_year)),
                   "model_year" = rep(model_year,length(pollutant)),
                   "fuel" = rep(fuel,length(pollutant)),
                   "EF" = ef_final)
}

return(ef_final)
}

