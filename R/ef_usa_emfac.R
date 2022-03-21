#' @title 
#' Running exhaust emissions factors from United States (EMFAC2017 model)
#' 
#' @description 
#' Returns a vector or data.frame of emission factors for buses based on 
#' values from the [California EMission Factor model (EMFAC2017)](https://arb.ca.gov/emfac/).
#' Estimates expressed in units 'g/km'.
#'
#' @param pollutant character; Pollutants: CH4(Methane), CO(Carbon Monoxide), 
#' CO2(Carbon Dioxide), N2O(Nitrous Oxide), NOx(Oxides of Nitrogen),
#'  PM10(Primary Exhaust PM10 - Total), PM25(Primary Exhaust PM2.5 - Total), SOX(Oxides of Sulfur),
#'  TOG(Total Organic Gases), ROG (Reactive Organic Gases)
#' @param calendar_year numeric; Calendar Year between 2015 - 2022. Year in which the emissions
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
#'  ef_usa_emfac(pollutant = c("CO","PM10"),
#'         calendar_year = 2019,
#'         model_year = 2015,
#'         speed = units::set_units(1:100,"km/h"),
#'         fuel = "D",
#'         as_list = TRUE)
#'         
ef_usa_emfac <- function(pollutant, calendar_year, fuel = 'D', model_year, speed, as_list = TRUE){
  
  # pollutant = c("CO","PM10","CH4","NOx")
  # calendar_year = "2019"
  # model_year = c("2014","2013","2010");model_year = c("2010")
  # speed = units::set_units(33,"km/h")
  # fuel = c("Diesel","CNG"); fuel = "D"
  # 

  # use specific name-----
  tmp_fuel <- fuel
  tmp_calendar_year <- calendar_year
  tmp_pollutant <- pollutant
  tmp_model_year <- model_year
  
  # pre-filter in usa data----
  temp_emfac <- usa_emfac_db[calendar_year %in% as.character(unique(tmp_calendar_year)) &
                      fuel %in% unique(tmp_fuel) &
                      pollutant %in% unique(tmp_pollutant) & 
                      model_year %in% unique(tmp_model_year), ]
  
  # check units and lengths----
  if(class(speed) != "units"){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'")
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("speed need to has 'units' in 'km/h'.")
  }
  tmp_speed <- as.numeric(speed)
  
  # fuel
  if(length(tmp_model_year) != length(tmp_fuel) && length(tmp_fuel) == 1){
    tmp_fuel <- rep(tmp_fuel,length(tmp_model_year))
  }
  # model_year
  if(length(tmp_model_year) != length(tmp_fuel) && length(tmp_model_year) == 1){
    tmp_model_year <- rep(tmp_model_year,length(tmp_fuel))
  }
  
  # Filter by pollutant and model_year-----
  
  ef_pol <- lapply(pollutant, function(i){   # i = pollutant[1]
    
    temp_ef0 <- sapply(seq_along(tmp_model_year), function(j){   # j = 1
      # Filter Model Year & Pollutant & Fuel
      temp_emfac2 <- data.table::copy(temp_emfac)[pollutant %in% i & 
                                                    model_year %in% tmp_model_year[j] & 
                                                    fuel %in% tmp_fuel[j], ]
      # check condition
      if(dim(temp_emfac2)[1] == 0){
        stop("Emission Factor do not exist. \nPlease check `data(usa_emfac_db)` for valid emission factors.")
      }
      return(temp_emfac2[, ef])
    }) 
    temp_ef0 <- data.table::as.data.table(temp_ef0)
    names(temp_ef0) <- paste(i, as.character(tmp_model_year),tmp_fuel,sep = "_")
    return(temp_ef0)
  }) 
  
  # aggregate EF-----
  ef_final <- do.call(cbind, ef_pol)
  
  # Filter Speed based on emission factor data.table speed interval-----------
  tmp_speed2 <- rep(NA, length(tmp_speed))
  
  # data.table with upper_limit info
  temp_order_dt <- data.table::data.table(
    "limit" = temp_emfac$upper_speed_interval %>% unique() %>% as.numeric() %>% sort(decreasing = TRUE)
    ,"id" = data.table::uniqueN(temp_emfac$upper_speed_interval):1)
  
  for(t in 1:nrow(temp_order_dt)){ # t = 10
    tmp_speed2[tmp_speed <= temp_order_dt$limit[t]] <- temp_order_dt$id[t]
  }
  
  # case of speed lower than upper_limit_speed
  tmp_speed2[tmp_speed2==0] <- 1
  
  ef_final <- ef_final[tmp_speed2,]
  
  # add units
  ef_final <- ef_final[, lapply(.SD, units::set_units, "g/km")]
  
  # export-----
  
  # as.list
  if(as_list == TRUE){
    ef_final <- list("pollutant" = rep(pollutant,each = length(tmp_model_year)),
                     "model_year" = rep(model_year,length(pollutant)),
                     "fuel" = rep(tmp_fuel,length(pollutant)),
                     "EF" = ef_final)
  }
  return(ef_final)
}

