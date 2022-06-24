#' @title 
#' Running exhaust emissions factors from United States (MOVES3 model)
#' 
#' @description 
#' Returns a vector or data.frame of emission factors for urban buses based on 
#' values from the [MOVES3 Model](https://www.epa.gov/moves).
#' Estimates expressed in units 'g/km'. 
#' 
#' @param pollutant character; Pollutants: CH4 (Methane), CO (Carbon Monoxide), 
#' CO2 (Carbon Dioxide), EC (Energy Consumption), HONO (Nitrous Acid), N2O (Nitrous Oxide),
#'  NH3 (Ammonia ), NH4 (Ammonium), NO (Nitrogen Oxide), NO2 (Nitrogen Dioxide),
#'  NO3 (Nitrate), NOx (Oxides of Nitrogen), PM10 (Primary Exhaust PM10 - Total), 
#'  PM25 (Primary Exhaust PM2.5 - Total), SO2 (Sulfur Dioxide), 
#'  THC (Total Gaseous Hydrocarbons ), TOG (Total Organic Gases) and 
#'  VOC (Volatile Organic Compounds)
#' @param reference_year numeric; Year of reference, between 2015 - 2022.
#'  Year in which the emissions inventory is estimated. Default is 2020.
#' @param fuel character; Type of fuel: 'D' (Diesel),'G' (Gasoline),
#' 'CNG' (Compressed Natural Gas). Default is 'D'.
#' @param model_year numeric; Model year of vehicle.
#' @param speed units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as " - 2.5", "2.5 - 7.5", "7.5 - 12.5", "12.5 - 17.5", "17.5 - 22.5", "22.5 - 27.5",
#'  "27.5 - 32.5","32.5 - 37.5","37.5 - 42.5","42.5 - 47.5","47.5 - 52.5",
#'  "52.5 - 57.5", "57.5 - 62.5", "62.5 - 67.5", "67.5 - 72.5", ">72.5" mph (miles/h).
#' @param as_list logical; Returns emission factors as a list, 
#' instead of data.table format. Default is TRUE.
#' 
#' @return List. Emission factors in units 'g/km' by speed and model_year.
#' 
#' @details 
#' User can view the pre-processed database in `data(ef_usa_moves_db)` function.
#' 
#' @family Emission factor model
#' 
#' @export
#' 
#' @examples if (interactive()) {
#'  ef_usa_moves(pollutant = c("CO","PM10"),
#'         model_year = 2015,
#'         speed = units::set_units(1:100,"km/h"),
#'         reference_year = 2016,
#'         fuel = "D",
#'         as_list = TRUE)
#'}
ef_usa_moves <- function(pollutant, model_year, reference_year = 2020, speed, fuel = 'D', as_list = TRUE){
  
  #  pollutant = c("CO","PM10","CH4","NOx")
  # # reference_year = "2019"
  #  model_year = c("2014","2013","2010");model_year = c("2010")
  #  speed = units::set_units(33,"km/h")
  #  fuel = c("D","CNG"); fuel = "D"
  # 

  # use specific name-----
  tmp_fuel <- fuel
  tmp_pollutant <- pollutant
  tmp_model_year <- model_year
  tmp_reference_year <- reference_year
  utils::data(ef_usa_moves_db)
  temp_ef <- ef_usa_moves_db
    
  # pre-filter in usa data----
  temp_moves <- temp_ef[reference_year %in% tmp_reference_year &
                          fuel  %in% unique(tmp_fuel) &
                      pollutant %in% unique(tmp_pollutant) & 
                      model_year %in% unique(tmp_model_year), ]
  
  # check units and lengths----
  if(data.table::uniqueN(tmp_reference_year) != 1){
    stop("calendar_date input needs to has length one.")
  }
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
      temp_moves2 <- data.table::copy(temp_moves)[pollutant %in% i & 
                                                    model_year %in% tmp_model_year[j] & 
                                                    fuel %in% tmp_fuel[j], ]
      # message(paste("pollutant",i, 
      #                  "| model_year", tmp_model_year[j],
      #                  "| fuel", tmp_fuel[j]))
      # check condition
      if(dim(temp_moves2)[1] == 0){
        stop("Emission Factor do not exist. \nPlease check `data(ef_usa_moves_db)` for valid emission factors.")
      }
      return(temp_moves2[, ef])
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
  tmp_limit <-  unique(temp_moves$upper_speed_interval) 
  tmp_limit <-  sort(as.numeric(tmp_limit),decreasing = TRUE)
  
  temp_order_dt <- data.table::data.table(
    "limit" = tmp_limit
    ,"id" = data.table::uniqueN(temp_moves$upper_speed_interval):1)
  
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

