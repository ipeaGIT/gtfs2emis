#' @title 
#' Running exhaust emissions factors from United States (EMFAC2017 model)
#' 
#' @description 
#' Returns a vector or data.frame of emission factors for buses based on 
#' the [California EMission Factor model (EMFAC2017)](https://arb.ca.gov/emfac/).
#' The model considers emission factors (EF) of urban buses in California (United States),
#'  considering different pollutants, years of reference, model year, fuel, speed ranges,
#' type of regions, model version, and type of season. The gtfs2emis package 
#' currently supports EF only for "Statewide" region type, and "Annual" season. 
#' Specific data of these variables can be download at 
#' <<https://arb.ca.gov/emfac/emissions-inventory>>.
#' 
#' The package returns the data in a data.frame format. The R script used to 
#' organize the EMFAC database can be found in the repository
#' <<https://github.com/ipeaGIT/gtfs2emis/blob/master/data-raw/ef_usa_emfac_db.R>>.
#'
#' @param pollutant Character; Pollutants: CH4(Methane), CO(Carbon Monoxide), 
#' CO2(Carbon Dioxide), N2O(Nitrous Oxide), NOx(Oxides of Nitrogen),
#'  PM10(Primary Exhaust PM10 - Total), PM25(Primary Exhaust PM2.5 - Total),
#'   SOX(Oxides of Sulfur), TOG(Total Organic Gases), ROG (Reactive Organic Gases)
#' @param reference_year Numeric; Calendar Year between 2015 - 2022. Year in which
#'  the emissions inventory is estimated. Default is 2020.
#' @param fuel Character; Type of fuel: 'D' (Diesel),'G' (Gasoline),
#' 'CNG' (Compressed Natural Gas). Default is 'D'.
#' @param model_year Numeric; Model year of vehicle.
#' @param speed Units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40",
#'   "40-45", "45-50", "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85"
#'   , "85-90", ">90" mph (miles/h).
#' @param as_list logical; Returns emission factors as a list, instead of data.table format.
#'  Default is TRUE.
#' 
#' @return List or data.table. Emission factors in units 'g/km' by speed and model_year.
#' @source \url{https://arb.ca.gov/emfac/}
#' 
#' @family Emission factor model
#' 
#' @export
#' 
#' @examples if (interactive()) {
#'  ef_usa_emfac(pollutant = c("CO","PM10"),
#'         reference_year = 2019,
#'         model_year = 2015,
#'         speed = units::set_units(1:100,"km/h"),
#'         fuel = "D",
#'         as_list = TRUE)
#'}
ef_usa_emfac <- function(pollutant, reference_year = 2020, fuel = 'D'
                         , model_year, speed, as_list = TRUE){
  
  # pollutant = c("CO","PM10","CH4","NOx")
  # reference_year = "2019"
  # model_year = c("2014","2013","2010");model_year = c("2010")
  # speed = units::set_units(33,"km/h")
  # fuel = c("Diesel","CNG"); fuel = "D"
  # 
  
  # Use specific name-----
  tmp_fuel <- fuel
  tmp_reference_year <- reference_year
  tmp_pollutant <- pollutant
  tmp_model_year <- model_year
  utils::data('ef_usa_emfac_db') 
  
  # Checkings -----
  # pollutant
  lapply(pollutant,function(i){
    if(!(i %in% unique(ef_usa_emfac_db$pollutant))){
      stop(
        paste0("Invalid input: pollutant '",i,"' argument not found.\n", 
               "Please check available data in `data(ef_usa_emfac_db)`.")
      )
    }
  })
  # reference_year
  if(!is.numeric(reference_year)){
    stop(
      "Invalid input: 'reference_year' argument should be a numeric value."
    )
  }
  if(length(reference_year) != 1){
    stop(
      "Invalid input: only one 'reference_year' is accepted."
    )
  }
  if(!(reference_year %in% 2010:2020)){
    stop(
      paste0("Invalid input: 'reference_year' argument should be between 2010 - 2020:\n", 
             "Please check available data in `data(ef_usa_emfac_db)`.")
    )
  }
  # fuel
  lapply(fuel,function(i){
    if(!(i %in% c("D","CNG","G"))){
      stop(
        paste0("Invalid input: fuel '",i,"' argument not found.\n"
               ,"Please check `utils::data('ef_usa_emfac_db')` for a valid 'fuel' input.")
      )
    }
  })
  # model_year
  lapply(model_year,function(i){
    if(!(i %in% 1982:2020)){
      stop(
        paste0("Invalid input: model_year '",i,"' argument not found.\n"
               ,"Please check `utils::data('ef_usa_emfac_db')` for a valid 'model_year' input.")
      )
    }
  })
  # check units and lengths
  if(class(speed) != "units"){
    stop(paste0("Invalid 'speed' argument: 'speed' needs to have class 'units' in 'km/h'.\n"
                ,"Please, check package 'units'"))
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("Invalid 'speed' argument: 'speed' needs to have 'units' in 'km/h'.")
  }
  if(sum(speed <= units::set_units(0,"km/h")) > 0){
    stop("Invalid 'speed' argument: 'speed' argument should be greater than 0 km/h.")
  }
  if(sum(speed > units::set_units(113,"km/h")) > 0){
    stop("Invalid 'speed' argument: 'speed' argument should be smaller than 113 km/h.")
  }
    # Pre-filter in usa data----
  temp_emfac <- ef_usa_emfac_db[reference_year %in% as.character(unique(tmp_reference_year)) &
                                  fuel %in% unique(tmp_fuel) &
                                  pollutant %in% unique(tmp_pollutant) & 
                                  model_year %in% unique(tmp_model_year), ]
  

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
        stop(paste0("Invalid inputs: Emission Factor do not exist.\n"
                    ,"Please check `data(ef_usa_emfac_db)` for valid emission factors."))
      }
      return(temp_emfac2[, ef])
    }) 
    temp_ef0 <- data.table::as.data.table(temp_ef0)
    names(temp_ef0) <- paste(i, as.character(tmp_model_year),tmp_fuel,sep = "_")
    return(temp_ef0)
  }) 
  
  # Aggregate EF-----
  ef_final <- do.call(cbind, ef_pol)
  
  # Filter Speed based on emission factor data.table speed interval-----------
  tmp_speed2 <- rep(NA, length(tmp_speed))
  
  # data.table with upper_limit info
  temp_order_dt <- data.table::data.table(
    "limit" =  
      sort(as.numeric(unique(
        temp_emfac$upper_speed_interval)
      ), decreasing = TRUE)
    ,"id" = data.table::uniqueN(temp_emfac$upper_speed_interval):1
  )
  
  for(t in 1:nrow(temp_order_dt)){ # t = 10
    tmp_speed2[tmp_speed <= temp_order_dt$limit[t]] <- temp_order_dt$id[t]
  }
  
  # case of speed lower than upper_limit_speed
  tmp_speed2[tmp_speed2 == 0] <- 1
  
  ef_final <- ef_final[tmp_speed2,]
  
  # add units
  ef_final <- ef_final[, lapply(.SD, units::set_units, "g/km")]
  
  # export-----
  
  # as.list
  if(as_list == TRUE){
    ef_final <- list(
      "pollutant" = rep(pollutant,each = length(tmp_model_year)),
      "model_year" = rep(model_year,length(pollutant)),
      "fuel" = rep(tmp_fuel,length(pollutant)),
      "EF" = ef_final
    )
  }
  return(ef_final)
}

