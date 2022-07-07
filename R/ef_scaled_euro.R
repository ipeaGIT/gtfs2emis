#' @title 
#' Scaled emission factors from the European Environment Agency (EMEP/EEA) model
#' 
#' @description 
#' Scale the local emission factors with EMEP/EEA EF model, in order to make the local emission
#' a function of speed.
#' 
#' @param ef_local data.frame or a list containing the emission factors data.frame;
#' Local emission factors, in units 'g/km'.
#' @param speed units; Speed in 'km/h'.
#' @param veh_type character; Bus type, classified in "Ubus Midi <=15 t",
#' "Ubus Std 15 - 18 t","Ubus Artic >18 t",
#' "Coaches Std <=18 t","Coaches Artic >18 t".
#' @param euro character; Euro period of vehicle, classified in "Conventional", 
#' "I", "II", "III", "IV", "V", "VI", and "EEV".
#' @param pollutant character; Pollutant, classified in "FC","CO2","CO","NOx","VOC",
#' "PM10","EC","CH4","NH3","N2O". "FC" means Fuel Consumption. 
#' @param fuel character; Fuel type, classified in "D" (Diesel),"DHD" 
#' (Diesel Hybrid ~ Diesel), "DHE" (Diesel Hybrid ~ Electricity),
#'  "CNG" (Compressed Natural Gas), "BD" (Biodiesel). Default is "D". 
#' @param tech character; After treatment technology, classified in "SCR" 
#' (Selective Catalytic Reduction), 
#' "EGR" (Exhaust Gas Recirculation), and "DPF+SCR" 
#' (Diesel Particulate Filter + SCR, for Euro VI). Default is "SCR" for "IV" and "V".
#' @param SDC numeric; Average speed of urban driving condition in 'km/h'. Default is 19 km/h, 
#' which is the average speed adopted in EMEP/EEA report.
#' @param slope numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02,
#'  0.04 and 0.06. Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param fcorr numeric; Correction based on fuel composition. The length must be one per
#' each euro standards. Default is 1.0.
#' 
#' @return list. Emission factors in units 'g/km'.
#' @details 
#' 
#' The scaled emission factor is related to speed by the expression
#' 
#'  EF_scaled (V) = EF_local * ( EF(V) / EF(SDC)), 
#'  
#'  where EF_scaled(V) is the scaled emission factors for each street link, 
#'  EF_local is the local emission factor, EF(V) and EF(SDC) are the EMEP/EEA
#'  emission factor the speed of V and the average urban driving speed 'SDC', respectively.
#'
#' Please note that the function reads the vector arguments in the same order 
#' as informed by the user. For instance, if the pollutant input is `c("CO","PM10")`
#' input in the local emission factor function, the order needs to be the same for the
#' pollutant in the `ef_scaled_euro` function. 
#' 
#' In the case of vehicle type, which generally changes according to the emission 
#' factor source, the input argument in the `ef_scaled_euro` needs to be consistent
#' with the order adopted in the local emission factor function.
#' 
#' For example, if the vector of local vehicle type is 
#'  `c("BUS_URBAN_D","BUS_MICRO_D")`, the related vector for EMEP/EEA model needs
#'  to be `c("Ubus Std 15 - 18 t","Ubus Midi <=15 t")`. The same approach applies for
#'  other input arguments. See more in the examples. 
#'   
#' 
#' @family Emission factor model
#' 
#' @export
#' 
#' @examples if (interactive()) {
#' 
#' temp_ef_br <- ef_brazil_cetesb(pollutant = c("CO","PM10","CO2","CH4","NOx"),
#'                         veh_type = c("BUS_URBAN_D","BUS_MICRO_D"),
#'                         model_year = c(2015,2015),
#'                         as_list = TRUE)
#' 
#' temp_ef_scaled <- ef_scaled_euro(ef_local = temp_ef_br
#'                                  , speed = units::set_units(1:100,"km/h")
#'                                  , veh_type = c("Ubus Std 15 - 18 t","Ubus Midi <=15 t")
#'                                  , euro = c("IV","IV")
#'                                  , fuel = c("D","D")
#'                                  , tech = c("SCR","SCR")
#'                                  , pollutant = c("CO","PM10","CO2","CH4","NOx"))
#'} 
ef_scaled_euro <- function(ef_local, speed, veh_type, euro, pollutant, fuel = "D", 
                           tech = "SCR", SDC = 19, slope = 0.0, 
                           load = 0.5, fcorr = 1){
  # local test
  # ef_local = ef_brazil(pollutant = c("CO","PM"),
  #                      veh_type = "BUS_URBAN_D",
  #                      years = c(2008:2017),as_list = TRUE)
  # speed = units::set_units(rep(1,1),"km/h")
  # veh_type = rep("Ubus Std 15 - 18 t",10)
  # euro = c(rep("IV",5),rep("V",5))
  # fuel = rep("D",10)
  # pollutant = c("CO","PM10")
  # SDC = 34.12; tech = rep("SCR",10); slope = 0.0; load = 0.5; fcorr = 1;
  
  
  checkmate::assert_class(pollutant, 'character')
  #
  # check dimensions
  #
  
  if (is(ef_local, "list")) {
    ef_local <- ef_local$EF
  }
  if (length(veh_type) == 1) {
    veh_type <- rep(veh_type, length(euro))
  }
  if (length(veh_type) != length(euro)) {
    stop("'euro' and 'veh_type' need to have the same length.")
  }
  if (length(fuel) == 1) {
    fuel <- rep(fuel,length(euro))
  }
  if (length(fuel) != length(euro)) {
    stop("'fuel' and 'veh_type' need to have the same length.")
  }
  
  #
  # check units
  #
  
  lapply(seq_along(ef_local), function(i){ # i = 1
    if (!is(ef_local, "units")) {
      stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
    }
    if(units(ef_local[[i]])$numerator != "g" | units(ef_local[[i]])$denominator != "km"){
      stop("ef need to has 'units' in 'g/km'.")
    }
  })
  
  #
  # ef_europe with SDC (urban driving speed condition)
  #
  
  euro1 <- euro
  ef_sdc <- ef_europe_emep(speed = units::set_units(SDC, "km/h"), 
                      veh_type = veh_type,
                      euro = euro1,
                      pollutant = pollutant,
                      fuel = fuel,
                      tech = tech,
                      slope = slope,
                      load = load, 
                      fcorr = fcorr,
                      as_list = FALSE)
  
  # ef_europe(speed = units::set_units(rnorm(100,50,5),"km/h"),
  #                 veh_type = c("Urban Buses Standard 15 - 18 t","Urban Buses Articulated >18 t"),
  #                 euro = c("IV","V"),
  #                 pollutant = c("CO2","NOx"),
  #                 fuel = "Diesel" ,
  #                 tech =  c("SCR","EGR"),
  #                 slope = 0.0,
  #                 load = 0.5,
  #                 fcorr = 1,
  #                 as_list = TRUE)
  # adjustment factor
  k <- as.numeric(ef_local)/as.numeric(ef_sdc) 
  
  # ef europe speed
  ef_speed <- ef_europe_emep(speed = speed, 
                        veh_type = veh_type,
                        fuel = fuel,
                        euro = euro1,
                        slope = slope,
                        load = load,
                        tech = tech, 
                        fcorr = fcorr,
                        pollutant = pollutant,
                        as_list = TRUE)
  ef_speed$EF <- as.matrix(ef_speed$EF)
  
  #
  # ef_scaled
  #
  
  ef_scaled <- sapply(seq_along(k),function(i){ef_speed$EF[,i] * k[i]})
  if(length(speed) == 1){
    ef_scaled <- t(as.matrix(ef_scaled))
  }
  #ef_scaled <- sapply(seq_along(k),function(i){ef_speed$EF[,i] * k[i]})
  
  # to units
  ef_scaled <- units::set_units(ef_scaled, 'g/km')
  ef_scaled <- data.table::as.data.table(ef_scaled)
  colnames(ef_scaled) <- colnames(ef_speed$EF)
  # add back to list
  ef_speed$EF <- ef_scaled
  
  return(ef_speed)
}
