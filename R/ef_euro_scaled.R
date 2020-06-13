#' @title Scaled emission factors from European (EMEP/EEA) methodology
#' 
#' @description Estimates scaled emission factor for buses based on EMEP/EEA 
#' 
#' @param dfcol Units; Local emission factors, in units [g/km]
#' @param speed Units; Speed, in units [km/h]
#' @param veh_type Character; Bus type, classified in "Urban Buses Midi <=15 t", "Urban Buses Standard 15 - 18 t", 
#' "Urban Buses Articulated >18 t", "Coaches Standard <=18 t"       
#' "Coaches Articulated >18 t", "Urban CNG Buses", "Urban Biodiesel Buses"  
#' @param fuel Character; Fuel type, classified in "Diesel", "CNG"  and "Biodiesel"
#' @param euro Character; Euro period of vehicle, classified in "Conventional", "Euro I", "Euro II", "Euro III", 
#' "Euro IV", "Euro V", "Euro VI" and "EEV"  
#' @param SDC Numeric; Speed Driving Cycle (default = 34.12)
#' @param tech Character; Technology, classified in "SCR", "EGR", "DPF+SCR" or NA (Not available). For EURO IV, V, default is "SCR".
#' For Euro III, default is NA.
#' @param pollutant Character; Pollutant, classified in "CO", "NOx", "VOC", "PM", "FC", "CH4", "NH3" and "N2O"
#' @param slope Numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 0.06.
#'  Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load Numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param show.equation Logical; show.equation from EMEP/EEEA used? Default parameter is TRUE.
#' @return Emission factors in units g/km
#' @export
ef_euro_scaled <- function(dfcol,speed,veh_type,fuel,euro,SDC = 34.12,
                          tech = "SCR",pollutant,slope = 0.0,load = 0.5,show.equation = TRUE){
  #
  # test
  # dfcol = units::set_units(0.5,g/km); speed = 30
  # fuel = "Diesel"; euro = "Euro V"
  # SDC = 34.12; tech = "SCR"; veh = "Urban Buses Standard 15 - 18 t"
  # pollutant = "CO"; slope = 0.0; load = 0.5; show.equation = TRUE
  #
  # adjust based on local emission factor
  #
  dfcol <- as.numeric(dfcol)
  temp_ef <- lapply(1:length(dfcol), function(i) { # i = 1
    funIN <- ef_europe(speed = SDC,veh_type = veh_type,fuel = "Diesel",
                          euro = euro,
                          tech = tech,pollutant = pollutant,show.equation = FALSE)
    k <- dfcol[i]/funIN
    ef_scaled <- ef_europe(speed = speed,veh_type = veh_type,
                              fuel = "Diesel",
                              euro = euro,
                              tech = tech,pollutant = pollutant,k = k,show.equation = show.equation)
    return(ef_scaled)
  }) %>% unlist()
  return(units::set_units(temp_ef,'g/km'))
}
