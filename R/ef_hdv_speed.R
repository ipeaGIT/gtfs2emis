#' @title Emission factor dependent on speed by EMEP/EEA
#' 
#' @description Estimates emission factor for buses based on EMEP/EEA 
#' 
#' @param vel Numeric; Speed in [km/h]
#' @param veh Character; Bus type, classified in "Urban Buses Midi <=15 t", "Urban Buses Standard 15 - 18 t", 
#' "Urban Buses Articulated >18 t", "Coaches Standard <=18 t"       
#' "Coaches Articulated >18 t", "Urban CNG Buses", "Urban Biodiesel Buses"  
#' @param fuel Character; Fuel type, classified in "Diesel", "CNG"  and "Biodiesel"
#' @param euro Character; Euro period of vehicle, classified in "Conventional", "Euro I", "Euro II", "Euro III", 
#' "Euro IV", "Euro V", "Euro VI" and "EEV"  
#' @param tech Character; Technology, classified in "SCR", "EGR" and "DPF+SCR"
#' @param pol Character; Pollutant, classified in "CO", "NOx", "VOC", "PM", "FC", "CH4", "NH3" and "N2O"
#' @param slope Numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 0.06.
#'  Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load Numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param show.equation Logical; show.equation from EMEP/EEEA used? Default parameter is TRUE.
#' @param k Numeric; constant value to adjust emission factors. Default is 1.0.
#' @return Emission factors in units g/km
#' @export
ef_hdv_speed <- function(vel,veh,fuel,euro,tech,pol,slope = 0.0,load = 0.5,k = 1,show.equation = TRUE){
  #
  # emission factor
  #
  temp_ef <- ef
  vel <- as.numeric(vel)
  #
  # pre-conditions
  #
  if(euro == "Euro III" | euro == "Euro II"){
    tech = NA
    message("no technology associated with Euro III")
  }
  #
  # expression
  #
  eq_num <- function(a,b,g,d,e,z,h,rf,v,k){
    eq <- (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k
    return(eq)
  }
  #
  # ef
  #
  temp_ef1 <- temp_ef[Fuel %in% fuel & 
              Segment %in% veh & 
              Euro.Standard %in% euro & 
              Technology %in% tech &
              Pollutant %in% pol &
              Road.Slope %in% slope &
              Load %in% load,]
  
  if(nrow(temp_ef1) == 0){return(message("No ef"))}
  if(nrow(temp_ef1) > 1){temp_ef1 <- temp_ef1[1,];message("More than one")}
  if(nrow(temp_ef1) == 1){temp_ef1 <- temp_ef1}
  #
  # show.equation
  #
  if (show.equation == TRUE) {
    cat(paste0("a = ", temp_ef1$Alpha, ", b = ", temp_ef1$Beta, ", g = ", 
               temp_ef1$Gamma, ", d = ", temp_ef1$Delta, ", e = ", temp_ef1$Epsilon, ", rf = ", 
               temp_ef1$`Reduction.Factor.[%]`, ", z = ", temp_ef1$Zita, ", h = ", temp_ef1$Hta, "\n"))
    cat(paste0("ef = (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k"))
  }
  #
  # fix speed
  #
  if(length(which(vel < temp_ef1$`Min.Speed.[km/h]`)) > 0) vel[vel < temp_ef1$`Min.Speed.[km/h]`] <- temp_ef1$`Min.Speed.[km/h]`
  if(length(which(vel > temp_ef1$`Max.Speed.[km/h]`)) > 0) vel[vel > temp_ef1$`Max.Speed.[km/h]`] <- temp_ef1$`Max.Speed.[km/h]`
  #
  # return
  #
  eq_output <- eq_num(a = temp_ef1$Alpha,b = temp_ef1$Beta,g = temp_ef1$Gamma,d = temp_ef1$Delta,
                      e = temp_ef1$Epsilon,z = temp_ef1$Zita,h = temp_ef1$Hta,
                      rf = temp_ef1$`Reduction.Factor.[%]`, v = vel, k = k)
  eq_output <- units::set_units(eq_output,g/km)
  
  return(eq_output)
}
