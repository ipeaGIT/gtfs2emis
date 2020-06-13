#' @title Emission factor dependent on speed by EMEP/EEA
#' 
#' @description Estimates emission factor for buses based on EMEP/EEA 
#' 
#' @param speed Units; Speed in [km/h]
#' @param veh_type Character; Bus type, classified in "Urban Buses Midi <=15 t", "Urban Buses Standard 15 - 18 t", 
#' "Urban Buses Articulated >18 t",  "Coaches Standard <=18 t",
#' "Coaches Articulated >18 t", "Urban CNG Buses", and "Urban Biodiesel Buses".  
#' @param fuel Character; Fuel type, classified in "Diesel", "CNG"  and "Biodiesel".
#' @param euro Character; Euro period of vehicle, classified in "Conventional", "I", "II", "Euro III", 
#' "IV", "V", "VI" and "EEV".
#' @param tech Character; Technology, classified in "SCR", "EGR" and "DPF+SCR".
#' @param pollutant Character; Pollutant, classified in "CO", "NOx", "VOC", "PM", "FC" (Fuel Consumption), "CH4", "NH3" and "N2O".
# @param aggregate Logical; does the emission factor should be aggregated? Default is TRUE.
# @param veh_distribution Numeric; Distribution of vehicle type, required only when aggregate == TRUE.
#' @param slope Numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 0.06.
#'  Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load Numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param show.equation Logical; show.equation from EMEP/EEEA used? Default parameter is TRUE.
#' @param k Numeric; constant value to adjust emission factors. Default is 1.0.
#' @return Emission factors in units g/km.
#' @export
ef_europe <- function(speed, veh_type, fuel = "Diesel", euro, tech, pollutant,slope = 0.0, load = 0.5, k = 1, show.equation = TRUE){
  # speed <- units::set_units(poa_gpslines$speed,"km/h")
  # veh_type <- "Urban Buses Standard 15 - 18 t"
  #  euro <- c("IV","V")
  # # veh_distribution <- c(0.5,0.5)
  #  pollutant <- c("CO","NOx")
  # tech <- "SCR"
  # fuel <- "Diesel"; slope = 0.0; load = 0.5; k =1
  # aggregate = TRUE
  
  # euro vector----
  
  euro <- paste0("Euro ",euro)
  temp_ef <- europe
  
  # check units and lengths----
  
  if(class(speed) != "units"){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'")
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("speed need to has 'units' in 'km/h'.")
  }
  speed <- as.numeric(speed)
  
  # check lengths
  
  if(length(euro) != length(veh_type) && length(veh_type) == 1){
    veh_type <- rep(veh_type,length(euro))
  }
  if(length(euro) != length(tech) && length(tech) == 1){
    tech <- rep(tech,length(euro))
  }
  if(length(euro) != length(fuel) && length(fuel) == 1){
    fuel <- rep(fuel,length(euro))
  }
  
  # polynomial expression----
  
  eq_num <- function(a,b,g,d,e,z,h,rf,v,k){
    eq <- (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k
    return(eq)
  }
  
  # emission factor----
  
  temp_ef1 <- lapply(seq_along(pollutant),function(j){  # j = 1
    temp_ef2 <- lapply(seq_along(euro),function(i){    # i = 1
      
      # pre-conditions
      
      if(euro[i] == "Euro III" | euro[i] == "Euro II"){
        tech[i] = NA
        message(paste0("no technology associated with ",euro[i]))
      }
      temp_ef3 <- temp_ef[Fuel %in% fuel[i] & 
                            Segment %in% veh_type[i] & 
                            Euro.Standard %in% euro[i] & 
                            Technology %in% tech[i] &
                            Pollutant %in% pollutant[j] &
                            Road.Slope %in% slope &
                            Load %in% load,]
      
      if(nrow(temp_ef3) == 0){
        stop("No available emission factor. Please check `data(europe)` for available data.")
      }
      
      # fix speed
      
      if(length(which(speed < temp_ef3$`Min.Speed.[km/h]`)) > 0) speed[speed < temp_ef3$`Min.Speed.[km/h]`] <- temp_ef3$`Min.Speed.[km/h]`
      if(length(which(speed > temp_ef3$`Max.Speed.[km/h]`)) > 0) speed[speed > temp_ef3$`Max.Speed.[km/h]`] <- temp_ef3$`Max.Speed.[km/h]`
      
      # return
      
      eq_output <- eq_num(a = temp_ef3$Alpha,b = temp_ef3$Beta,g = temp_ef3$Gamma,
                          d = temp_ef3$Delta,e = temp_ef3$Epsilon,z = temp_ef3$Zita,
                          h = temp_ef3$Hta,rf = temp_ef3$`Reduction.Factor.[%]`,
                          v = speed, k = k)
      return(eq_output)
    })
    
      # do not aggregate emission factors
      
      temp_ef3 <- do.call(cbind,temp_ef2) %>% 
        units::set_units('g/km') %>% data.table::as.data.table()
      names(temp_ef3) <- paste0(pollutant[j],"_",rep(euro))
      return(temp_ef3)
    #}
  })
  
  # show.equation----
  
  if (show.equation == TRUE) {
   # cat(paste0("a = ", temp_ef1$Alpha, ", b = ", temp_ef1$Beta, ", g = ",
   #            temp_ef1$Gamma, ", d = ", temp_ef1$Delta, ", e = ", temp_ef1$Epsilon, ", rf = ",
   #            temp_ef1$`Reduction.Factor.[%]`, ", z = ", temp_ef1$Zita, ", h = ", temp_ef1$Hta, "\n"))
   cat(paste0("ef = (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k"))
  }

  # return in a data.table like format----
  
  ef_final <- do.call(cbind,temp_ef1)
  return(ef_final)
}
