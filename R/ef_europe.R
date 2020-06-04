#' @title Emission factor dependent on speed by EMEP/EEA
#' 
#' @description Estimates emission factor for buses based on EMEP/EEA 
#' 
#' @param vel Units; Speed in [km/h]
#' @param veh_type Character; Bus type, classified in "Urban Buses Midi <=15 t", "Urban Buses Standard 15 - 18 t", 
#' "Urban Buses Articulated >18 t",  "Coaches Standard <=18 t",
#' "Coaches Articulated >18 t", "Urban CNG Buses", and "Urban Biodiesel Buses".  
#' @param fuel Character; Fuel type, classified in "Diesel", "CNG"  and "Biodiesel".
#' @param euro Character; Euro period of vehicle, classified in "Conventional", "I", "II", "Euro III", 
#' "IV", "V", "VI" and "EEV".
#' @param tech Character; Technology, classified in "SCR", "EGR" and "DPF+SCR".
#' @param pol Character; Pollutant, classified in "CO", "NOx", "VOC", "PM", "FC" (Fuel Consumption), "CH4", "NH3" and "N2O".
#' @param aggregate Logical; does the emission factor should be aggregated? Default is TRUE.
#' @param veh Numeric; Distribution of vehicle type, required only when aggregate == TRUE.
#' @param slope Numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 0.06.
#'  Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load Numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param show.equation Logical; show.equation from EMEP/EEEA used? Default parameter is TRUE.
#' @param k Numeric; constant value to adjust emission factors. Default is 1.0.
#' @return Emission factors in units g/km.
#' @export
ef_europe <- function(vel, veh_type, veh, fuel = "Diesel", aggregate = TRUE, euro, tech, pol, slope = 0.0, load = 0.5, k = 1, show.equation = TRUE){
  # vel <- units::set_units(poa_gpslines$speed,"km/h")
  # veh_type <- "Urban Buses Standard 15 - 18 t"
  # euro <- c("IV","V")
  # veh <- c(0.5,0.5)
  # pol <- c("CO","NOx")
  # tech <- "SCR"
  # fuel <- "Diesel"; slope = 0.0; load = 0.5; k =1
  # aggregate = TRUE
  # 
  # euro vector
  #
  euro <- paste0("Euro ",euro)
  temp_ef0 <- ef
  # check speed units
  #
  if(class(vel) != "units"){
    stop("vel neeeds to has class 'units' in 'km/h'. Please, check package 'units'")
  }
  if(units(vel)$numerator != "km" | units(vel)$denominator != "h"){
    stop("vel need to has 'units' in 'km/h'.")
  }
  vel <- as.numeric(vel)
  #
  # check lengths
  #
  if(length(euro) != length(veh_type) && length(veh_type) == 1){
    veh_type <- rep(veh_type,length(euro))
  }
  if(length(euro) != length(tech) && length(tech) == 1){
    tech <- rep(tech,length(euro))
  }
  if(length(euro) != length(fuel) && length(fuel) == 1){
    fuel <- rep(fuel,length(euro))
  }
  #
  # polynomial expression
  #
  eq_num <- function(a,b,g,d,e,z,h,rf,v,k){
    eq <- (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k
    return(eq)
  }
  #
  # emission factor
  #
  temp_ef <- lapply(seq_along(pol),function(j){ # j = 1
    temp_ef1 <- lapply(seq_along(euro),function(i){ # i = 1
      #
      # pre-conditions
      #
      if(euro[i] == "Euro III" | euro[i] == "Euro II"){
        tech[i] = NA
        message(paste0("no technology associated with ",euro[i]))
      }
      temp_ef2 <- temp_ef0[Fuel %in% fuel[i] & 
                       Segment %in% veh_type[i] & 
                       Euro.Standard %in% euro[i] & 
                       Technology %in% tech[i] &
                       Pollutant %in% pol[j] &
                       Road.Slope %in% slope &
                       Load %in% load,]
      
      if(nrow(temp_ef2) == 0){
        stop("No available emission factor. Please check `data(ef)` for available data.")
      }
      #
      # fix speed
      #
      if(length(which(vel < temp_ef2$`Min.Speed.[km/h]`)) > 0) vel[vel < temp_ef2$`Min.Speed.[km/h]`] <- temp_ef2$`Min.Speed.[km/h]`
      if(length(which(vel > temp_ef2$`Max.Speed.[km/h]`)) > 0) vel[vel > temp_ef2$`Max.Speed.[km/h]`] <- temp_ef2$`Max.Speed.[km/h]`
      #
      # return
      #
      eq_output <- eq_num(a = temp_ef2$Alpha,b = temp_ef2$Beta,g = temp_ef2$Gamma,
                          d = temp_ef2$Delta,e = temp_ef2$Epsilon,z = temp_ef2$Zita,
                          h = temp_ef2$Hta,rf = temp_ef2$`Reduction.Factor.[%]`,
                          v = vel, k = k)
      return(eq_output)
    })
    #
    # aggregate
    #
    if(aggregate){
      if(is.null(veh)){stop("Veh file is missing")}else{
        #
        # average ef
        temp_ef_avg1 <- lapply(seq_along(temp_ef1),function(k){ # k = 1
          temp_ef1[[k]] * veh[k]
        }) 
        temp_ef_avg <- do.call(cbind,temp_ef_avg1) %>% rowSums() %>% 
          units::set_units('g/km') %>% data.table::as.data.table()
        names(temp_ef_avg) <- paste0(pol[j],"_avg")
        
        return(temp_ef_avg)
      }
    }else{
      #
      # do not aggregate emission factors
      temp_ef3 <- do.call(cbind,temp_ef1) %>% 
        units::set_units('g/km') %>% data.table::as.data.table()
      names(temp_ef3) <- paste0(temp_pol,"_",euro)
      return(temp_ef3)
    }
  })
  #
  # show.equation
  #
  #if (show.equation == TRUE) {
  #  cat(paste0("a = ", temp_ef1$Alpha, ", b = ", temp_ef1$Beta, ", g = ", 
  #             temp_ef1$Gamma, ", d = ", temp_ef1$Delta, ", e = ", temp_ef1$Epsilon, ", rf = ", 
  #             temp_ef1$`Reduction.Factor.[%]`, ", z = ", temp_ef1$Zita, ", h = ", temp_ef1$Hta, "\n"))
  #  cat(paste0("ef = (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k"))
  #}
  #
  # return in a data.table like format
  ef_final <- do.call(cbind,temp_ef)
  return(ef_final)
}
