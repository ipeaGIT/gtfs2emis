#' @title Emission factor dependent on speed by EMEP/EEA
#' 
#' @description Returns a vector or data.frame of emission factors for buses based on EMEP/EEA.
#' Function based on values from [EMEP/EEA air pollutant emission inventory guidebook 2019](https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook).
#' Estimates expressed in units 'g/km'.
#' 
#' @param speed units; Speed in 'km/h'.
#' @param veh_type character; Bus type, classified in "Urban Buses Midi <=15 t",
#' "Urban Buses Standard 15 - 18 t", "Urban Buses Articulated >18 t", "Coaches Standard <=18 t",
#' "Coaches Articulated >18 t", "Urban CNG Buses", and "Urban Biodiesel Buses".
#' @param euro character; Euro period of vehicle, classified in "Conventional", "I", "II",
#' "Euro III", "IV", "V", "VI", and "EEV".
#' @param pollutant character; Pollutant, classified in "CO", "NOx", "VOC", "PM", "FC" (Fuel
#' Consumption), "CH4", "NH3" and "N2O".
# @param aggregate Logical; does the emission factor should be aggregated? Default is TRUE.
# @param veh_distribution Numeric; Distribution of vehicle type, required only when 
# aggregate == TRUE.
#' @param fuel character; Fuel type, classified in "Diesel", "CNG", and "Biodiesel". Default is 
#' "Diesel".
#' @param tech character; Technology, classified in "SCR", "EGR", and "DPF+SCR". Default is "SCR".
#' @param slope numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 
#' 0.06.
#'  Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param k numeric; constant value to adjust emission factors. Default is 1.0.
#' @param fcorr numeric; Correction based on fuel composition. The length must be one per
#' each euro standards. Default is 1.0.
#' @param show.equation logical; show.equation from EMEP/EEEA used? Default parameter is TRUE.
#' @return emission factors in units 'g/km' (a vector or a data.frame).
#' @export
ef_europe <- function(speed, veh_type,  euro,  pollutant, fuel = "Diesel", tech = "SCR", 
                      slope = 0.0, load = 0.5, k = 1, fcorr = 1, show.equation = FALSE){
  # local test
  # speed <- vein::Speed(1:100)
  # veh_type <- "Urban Buses Standard 15 - 18 t"
  # euro <- c("IV","V")
  # pollutant <- c("CO","NOx")
  # fuel <- "Diesel";tech <- "SCR"; slope = 0.0; load = 0.5; k =1; fcorr = 1;i = j = 1
  
  # euro vector----
  
  euro <- paste0("Euro ", euro)
  temp_ef <- gtfs2emis::europe
  
  # check units and lengths----
  
  if(class(speed) != "units"){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'.")
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("speed need to has 'units' in 'km/h'.")
  }
  
  speed <- round(as.numeric(speed))
  speed[speed < 1] <- 1
  maxSpeed <- max(speed)
  
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
  if(length(euro) != length(k) && length(k) == 1){
    k <- rep(k,length(euro))
  }
  if(length(fcorr) == 1){
    fcorr <- rep(fcorr,length(euro))
  }
  if(length(slope) == 1){
    slope <- rep(slope,length(euro))
  }
  if(length(load) == 1){
    load <- rep(load,length(euro))
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
      
      if(euro[i] %in%  c("Conventional","Euro I","Euro II","Euro III")){
        tech[i] = NA
        message(paste0("no technology associated with ", euro[i]))
      }
      temp_ef3 <- temp_ef[Fuel %in% fuel[i] & 
                            Segment %in% veh_type[i] & 
                            Euro.Standard %in% euro[i] & 
                            Technology %in% tech[i] &
                            Pollutant %in% pollutant[j] &
                            Road.Slope %in% slope[i] &
                            Load %in% load[i], ]
      
      if(nrow(temp_ef3) == 0){
        erro_msg <- paste("No available emission factor for combination of the following parameters\n",
                       "fuel = ",fuel[i],"| veh_type = ",veh_type[i],
                       "\n euro = ",euro[i],"| tech = ",tech[i],
                      "| pollutant = ",pollutant[j],"| slope = ",slope[i],
                      "| load = ",load[i],
                      "\n Please check `data(europe)` for available data.")
        stop(erro_msg)
      }
      
      # fix speed
      tmpSpeed <- 1:maxSpeed
      
      tmpSpeed[tmpSpeed < temp_ef3$`Min.Speed.[km/h]`] <- temp_ef3$`Min.Speed.[km/h]`
      tmpSpeed[tmpSpeed > temp_ef3$`Max.Speed.[km/h]`] <- temp_ef3$`Max.Speed.[km/h]`
      
      # return
      
      eq_output <- eq_num(a = temp_ef3$Alpha, b = temp_ef3$Beta, g = temp_ef3$Gamma,
                          d = temp_ef3$Delta, e = temp_ef3$Epsilon, z = temp_ef3$Zita,
                          h = temp_ef3$Hta, rf = temp_ef3$`Reduction.Factor.[%]`,
                          v = tmpSpeed, k = k[i] * fcorr[i])
      return(eq_output)
    })
    
    # do not aggregate emission factors
    temp_ef3 <- do.call(cbind, temp_ef2) %>% 
      units::set_units('g/km') %>% data.table::as.data.table()
    names(temp_ef3) <- paste0(pollutant[j], "_", rep(euro))
    return(temp_ef3)
  })
  
  # show.equation----
  
  if (show.equation) {
    message(paste0("ef = (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k"))
  }
  
  # return in a data.table like format----
  
  ef_final <- do.call(cbind, temp_ef1)
  return(ef_final[speed, ])
}
