#' @title Scaled emission factors from European (EMEP/EEA) methodology
#' 
#' @description Estimates scaled emission factor for buses based on EMEP/EEA 
#' 
#' @param ef_local units; Local emission factors, in units 'g/km'.
#' @param speed units; Speed, in units 'km/h'.
#' @param veh_type character; Bus type, classified in "Urban Buses Midi <=15 t",
#' "Urban Buses Standard 15 - 18 t", "Urban Buses Articulated >18 t", "Coaches Standard <=18 t"       
#' "Coaches Articulated >18 t", "Urban CNG Buses", "Urban Biodiesel Buses".
#' @param euro character; Euro period of vehicle, classified in "Conventional", "Euro I", "Euro II",
#' "Euro III", "Euro IV", "Euro V", "Euro VI" and "EEV".
#' @param fuel character; Fuel type, classified in "Diesel", "CNG"  and "Biodiesel".
#' @param pollutant character; Pollutant, classified in "CO", "NOx", "VOC", "PM", "FC", "CH4", "NH3"
#' and "N2O".
#' @param SDC numeric; Speed Driving Cycle (default = 34.12).
#' @param tech character; Technology, classified in "SCR", "EGR", "DPF+SCR" or NA (Not available).
#' For EURO IV, V, default is "SCR". For Euro III, default is NA.
#' @param slope numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and
#' 0.06. Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param fcorr numeric; Correction based on fuel composition. The length must be one per
#' each euro standards. Default is 1.0.
#' @return emission factors in units 'g/km'.
#' @export
ef_euro_scaled <- function(ef_local, speed, veh_type, euro, fuel, pollutant, SDC = 34.12,
                           tech = "SCR", slope = 0.0, 
                           load = 0.5, fcorr = 1){
  # local test
  # ef_local = ef_brazil(pollutant = c("CO","PM"),
  #                      veh_type = "BUS_URBAN_D",
  #                      years = c(2008:2018))
  # speed = vein::Speed(1:100)
  # veh_type = "Urban Buses Standard 15 - 18 t"
  # euro = c(rep("IV",5),rep("V",6))
  # fuel = "Diesel"
  # pollutant = c("CO","PM10")
  # SDC = 34.12; tech = "SCR"; slope = 0.0; load = 0.5; fcorr = 1;
  
  #
  # check dimensions
  #
  
  if(length(veh_type) == 1 && length(veh_type) != length(euro)){
    veh_type <- rep(veh_type,length(euro))
  }else{
    stop("'euro' and 'veh_type' need to have the same length.")
  }
  if(length(fuel) == 1 && length(fuel) != length(euro)){
    fuel <- rep(fuel,length(euro))
  }else{
    stop("'euro' and 'fuel' need to have the same length.")
  }
  
  #
  # check units
  #
  
  lapply(seq_along(ef_local), function(i){ # i = 1
    if(class(ef_local) != "units"){
      stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
    }
    if(units(ef_local[[i]])$numerator != "g" | units(ef_local[[i]])$denominator != "km"){
      stop("ef need to has 'units' in 'g/km'.")
    }
  })
  
  #
  # ef_europe with SDC (speed of driving cycle)
  #
  
  ef_sdc <- ef_europe(speed = units::set_units(SDC,"km/h"), 
                      veh_type = veh_type, euro = euro, pollutant = pollutant,
                      fuel = fuel, tech = tech, slope = slope, load = load, 
                      fcorr = fcorr)
  
  # adjustment factor
  k <- as.numeric(ef_local)/as.numeric(ef_sdc) 
  
  # ef europe speed
  ef_speed <- ef_europe(speed = speed, 
                        veh_type = veh_type, fuel = fuel, euro = euro,
                        slope = slope, load = load, tech = tech, 
                        pollutant = pollutant)
  ef_speed <- as.matrix(ef_speed)
  #
  # ef_scaled
  #
  ef_scaled <- sapply(seq_along(k),function(i){ef_speed[,i] * k[i]})
  # to units
  ef_scaled <- units::set_units(ef_scaled, 'g/km')
  ef_scaled <- data.table::as.data.table(ef_scaled)
  colnames(ef_scaled) <- colnames(ef_speed)
  
  return(ef_scaled)
}
