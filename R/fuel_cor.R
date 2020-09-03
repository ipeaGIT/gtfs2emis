#' @title Correction factor equation
#'
#' @description Relations between emissions and fuel properties for diesel heavy-duty vehicles based on EMEP/EEA. Function based on
#' values from the [EMEP/EEA air pollutant emission inventory guidebook 2019](https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook).
#' Estimates are given by the ratio between correction factor of improved fuel by the correction factor of base fuel.    
#' 
#' @param pollutant character; Pollutant classified in "CO", "VOC", "NOx" or "PM".
#' @param euro_stage character; EURO period of vehicle, classified in "PRE", "I", "II",
#' "III", "IV", "V" and "VI".
#' @param improved_fuel numeric; Numeric vector for characteristics of an improved fuel, ordered by:
#' den (Density at 15 degrees C), s (Sulphur content in ppm), pah (Polycyclic aromatics content in \%),
#' cn (Cetane number), t95 (Back end distillation in degrees C). Default input uses
#' c(den = 835, s = 40, pah = 5, cn = 53, t95 = 320).
#' @return numeric; fuel correction factor.
#' @export
fuel_cor <- function(pollutant, euro_stage, improved_fuel = c(den = 835, s = 40, pah = 5, cn = 53, t95 = 320)){
  # init config
  # 
  # euro_stage <- c("PRE", "PRE", "I", "I", "II", "III", "IV", "V", "VI")
  # pollutant <- c("CO","PM","VOC","NOx")
  # improved_fuel = c(den = 835, s = 40, pah = 5, cn = 53, t95 = 320)
  
  
  # relation between fuel properties and emissions
  
  fcorr_list <- list(
    "CO" = function(den, pah, cn, t95, s = 0){2.24407 - 0.0011 * den + 0.00007 * pah - 0.00768 * cn - 0.00087 * t95},
    "VOC" = function(den, pah, cn, t95, s = 0){1.61466 - 0.00123 * den + 0.00133 * pah - 0.00181 * cn - 0.00068 * t95},
    "NOx" = function(den, pah, cn, t95, s = 0){-1.75444 + 0.00906 * den - 0.0163 * pah + 0.00493 * cn + 0.00266 * t95},
    "PM" = function(den, pah, cn, t95, s){(0.06959 + 0.00006 * den + 0.00065 * pah - 0.00001 * cn) * (1-0.0086 * (450 - s)/100)}
  )
  
  #
  # base fuel -----
  #
  
  base_fuel <- data.table::data.table("veh_type"    =   c("PRE", "I", "II", "III", "IV", "V", "VI"),
                                      "base_fuel"    =  c(1996, 1996, 2000, 2005, 2005, 2005, 2005),
                                      "improved_fuel" = c(2005, 2005, 2005, 2005, 2005, 2005, 2005))
  
  #
  # diesel properties --------
  #
  
  diesel_fuel <- data.table::data.table("den" = c(840,840),
                                        "s" = c(400,300),
                                        "pah" = c(9,7),
                                        "cn" = c(51,53),
                                        "t95" = c(350,330),
                                        "year" = c(1996,2000))
  
  input_fuel <- data.table::as.data.table(t(improved_fuel))
  input_fuel[, year := 2005]
  diesel_fuel <- rbind(diesel_fuel,input_fuel)
  
  #
  # diesel fuel specifications ------
  #
  
  temp_year <- base_fuel[data.table::as.data.table(euro_stage),
                         on = c("veh_type" = "euro_stage")]
  temp_year[diesel_fuel,on = c("base_fuel" = "year"),
            paste0(c('den','s','pah','cn','t95'),"_base") := .(i.den,i.s,i.pah,i.cn,i.t95)]
  temp_year[diesel_fuel,on = c("improved_fuel" = "year"),
            paste0(c('den','s','pah','cn','t95'),"_imp") := .(i.den,i.s,i.pah,i.cn,i.t95)]
  
  lapply(seq_along(pollutant),function(i){ # i = 1
    temp_year[,(paste0(pollutant[i],"_base")) := 
                fcorr_list[pollutant[i]][[1]](den_base,pah_base,cn_base,t95_base,s_base)]
    temp_year[,(paste0(pollutant[i],"_imp")) := 
                fcorr_list[pollutant[i]][[1]](den_imp,pah_imp,cn_imp,t95_imp,s_imp)]
    temp_year[,(paste0("fcorr_",pollutant[i])) := 
                get(paste0(pollutant[i],"_imp")) / get(paste0(pollutant[i],"_base")) ]
    return(NULL)
  }) 
  
  fcorr <- temp_year[,.SD,.SDcols = (paste0("fcorr_",pollutant))]
  
  return(fcorr)
}