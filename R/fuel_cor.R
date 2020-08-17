#' @title Correction factor equation
#'
#' @description Relations between emissions and fuel properties for diesel heavy-duty vehicles based on EMEP/EEA. Function based on
#' values from the [EMEP/EEA air pollutant emission inventory guidebook 2019](https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook).
#' Estimates are dimensionless.
#' 
#' @param pollutant character; Pollutant classified in "CO", "VOC", "NOx" or "PM".
#' @param den numeric; Density at 15 °C (units kg/m3).
#' @param s numeric; Sulphur content in ppm.
#' @param pah numeric; Polycyclic aromatics content in \%.
#' @param cn numeric; Cetane number.
#' @param t95 numeric; Back end distillation in degrees C.
#' @return numeric; fuel correction factor.
#' @export
fuel_cor <- function(pollutant,den,s,pah,cn,t95){
  
  # check inputs
  if(pollutant %nin% c("CO","VOC","NOx","PM")){
    stop("Fuel correction factor is only available for 'CO', 'VOC', 'NOx' and 'PM'.")
  }
  if(pollutant == "CO"){
    Fcorr = 2.24407 - 0.0011 * den + 0.00007 * pah - 0.00768 * cn - 0.00087 * t95
  }
  if(pollutant == "VOC"){
    Fcorr = 1.61466 - 0.00123 * den + 0.00133 * pah - 0.00181 * cn - 0.00068 * t95
  }
  if(pollutant == "NOx"){
    Fcorr = -1.75444 + 0.00906 * den - 0.0163 * pah + 0.00493 * cn + 0.00266 * t95
  }
  if(pollutant == "PM"){
    Fcorr = (0.06959 + 0.00006 * den + 0.00065 * pah - 0.00001 * cn) * 
      (1-0.0086 * (450 - s)/100)
  }
  
  return(Fcorr)
}