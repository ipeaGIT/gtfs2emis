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
#' @param fcorr numeric; Correction based on fuel composition. The length must be one per
#' each euro standards. Default is 1.0.
#' @return emission factors in units 'g/km' (a vector or a data.frame).
#' @export
ef_europe <- function(speed, veh_type,  euro,  pollutant, fuel = "Diesel", tech = "SCR", 
                      slope = 0.0, load = 0.5, fcorr = 1){
  # local test
  # speed <- vein::Speed(1:100)
  # veh_type <- "Urban Buses Standard 15 - 18 t"
  # euro <- c("IV","V")
  # pollutant <- c("CO2","NOx")
  # fuel <- "Diesel";tech <- c("SCR","EGR"); slope = 0.0; load = 0.5; k =1; fcorr = 1;i = j = 1

  # euro vector----
  
  euro <- paste0("Euro ", euro)
  temp_ef <- gtfs2emis::europe
  temp_ef$Pollutant %>% unique()
  
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
  if(length(fcorr) == 1){
    fcorr <- rep(fcorr,length(euro))
  }
  # polynomial expression----
  
  eq_num <- function(Alpha,Beta,Gamma,Delta,Epsilon,Zita,Hta,RF,Speed,Function_ID,k,fcorr){
    switch(Function_ID,
           "1" = fcorr*(Alpha*(Beta^Speed))*(Speed^Gamma),
           "2" = fcorr*(Alpha*(Speed^Beta))+(Gamma*(Speed^Delta)),
           "3" = fcorr*(exp((Alpha+(Beta/Speed))+(Gamma*log(Speed)))),
           "4" = fcorr*(Alpha*(Speed^3)+Beta*(Speed^2)+Gamma*Speed+Delta),
           "5" = fcorr*(Alpha*(Beta^Speed)*(Speed^Gamma)),
           "6" = fcorr*(Alpha/(1+(Beta*exp(((-1)*Gamma)*Speed)))),
           "7" = fcorr*(1/(Alpha+(Beta*(Speed^Gamma)))),
           "8" = fcorr*(Alpha-(Beta*exp(((-1)*Gamma)*(Speed^ Delta)))),
           "9" = fcorr*(1/(Alpha+(Beta*Speed))),
           "10" = fcorr*(Alpha*(Speed^3)+(Beta*(Speed^2))+(Gamma*Speed)+Delta),
           "11" = fcorr*(Alpha*Speed^2+Beta*Speed+Gamma+Delta/Speed)/(Epsilon*Speed^2+Zita*Speed+Hta)*(1-RF)*k
    )
  }
  
  # emission factor----
  temp_ef1 <- lapply(seq_along(pollutant),function(j){  # j = 1
    temp_ef2 <- lapply(seq_along(euro),function(i){    # i = 2
      
      # pre-conditions
      #message(paste0(pollutant[j]))
      #message(paste0(euro[i]))
      #message(i)
      if(euro[i] %in%  c("Conventional","Euro I","Euro II","Euro III")){
        tech[i] = NA
        message(paste0("no technology associated with ", euro[i]))
      }
      temp_ef3 <- temp_ef[ Pollutant %in% pollutant[j] &  
                             Fuel %in% fuel[i] & 
                             Segment %in% veh_type[i] & 
                             Technology %in% tech[i] &
                             Euro.Standard %in% euro[i] & 
                             Road.Slope %in% slope[i] & 
                             Load == load[i], ]
      
      # no EF
      if(nrow(temp_ef3) == 0){
        erro_msg <- paste("No available emission factor for the following combination of parameters\n",
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
      
      eq_output <- eq_num(Alpha = temp_ef3$Alpha, Beta = temp_ef3$Beta, 
                          Gamma = temp_ef3$Gamma, Delta = temp_ef3$Delta, 
                          Epsilon = temp_ef3$Epsilon, Zita = temp_ef3$Zita,
                          Hta = temp_ef3$Hta, RF = temp_ef3$RF,
                          Speed = tmpSpeed, Function_ID = temp_ef3$Function_ID, 
                          k = temp_ef3$k, fcorr = fcorr[i])
      return(eq_output)
    })
    
    # do not aggregate emission factors
    temp_ef3 <- do.call(cbind, temp_ef2) %>% 
      units::set_units('g/km') %>% data.table::as.data.table()
    names(temp_ef3) <- paste0(pollutant[j], "_", rep(euro))
    return(temp_ef3)
  })
  
  # return in a data.table like format----
  
  ef_final <- do.call(cbind, temp_ef1)
  return(ef_final[speed, ])
}
