#' @title Emission factor dependent on speed by EMEP/EEA
#' 
#' @description Returns a list or data.frame of emission factors for buses based on EMEP/EEA.
#' Function is based on values from [EMEP/EEA air pollutant emission inventory guidebook 2019](https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook).
#' Estimates expressed in units 'g/km'.
#' 
#' @param speed units; Speed in 'km/h'.
#' @param veh_type character; Bus type, classified in "Urban Buses Midi <=15 t","Urban Buses Standard 15 - 18 t"
#' "Urban Buses Articulated >18 t",  "Urban Buses Diesel Hybrid", "Urban CNG Buses",  "Urban Biodiesel Buses".
#' @param euro character; Euro period of vehicle, classified in "Conventional", "I", "II",
#' "Euro III", "IV", "V", "VI", and "EEV".
#' @param pollutant character; Pollutant, classified in "FC","CO2","CO","NOx","VOC","PM10","EC","CH4","NH3","N2O" 
#' @param fuel character; Fuel type, classified in "Diesel","Diesel Hybrid ~ Diesel","Diesel Hybrid ~ Electricity",
#' "CNG","Biodiesel"   
#' @param tech character; Technology, classified in "SCR", "EGR", and "DPF+SCR" (for Euro VI). Default is "SCR". There are 
#' no available technology associated with euro standards "Conventional", "I", "II" and "III". 
#' @param slope numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 
#' 0.06. Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param fcorr numeric; Correction based on fuel composition. The length must be one per
#' each euro standards. Default is 1.0.
#' @param as_list logical; Returns emission factors as a list, instead of data.table format. Default is TRUE.
#' @return emission factors in units 'g/km' (list or a data.table).
#' @export
#' @examples 
#' set.seed(1335)
#' ef_europe(speed = units::set_units(rnorm(100,50,5),"km/h"),
#'                 veh_type = c("Urban Buses Standard 15 - 18 t","Urban Buses Articulated >18 t"),
#'                 euro = c("IV","V"),
#'                 pollutant = c("CO2","NOx"),
#'                 fuel = "Diesel" ,
#'                 tech =  c("SCR","EGR"),
#'                 slope = 0.0,
#'                 load = 0.5,
#'                 fcorr = 1,
#'                 as_list = TRUE)
ef_europe <- function(speed, veh_type,  euro,  pollutant, fuel = "Diesel", tech = "SCR", 
                      slope = 0.0, load = 0.5, fcorr = 1,as_list = TRUE){
  #
  # local test
  #
  
  # library(magrittr)
  # #speed = rnorm(n = 100,mean = 50,sd = 5) %>% units::set_units("km/h")
  # speed = units::set_units(rep(1:100,2),"km/h")
  # veh_type <- c("Urban Buses Standard 15 - 18 t","Urban Buses Articulated >18 t")
  # euro <-c("IV","V")
  # pollutant <- c("CO2","NOx","CH4","PM10","CO")
  # fuel <- "Diesel"
  # tech <- c("SCR","EGR")
  # slope = 0.0
  # load = 0.5
  # k =1
  # fcorr = 1
  # i = j = 1
  # 
  # euro vector----
  
  neweuro <- paste0("Euro ", euro)
  temp_ef <- gtfs2emis::europe
  
  # check units and lengths----
  
  if(class(speed) != "units"){
    stop("speed neeeds to has class 'units' in 'km/h'. Please, check package 'units'.")
  }
  if(units(speed)$numerator != "km" | units(speed)$denominator != "h"){
    stop("speed need to has 'units' in 'km/h'.")
  }
  
  # speed----
  
  speed <- round(as.numeric(speed))
  tmpSpeed <- unique(speed)
  tmpSpeed[tmpSpeed < 1] <- 1
  
  # check lengths----
  
  if(length(neweuro) != length(veh_type) && length(veh_type) == 1){
    veh_type <- rep(veh_type,length(neweuro))
  }
  if(length(neweuro) != length(tech) && length(tech) == 1){
    tech <- rep(tech,length(neweuro))
  }
  if(length(neweuro) != length(fuel) && length(fuel) == 1){
    fuel <- rep(fuel,length(neweuro))
  }
  if(length(fcorr) == 1){
    fcorr <- rep(fcorr,length(neweuro))
  }
  if(length(slope) == 1){
    slope <- rep(slope,length(speed))
  }
  if(length(load) == 1){
    load <- rep(load,length(speed))
  }
  if(length(fcorr) == 1){
    fcorr <- rep(fcorr,length(neweuro))
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
  
  temp_ef1 <- lapply(seq_along(pollutant),function(i){  # i = 3
    temp_ef2 <- lapply(seq_along(neweuro),function(j){    # j = 1
      
       #message(paste0("i=",i,"| j=",j))
      #
      # condition for missing technologies
      #
      
      if(neweuro[j] %in%  c("Conventional","Euro I","Euro II","Euro III")){
        tech[j] = "-"
        message(paste0("no technology associated with ", neweuro[j]))
      }
      if(neweuro[j] %in%  c("Euro IV") && pollutant[i] == "CO2"){
        tech[j] = "SCR"
        message(paste0("Only 'SCR' technology associated with ", neweuro[j]," and pollutant ", pollutant[i]))
      }
      
      
      
      #
      # fix load and slope
      #
      
      if(length(unique(slope)) > 1 & length(unique(slope)) > 1){
        temp_ef3 <- lapply(seq_along(neweuro),function(k){ # k = 1
          temp_ef[ Pollutant %in% pollutant[i] &  
                     Fuel %in% fuel[j] & 
                     Segment %in% veh_type[j] & 
                     Technology %in% tech[j] &
                     Euro.Standard %in% neweuro[j] & 
                     Road.Slope %in% slope[k] & 
                     Load == load[k], ]
        }) %>% data.table::rbindlist()
      }else{
        temp_ef3 <- temp_ef[ Pollutant %in% pollutant[i] &  
                               Fuel %in% fuel[j] & 
                               Segment %in% veh_type[j] & 
                               Technology %in% tech[j] &
                               Euro.Standard %in% neweuro[j] & 
                               Road.Slope %in% slope[1] & 
                               Load == load[1], ]
      }
      
      #
      # no EF case
      #
      
      if(nrow(temp_ef3) == 0){
        erro_msg <- paste("No available emission factor for the following combination of parameters\n",
                          "fuel = ",fuel[j],"| veh_type = ",veh_type[j],
                          "\n euro = ",neweuro[j],"| tech = ",tech[j],
                          "| pollutant = ",pollutant[i],"| slope = ",slope[i],
                          "| load = ",load[i],
                          "\n Please check `data(europe)` for available data.")
        stop(erro_msg)
      }
      
      #
      # fix speed
      #
      
      tmpSpeedPol <- tmpSpeed
      tmpSpeedPol[tmpSpeedPol < temp_ef3$`Min.Speed.[km/h]`] <- temp_ef3$`Min.Speed.[km/h]`
      tmpSpeedPol[tmpSpeedPol > temp_ef3$`Max.Speed.[km/h]`] <- temp_ef3$`Max.Speed.[km/h]`
      
      #
      # Ef estimates
      #
      
      eq_output <- eq_num(Alpha = temp_ef3$Alpha, Beta = temp_ef3$Beta, 
                          Gamma = temp_ef3$Gamma, Delta = temp_ef3$Delta, 
                          Epsilon = temp_ef3$Epsilon, Zita = temp_ef3$Zita,
                          Hta = temp_ef3$Hta, RF = temp_ef3$RF,
                          Speed = tmpSpeedPol, Function_ID = temp_ef3$Function_ID, 
                          k = temp_ef3$k, fcorr = fcorr[j])
      
      # expands speed---
      
      data_speed <- data.table::data.table("ef" = eq_output, "tmpSpeed" = tmpSpeed)
      data.table::setkey(data_speed,tmpSpeed)
      eq_output <- data_speed[data.table::data.table(speed)]$ef
      
      return(eq_output)
    })
    
    #
    # do not aggregate EF
    #
    
    temp_ef3 <- do.call(cbind, temp_ef2) %>% 
      units::set_units('g/km') %>% data.table::as.data.table()
    names(temp_ef3) <- paste0(pollutant[i],"_Euro_", rep(euro))
    return(temp_ef3)
  })
  
  # return in a data.table/list like format----
  
  ef_final <- do.call(cbind, temp_ef1)
  
  if(as_list){
    # local test
    ef_final <- list("pollutant" = rep(pollutant,each = length(veh_type)),
                     "veh_type" = rep(veh_type,length(pollutant)),
                     "euro" = rep(neweuro,length(pollutant)),
                     "fuel" = rep(fuel,length(pollutant)),
                     "tech" = rep(tech,length(pollutant)),
                     "slope" = slope,
                     "load" = load,
                     "EF" = ef_final)
  }
  
  return(ef_final)
}
