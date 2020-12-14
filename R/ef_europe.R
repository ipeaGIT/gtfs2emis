#' @title Emission factor dependent on speed by EMEP/EEA
#' 
#' @description Returns a list or data.frame of emission factors for buses based on EMEP/EEA.
#' Function is based on values from [EMEP/EEA air pollutant emission inventory guidebook 2019](https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook).
#' And [2016 edition](https://www.eea.europa.eu/publications/emep-eea-guidebook-2016/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-3-b-i-1/)
#' Estimates expressed in units 'g/km'.
#' 
#' @param speed units; Speed in 'km/h'.
#' @param veh_type character; Bus type, classified in "Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t",
#' "Coaches Std <=18 t","Coaches Artic >18 t".
#' @param euro character; Euro period of vehicle, classified in "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV".
#' @param pollutant character; Pollutant, classified in "FC","CO2","CO","NOx","VOC","PM10","EC","CH4","NH3","N2O". "FC" means Fuel Consumption. 
#' @param fuel character; Fuel type, classified in "D" (Diesel),"DHD" (Diesel Hybrid ~ Diesel),
#' "DHE" (Diesel Hybrid ~ Electricity), "CNG" (Compressed Natural Gas), "BD" (Biodiesel).   
#' @param tech character; After treatment technology, classified in "SCR" (Selective Catalytic Reduction), 
#' "EGR" (Exhaust Gas Recirculation), and "DPF+SCR" (Diesel Particulate Filter + SCR, for Euro VI). Default is "SCR" for "IV" and "V". There are 
#' no available after treatment technology associated with euro standards "Conventional", "I", "II" and "III". 
#' @param slope numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 
#' 0.06. Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param fcorr numeric; Correction based on fuel composition. The length must be one per
#' each euro standards. Default is 1.0.
#' @param as_list logical; Returns emission factors as a list, instead of data.table format. Default is TRUE.
#' @return emission factors in units 'g/km' (list or a data.table).
#' @export
#' @examples
#' ef_europe( speed = units::set_units(1:100,"km/h"), veh_type = c("Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t"),
#' euro = c("III","IV","V"),fuel = "D",pollutant = c("CO","PM10","CO2","CH4","NOx"),as_list = FALSE)
#' 
ef_europe <- function(speed, veh_type,  euro,  pollutant, fuel = "D", tech = "SCR", 
                      slope = 0.0, load = 0.5, fcorr = 1,as_list = TRUE){
  #
  # local test
  #
  
  # library(magrittr)
  # #speed = rnorm(n = 100,mean = 50,sd = 5) %>% units::set_units("km/h")
  # speed = units::set_units(rep(1:100,2),"km/h")
  # veh_type <- c("Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t")
  # euro <-c("IV","IV","V")
  # pollutant <- c("CO2","NOx","CH4","PM10","CO")
  # fuel <- "D"
  # tech <- c("EGR","EGR","SCR")
  # slope = 0.0
  # load = 0.5
  # k =1
  # fcorr = 1
  # i = j = 1
  # data(europe)
  # temp_ef <- europe
  #
  # euro vector----
  
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
  
  if(length(euro) != length(veh_type) && length(veh_type) == 1){
    veh_type <- rep(veh_type,length(euro))
  }
  if(length(euro) != length(tech) && length(tech) == 1){
    tech <- rep(tech,length(euro))
  }
  if(length(euro) != length(fuel) && length(fuel) == 1){
    fuel <- rep(fuel,length(euro))
  }
  if(length(fcorr) == 1){
    fcorr <- rep(fcorr,length(euro))
  }
  if(length(slope) == 1){
    slope <- rep(slope,length(speed))
  }
  if(length(load) == 1){
    load <- rep(load,length(speed))
  }
  if(length(fcorr) == 1){
    fcorr <- rep(fcorr,length(euro))
  }
  
  # polynomial expression----
  
  eq_num <- function(Alpha,Beta,Gamma,Delta,Epsilon,Zita,Hta,RF,Speed,Function_ID,k,fcorr){
    switch(Function_ID,
           "1" = fcorr*(Alpha*Speed^2+Beta*Speed+Gamma+Delta/Speed)/(Epsilon*Speed^2+Zita*Speed+Hta)*(1-RF)*k
    )
  }
  
  # emission factor----
  
  temp_ef1 <- lapply(seq_along(pollutant),function(i){  # i = 3
    temp_ef2 <- lapply(seq_along(euro),function(j){    # j = 1
      
      #message(paste0("i=",i,"; j=",j))
      #
      # condition for missing technologies
      #
      
      if(euro[j] %in%  c("Conventional","I","II","III")){
        tech[j] = "-"
        message(paste0("no technology associated with ", euro[j]))
      }
      if(euro[j] %in%  c("IV") && pollutant[i] == "CO2"){
        tech[j] = "SCR"
        message(paste0("Only 'SCR' technology associated with ", euro[j]," and pollutant ", pollutant[i]))
      }
      
      
      
      #
      # fix load and slope
      #
      
      if(length(unique(slope)) > 1 & length(unique(slope)) > 1){
        temp_ef3 <- lapply(seq_along(euro),function(k){ # k = 1
          temp_ef[ Pol %in% pollutant[i] &  
                     Fuel %in% fuel[j] & 
                     Segment %in% veh_type[j] & 
                     Technology %in% tech[j] &
                     Euro %in% euro[j] & 
                     Slope %in% slope[k] & 
                     Load == load[k], ]
        }) %>% data.table::rbindlist()
      }else{
        temp_ef3 <- temp_ef[ Pol %in% pollutant[i] &  
                               Fuel %in% fuel[j] & 
                               Segment %in% veh_type[j] & 
                               Technology %in% tech[j] &
                               Euro %in% euro[j] & 
                               Slope %in% slope[1] & 
                               Load == load[1], ]
      }
      
      #
      # no EF case
      #
      
      if(nrow(temp_ef3) == 0){
        erro_msg <- paste("No available emission factor for the following combination of parameters\n",
                          "fuel = ",fuel[j],"| veh_type = ",veh_type[j],
                          "\n euro = ",euro[j],"| tech = ",tech[j],
                          "| pollutant = ",pollutant[i],"| slope = ",slope[i],
                          "| load = ",load[i],
                          "\n Please check `data(europe)` for available data.")
        stop(erro_msg)
      }
      
      #
      # fix speed
      #
      
      tmpSpeedPol <- tmpSpeed
      tmpSpeedPol[tmpSpeedPol < temp_ef3$Vmin] <- temp_ef3$Vmin
      tmpSpeedPol[tmpSpeedPol > temp_ef3$Vmax] <- temp_ef3$Vmax
      
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
                     "euro" = rep(euro,length(pollutant)),
                     "fuel" = rep(fuel,length(pollutant)),
                     "tech" = rep(tech,length(pollutant)),
                     "slope" = slope,
                     "load" = load,
                     "EF" = ef_final)
  }
  
  return(ef_final)
}
