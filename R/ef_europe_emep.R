#' @title 
#' Speed-dependent emission factor from the European Environment Agency (EMEP/EEA) model 
#' 
#' @description 
#' Returns a list or data.table of emission factors for buses based on EMEP/EEA air pollutant
#'  emission inventory guidebooks. Estimates are expressed in units 'g/km'.
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
#' 
#' @return List. emission factors in units 'g/km' (list or a data.table).
#' @details 
#' 
#' The new convention for vehicles names are translated from the EMEP/EEA report:
#' 
#' | vehicle category    | description                                              |
#' | ------------------  | -------------------------------------------------------- |
#' | Ubus Midi <=15 t    | Urban Bus Midi size, Gross Vehicle Weight (GVW) <= 15 tons |
#' | Ubus Std 15 - 18 t  | Urban Bus Standard size, GVW between 15 - 18 tons |
#' | Ubus Artic >18 t    | Urban Bus Articulated size, GVW >= 18 tons |
#' | Coaches Std <=18 t  | Coach (inter-state) Standard size, GVW <= 18 tons |
#' | Coaches Artic >18 t | Coach (inter-state) Articulated size, GVW > 18 tons |
#' 
#' The R scripts used to download and pre-process all 4 EMEP/EEA editions (2019, 2016, 2013 and 2007)
#' can be accessed in the gtfs2emis GitHub repository at
#' <<https://github.com/ipeaGIT/gtfs2emis/blob/master/data-raw/ef_europe_emep_db.R>>  
#' 
#' EMEP/EEA data and reports can be accessed in the following links:
#'  - 2019 edition \url{https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook},
#'  - 2016 edition \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2016/},
#'  - 2013 edition \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2013/}, and
#'  - 2007 edition \url{https://www.eea.europa.eu/publications/EMEPCORINAIR5/}.
#' 
#' @family Emission factor model
#' 
#' @export
#' 
#' @examples if (interactive()) {
#' ef_europe_emep( speed = units::set_units(1:100,"km/h"),
#'            veh_type = c("Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t"),
#'            euro = c("III","IV","V"),
#'            fuel = "D",
#'            pollutant = c("CO","PM10","CO2","CH4","NOx"),
#'            as_list = FALSE) 
#'}
ef_europe_emep <- function(speed, veh_type, euro,  pollutant, fuel = "D", tech = "SCR", 
                           slope = 0.0, load = 0.5, fcorr = 1, as_list = TRUE){
  #
  # local test
  #
  # 
  # library(magrittr)
  # #speed = rnorm(n = 100,mean = 50,sd = 5) %>% units::set_units("km/h")
  # speed = units::set_units(rep(1:100,2),"km/h")
  # veh_type <- c("Ubus Std 15 - 18 t")
  # euro <-c("I","II","III","IV")
  # pollutant <- c("CO2")
  # fuel <- "D"
  # tech <- c("-")
  # slope = 0.0
  # load = 0.5
  # k =1
  # fcorr = 1
  # i = j = 1
  
  # euro vector----
  utils::data('ef_europe_emep_db') 
  temp_ef <- ef_europe_emep_db
  
  
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
  if(length(fcorr) == 1)  fcorr <- rep(fcorr,length(euro))
  if(length(slope) == 1)  slope <- rep(slope,length(speed))
  if(length(load)  == 1)  load  <- rep(load,length(speed))
  if(length(fcorr) == 1)  fcorr <- rep(fcorr,length(euro))
  
  if(length(slope) > 1 & (length(slope) != length(speed)) ){
    stop("'slope' and 'speed' arguments need to have the same length.")
  }
  if(length(load) > 1 & (length(load) != length(speed)) ){
    stop("'load' and 'speed' arguments need to have the same length.")
  }
  
  # polynomial expression----
  
  eq_num <- function(Alpha,Beta,Gamma,Delta,Epsilon,Zita,Hta,RF,Speed,Function_ID,k,fcorr){
    switch(Function_ID,
           "1" = (Alpha * (Speed ^ 2) + Beta * Speed + Gamma + Delta / Speed)/(Epsilon * (Speed ^ 2) + Zita * Speed + Hta) * (1 - RF) * k * fcorr
           ,"2019_1"  = (Alpha * (Speed ^ 2) + Beta * Speed + Gamma + Delta / Speed)/(Epsilon * (Speed ^ 2) + Zita * Speed + Hta) * (1-RF) * k * fcorr
           ,"2013_3"  = (((Alpha * (Beta ^ Speed)) * (Speed ^ Gamma))) * k * fcorr
           ,"2013_12" = ((Alpha / (1 + (Beta * exp(((-1) * Gamma) * Speed))))) * k * fcorr
           ,"2013_9"  = ((1 / (Alpha + (Beta * (Speed ^ Gamma))))) * k * fcorr
           ,"2013_11" = ((Alpha - (Beta * exp(((-1) * Gamma) * (Speed ^ Delta))))) * k * fcorr
           ,"2013_4"  = (((Alpha * (Speed ^ Beta)) + (Gamma * (Speed ^ Delta)))) * k * fcorr
           ,"2013_16" = (exp((Alpha + (Beta / Speed)) + (Gamma * log(Speed)))) * k * fcorr
           ,"2013_10" = ((1 / (Alpha + (Beta * Speed)))) * k * fcorr
           ,"2013_1"  = (((((Alpha * (Speed ^ 3)) + (Beta * (Speed ^ 2))) + (Gamma * Speed)) + Delta)) * k * fcorr
           ,"2007_1"  = ((Epsilon + (Alpha * exp(((-1) * Beta) * Speed))) + (Gamma * exp(((-1) * Delta) * Speed))) * k * fcorr
           ,"2007_2"  = ((Alpha * (Beta ^ Speed)) * (Speed ^ Gamma)) * k * fcorr
           ,"2007_3"  = ((Alpha * (Speed^Beta)) + (Gamma * (Speed ^ Delta))) * k * fcorr
           ,"2007_4"  = (Alpha + (Beta/(1 + exp((((-1) * Gamma)+(Delta * log(Speed)))+(Epsilon * Speed))))) * k * fcorr
           ,"2007_5"  = ((Alpha + (Beta * Speed)) + (((Gamma-Beta)*(1-exp(((-1) * Delta) * Speed)))/Delta)) * k * fcorr
           ,"2007_6"  = (Gamma + (Alpha * exp(Beta * Speed))) * k * fcorr
           ,"2007_7"  = ((((Alpha * (Speed ^ 3))+(Beta * (Speed^2)))+(Gamma * Speed)) + Delta) * k * fcorr
           ,"2007_9"  = (1/(((Gamma * (Speed ^ 2))+(Beta * Speed)) + Alpha)) * k * fcorr
           ,"2007_10" = (Alpha/(1+(Beta * exp(((-1) * Gamma) * Speed)))) * k * fcorr
           ,"2007_11" = exp((Alpha + (Beta / Speed))+(Gamma * log(Speed))) * k * fcorr
           ,"2007_8"  =  (Gamma + (Alpha * exp(((-1) * Beta) * Speed))) * k * fcorr
    ) }
  
  # emission factor----
  
  temp_ef1 <- lapply(seq_along(pollutant),function(i){  # i = 3
    temp_ef2 <- lapply(seq_along(euro),function(j){    # j = 1
      
      #message(sprintf("i=%s, j=%s",i,j))
      # condition for missing technologies
      
      if(euro[j] %in%  c("Conventional","I","II","III")){
        tech[j] = "-"
        message(sprintf("no technology associated with %s", euro[j]))
      }
      if(euro[j] %in%  c("IV") && (pollutant[i] == "CO2" | pollutant[i] == "FC") ){
        tech[j] = "-"
        message(sprintf("no technology associated with %s and pollutant %s", euro[j],pollutant[i]))
      }
      if((euro[j] %in%  c("V","VI")) && (pollutant[i] %in% c("CO2","FC"))){
        tech[j] = "SCR"
        message(sprintf("Only 'SCR' technology associated with Euro %s and pollutant %s", euro[j],pollutant[i]))
      }
      if((euro[j] %in%  c("VI")) && !(pollutant[i] %in% c("CO2","FC"))){
        tech[j] = "DPF+SCR"
        message(sprintf("Only 'DPF+SCR' technology associated with Euro %s and pollutant %s", euro[j], pollutant[i]))
      }
      
      # fix load and slope
      
      if(length(unique(slope)) > 1 & length(unique(slope)) > 1){
        
        temp_ef3 <- lapply(seq_along(euro),function(k){ # k = 1
          temp_ef[ Pol %in% pollutant[i] &  
                     Fuel %in% fuel[j] & 
                     Segment %in% veh_type[j] & 
                     Technology %in% tech[j] &
                     Euro %in% euro[j] & 
                     Slope %in% slope[k] & 
                     Load == load[k], ]
          
        }) 
        temp_ef3 <- data.table::rbindlist(temp_ef3)
      }else{
        temp_ef3 <- temp_ef[ Pol %in% pollutant[i] &  
                               Fuel %in% fuel[j] & 
                               Segment %in% veh_type[j] & 
                               Technology %in% tech[j] &
                               Euro %in% euro[j] & 
                               Slope == slope[1] & 
                               Load == load[1], ]
      }
      
      #
      # no EF case
      #
      
      if(nrow(temp_ef3) == 0){
        erro_msg <- paste0("No available emission factor for the following combination of parameters:\n\n",
                           "ef_europe_emep_db[Pol %in% '",pollutant[i],
                           "' &\n Fuel %in% '",fuel[j],
                           "' &\n Segment %in% '",veh_type[j],
                           "' &\n Technology %in% '",tech[j],
                           "' &\n Euro %in% '",euro[j],
                           "' &\n Slope == ",slope[1],
                           " &\n Load == ",load[1],", ]",
                           "\n\n Please check `data(ef_europe_emep_db)` for available data.")
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
      if(length(unique(slope)) > 1 & length(unique(slope)) > 1){
        
        eq_output <- lapply(seq_along(euro),function(k){ # k = 1
          tmp_eq <- eq_num(Alpha = temp_ef3$Alpha[k]
                              , Beta = temp_ef3$Beta[k]
                              , Gamma = temp_ef3$Gamma[k]
                              , Delta = temp_ef3$Delta[k]
                              , Epsilon = temp_ef3$Epsilon[k]
                              , Zita = temp_ef3$Zita[k]
                              , Hta = temp_ef3$Hta[k]
                              , RF = temp_ef3$RF[k]
                              , Speed = tmpSpeedPol[k]
                              , Function_ID = temp_ef3$Function_ID[k]
                              , k = temp_ef3$k[k]
                              , fcorr = fcorr[j])
        }) 
        eq_output <- data.table::rbindlist(eq_output)
        
      }else{
        
        eq_output <- eq_num(Alpha = temp_ef3$Alpha
                            , Beta = temp_ef3$Beta
                            , Gamma = temp_ef3$Gamma
                            , Delta = temp_ef3$Delta
                            , Epsilon = temp_ef3$Epsilon
                            , Zita = temp_ef3$Zita
                            , Hta = temp_ef3$Hta
                            , RF = temp_ef3$RF
                            , Speed = tmpSpeedPol
                            , Function_ID = temp_ef3$Function_ID
                            , k = temp_ef3$k
                            , fcorr = fcorr[j])
      }
      # expands speed---
      
      data_speed <- data.table::data.table("ef" = eq_output, "tmpSpeed" = tmpSpeed)
      data.table::setkey(data_speed,tmpSpeed)
      eq_output <- data_speed[data.table::data.table(speed)]$ef
      
      return(eq_output)
    })
    
    #
    # do not aggregate EF
    #
    
    temp_ef3 <- do.call(cbind, temp_ef2) 
    temp_ef3 <- units::set_units(temp_ef3,'g/km') 
    temp_ef3 <- data.table::as.data.table(temp_ef3)
    
    names(temp_ef3) <- sprintf("%s_Euro_%s",pollutant[i],rep(euro))
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
