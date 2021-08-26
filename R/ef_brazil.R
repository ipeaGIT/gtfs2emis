#' @title Emission factor for buses in Brazil
#' 
#' @description Returns a vector or data.frame of emission factors for buses based on
#' estimates from the 
#' [Environment Company of Sao Paulo, Brazil (CETESB) 2017](https://cetesb.sp.gov.br/veicular/),
#' and obtained via vein package.
#' Estimates are expressed in units 'g/km'.
#'
#' @param pollutant character; Pollutants: "CH4", "CO2", "PM10", "N2O", "KML", 
#' "FC" (Fuel Consumption), "gD/KWH", "gCO2/KWH", "CO", "HC" (Total Hydrocarbon), 
#' "NMHC" (Non-Methane Hydrocarbon), "NOx", "NO2", "NO", "RCHO", "ETOH",
#'  "FS"(Fuel Sales) and "NH3"
#' @param veh_type character; Vehicle categories by fuel:"BUS_URBAN_D", "BUS_MICRO_D", 
#' "BUS_COACH_D" and "BUS_ARTIC_D".
#' @param model_year numeric; Filter the emission factor to start from a specific base year. 
#' @param as_list logical; Returns emission factors as a list, instead of data.table format. Default is TRUE.
#' 
#' @return data.table; Emission factors in units 'g/km' by speed and model_year.
#' 
#' @details 
#' 
#' The new convention for vehicles names are translated from CETESB report:
#' 
#' | vehicle       | description                                                    |
#' | ------------- | --------------------------------------------------------------------- |
#' | BUS_URBAN_D   | Urban Bus Diesel (5perc bio-diesel) |
#' | BUS_MICRO_D   | Micro Urban Bus Diesel (5perc bio-diesel) |
#' | BUS_COACH_D   | Coach (inter-state) Bus Diesel (5perc bio-diesel) |
#' | BUS_ARTIC_D   | Articulated Urban Bus Diesel (5perc bio-diesel) |
#' 
#' The percentage varies of biofuels varies by law.
#' 
#' These emission factors are not exactly the same as the report of CETESB.
#' 
#' 1) In this emission factors, there is also NO and NO2 based on split by published in the EMEP/EEA
#' air pollutant emission inventory guidebook.
#' 2) Also, the emission factors were extended till 50 years of use, repeating the oldest value.
#' @md
#' @source \url{https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/}
#' @export
#' @examples 
#' ef_brazil(pollutant = c("CO","PM10","CO2","CH4","NOx"),veh_type = "BUS_URBAN_D",
#' model_year = 2015,as_list = TRUE)
ef_brazil <- function(pollutant, veh_type, model_year, as_list = TRUE){
  
  #
  # init config
  #
   # pollutant = c("CO","PM10","CO2","CH4","NOx")
   # veh_type = c("BUS_MICRO_D") #veh_type
   # model_year = c(2001)
  
  # check lengths----
  
  if(length(model_year) != length(veh_type) && length(veh_type) == 1){
    veh_type <- rep(veh_type, length(model_year))
  }
  if(length(model_year) != length(veh_type) && length(model_year) == 1){
    model_year <- rep(model_year, length(veh_type))
  }
  
  # vehicle distribution----
  
  ef_temp1 <- lapply(pollutant, function(p){ # p = pollutant[1]
    ef_temp <- lapply(seq_along(model_year), function(i){# i = 1
      ef_brazil_db[pollutant == p &
                  model_year == model_year[i],
                .SD,.SDcols = veh_type[i]][[1]]
    })
    ef_temp <- do.call(cbind, ef_temp)
    return(ef_temp)
  })
  
  ef_final <- do.call(cbind, ef_temp1)  %>% units::set_units("g/km")  
  
  # rename colnames
  colnames(ef_final) <- paste0(rep(pollutant, each = length(model_year)), "_", model_year)
  
  # return in a data.table/list like format----
  
  if(as_list){
    # local test
    ef_final <- list("pollutant" = rep(pollutant,each = length(veh_type)),
                     "veh_type" = rep(veh_type,length(pollutant)),
                     "years" = rep(model_year,length(pollutant)),
                     "EF" = ef_final)
  }
  
  return(ef_final)
  
}
