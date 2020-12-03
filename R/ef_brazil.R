#' @title Emission factor for buses in Brazil
#' 
#' @description Returns a vector or data.frame of emission factors for buses based on
#' estimates from the 
#' [Environment Company of Sao Paulo, Brazil (CETESB) 2017](https://cetesb.sp.gov.br/veicular/),
#' and obtained via vein package.
#' Estimates are expressed in units 'g/km'.
#'
#' @param pollutant character; Pollutants: "CO", "HC", "NMHC", "CH4", "NOx", "CO2", "RCHO", "ETOH",
#'  "PM", "N2O", "KML", "FC", "NO2", "NO", "gD/KWH", "gCO2/KWH", "RCHO", "CO_0km", "HC_0km",
#'  "NMHC_0km", "NOx_0km", "NO2_0km", "NO_0km", "RCHO_0km", "ETOH_0km", "FS" (fuel sales) (g/km). 
#'  Evaporative emissions at average temperature ranges: "D_20_35", "S_20_35", "R_20_35", 
#'  "D_10_25", "S_10_25", "R_10_25", "D_0_15", "S_0_15" and "R_0_15" where D means diurnal (g/day),
#'   S hot/warm soak (g/trip) and R hot/warm running losses (g/trip).
#' @param veh_type character; Vehicle categories by fuel:"BUS_URBAN", "BUS_MICRO", "BUS_COACH", 
#' "BUS_ARTIC", "MC_G_150", "MC_G_150_500".
#' @param years numeric; Filter the emission factor to start from a specific base year. 
#' If project is 'constant' values above 2017 and below 1980 will be repeated.
#' @param as_list logical; Returns emission factors as a list, instead of data.table format. Default is TRUE.
#' @return data.table; Emission factors in units 'g/km' by speed and model_year.
#' @note 
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
#' 3) CNG emission factors were expanded to other pollutants by comparison of US.EPA-AP42 emission
#' factor: Section 1.4 Natural Gas Combustion.
#' Range LCV diesel : 2018 - 2006. EF for 2005 and older as moving average.
#' @md
#' @source \url{https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/}
#' @export
#' @examples 
#' ef_brazil(pollutant = c("CO","PM","CO2","CH4","NOx"),
#' veh_type = "BUS_URBAN_D",years = 2015,as_list = TRUE)
ef_brazil <- function(pollutant, veh_type, years, as_list = TRUE){
  
  #
  # init config
  #
  # pollutant = c("CO","PM")
  # veh_type = c("BUS_MICRO_D","BUS_URBAN_D") #veh_type
  # years = c(2001,2005)
  
  # check lengths----
  
  if(length(years) != length(veh_type) && length(veh_type) == 1){
    veh_type <- rep(veh_type, length(years))
  }
  
  # vehicle distribution----
  
  ef_temp1 <- lapply(pollutant, function(p){ # p = pollutant[1]
    ef_temp <- lapply(seq_along(years), function(i){# i = colnames(total_fleet)[1]
      vein::ef_cetesb(p = p,
                      veh = veh_type[i],
                      year = years[i],
                      scale = "tunnel",
                      agemax = 1) 
    })
    ef_temp <- do.call(cbind, ef_temp)
    return(ef_temp)
  })
  
  ef_final <- do.call(cbind, ef_temp1)  %>% units::set_units("g/km")  
  
  # rename colnames
  colnames(ef_final) <- paste0(rep(pollutant, each = length(years)), "_", years)
  
  # return in a data.table/list like format----
  
  if(as_list){
    # local test
    ef_final <- list("pollutant" = rep(pollutant,each = length(veh_type)),
                     "veh_type" = rep(veh_type,length(pollutant)),
                     "years" = rep(years,length(pollutant)),
                     "EF" = ef_final)
  }
  
  return(ef_final)
  
}
