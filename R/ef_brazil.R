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
#' @param veh_type character; Vehicle categories by fuel: "PC_G", "PC_FG", "PC_FE", "PC_E", "LCV_G",
#'  "LCV_FG", "LCV_FE", "LCV_E", "LCV_D", "TRUCKS_SL", "TRUCKS_L", "TRUCKS_M", "TRUCKS_SH",
#'  "TRUCKS_H", "BUS_URBAN", "BUS_MICRO", "BUS_COACH", "BUS_ARTIC", "MC_G_150", "MC_G_150_500",
#'  "MC_G_500", "MC_FG_150", "MC_FG_150_500", "MC_FG_500", "MC_FE_150", "MC_FE_150_500",
#'  "MC_FE_500", "CICLOMOTOR", "GNV".
#' @param years numeric; Filter the emission factor to start from a specific base year. 
#' If project is 'constant' values above 2017 and below 1980 will be repeated.
#' @return data.table; Emission factors in units 'g/km' by speed and model_year.
#' @note 
#' 
#' The new convention for vehicles names are translated from CETESB report:
#' 
#' | vehicle       | description                                                    |
#' | ------------- | --------------------------------------------------------------------- |
#' | PC_G          | Passenger Car Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | PC_E	         | Passenger Car Ethanol (hydrous ethanol) |
#' | PC_FG	       | Passenger Car Flex Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | PC_FE	       | Passenger Car Flex Ethanol (hydrous ethanol) |
#' | LCV_G      	 | Light Commercial Vehicle Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | LCV_E      	 | Light Commercial Vehicle Ethanol (hydrous ethanol) |
#' | LCV_FG	       | Light Commercial Vehicle Flex Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | LCV_FE	       | Light Commercial Vehicle Flex Ethanol (hydrous ethanol) |
#' | LCV_D	       | Light Commercial Vehicle Diesel (5perc bio-diesel) |
#' | TRUCKS_SL_D   | Trucks Semi Light Diesel (5perc bio-diesel) |
#' | TRUCKS_L_D	   | Trucks Light Diesel (5perc bio-diesel) |
#' | TRUCKS_M_D	   | Trucks Medium Diesel (5perc bio-diesel) |
#' | TRUCKS_SH_D   | Trucks Semi Heavy Diesel (5perc bio-diesel) |
#' | TRUCKS_H_D	   | Trucks Heavy Diesel (5perc bio-diesel) |
#' | BUS_URBAN_D   | Urban Bus Diesel (5perc bio-diesel) |
#' | BUS_MICRO_D   | Micro Urban Bus Diesel (5perc bio-diesel) |
#' | BUS_COACH_D   | Coach (inter-state) Bus Diesel (5perc bio-diesel) |
#' | BUS_ARTIC_D   | Articulated Urban Bus Diesel (5perc bio-diesel) |
#' | MC_150_G	     | Motorcycle engine less than 150cc Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | MC_150_500_G	 | Motorcycle engine 150-500cc Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | MC_500_G	     | Motorcycle greater than 500cc Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | MC_150_FG	   | Flex Motorcycle engine less than 150cc Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | MC_150_500_FG | Flex Motorcycle engine 150-500cc Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | MC_500_FG	   | Flex Motorcycle greater than 500cc Gasohol (Gasoline + 27perc of anhydrous ethanol) |
#' | MC_150_FE	   | Flex Motorcycle engine less than 150cc Ethanol (hydrous ethanol) |
#' | MC_150_500_FE | Flex Motorcycle engine 150-500cc Ethanol (hydrous ethanol) |
#' | MC_500_FE	   | Flex Motorcycle greater than 500cc Ethanol (hydrous ethanol) |
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
#' 
#' In the previous versions I used the letter 'd' for deteriorated. I removed the letter 'd'
#' internally to not break older code.
#' If by mistake, the user inputs one of veh names from the old convention, they are internally
#' changed to the new convention: "SLT", "LT", "MT", "SHT","HT", "UB", "SUB", "COACH", "ARTIC",
#' "M_G_150", "M_G_150_500", "M_G_500", "M_FG_150", "M_FG_150_500", "M_FG_500", "M_FE_150",
#' "M_FE_150_500","M_FE_500",
#' If pollutant is "SO2", it needs 'sppm.' It is designed when 'veh' has length 1, if it has length
#' 2 or more, it will show a warning
#' Emission factor for vehicles older than the reported by CETESB were filled as the moving average
#' of 2:
#' Range EF from PC and LCV otto: 2018 - 1982. EF for 1981 and older as moving average.
#' Range LCV diesel : 2018 - 2006. EF for 2005 and older as moving average.
#' Range Trucks and Buses: 2018 - 1998. EF for 1997 and older as moving average.
#' Range MC Gasoline: 2018 - 2003. EF for 2002 and older as moving average.
#' Range MC Flex 150-500cc and >500cc: 2018 - 2012. EF for 2011 and older as moving average.
#' Currently, 2020, there are not any system for recovery of fuel vapors in Brazil. 
#' Hence, the FS takes into account the vapour that comes from the fuel tank inside the car and
#' released into the atmosphere when injecting new fuel. There are discussions about incrementing
#' implementing stage I and II and/or ORVR thesedays. The ef FS is calculated by transforming
#' g FC/km into (L/KM)*g/L with g/L 1.14 fgor gasoline and 0.37 for ethanol (CETESB, 2016). The
#' density considered is 0.75425 for gasoline and 0.809 for ethanol (t/m^3).
#' @md
#' @source \url{https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/}
#' @export
ef_brazil <- function(pollutant, veh_type, years){
  # pollutant = c("CO","PM")
  # veh_type = "BUS_URBAN_D" #veh_type
  # years = total_fleet$year
  
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
                      agemax = 1) 
    })
    ef_temp <- do.call(cbind, ef_temp)
    return(ef_temp)
  })

  ef_temp2 <- do.call(cbind, ef_temp1)  %>% units::set_units("g/km")  
  
  # rename colnames
  colnames(ef_temp2) <- paste0(rep(pollutant, each = length(years)), "_", years)
  
  return(ef_temp2)
}
