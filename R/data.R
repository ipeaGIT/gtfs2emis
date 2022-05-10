#' Emission factors from EMEP/EEA
#'
#' Hot emission factors are speed dependent and are expressed in g/km. They differ by fuel, 
#' vehicle class and engine technology. Several functions are 
#' consolidated into a single equation, given by
#'
#' EF = (Alpha x V^2 + Beta x V + Gamma + Delta / V) / (Epsilon x V^2 + Zeta x V + Eta) x (1 - RF)
#'
#' where Alpha, Beta, Gamma, Delta, Epsilon, Zera, Eta are constant parameters that depend on
#' pollutant, speed, technology, and vehicle category.
#' 
#' The emissions factors are derived from the EMEP/EEA air pollutant emission inventory guidebook
#' (formerly called the EMEP CORINAIR emission inventory guidebook). The guide provides guidance on
#' estimating emissions from both anthropogenic and natural emission sources.
#'
#' @format A data table with 6431 rows and 22 variables:
#' \describe{
#' \item{Category}{Buses.}
#' \item{Fuel}{ Fuel type, classified in "D" (Diesel),"DHD" (Diesel Hybrid ~ Diesel),
#' "DHE" (Diesel Hybrid ~ Electricity), "CNG" (Compressed Natural Gas), "BD" (Biodiesel). }
#' \item{Segment}{character; Bus type, classified in "Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t",
#' "Coaches Std <=18 t","Coaches Artic >18 t".}
#' \item{Euro}{character; Euro period of vehicle, classified in "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV".}
#' \item{Technology}{character; After treatment technology, classified in "SCR" (Selective Catalytic Reduction), 
#' "EGR" (Exhaust Gas Recirculation), and "DPF+SCR" (Diesel Particulate Filter + SCR, for Euro VI). Default is "SCR" for "IV" and "V". There are 
#' no available after treatment technology associated with euro standards "Conventional", "I", "II" and "III". }
#' \item{Pol}{character; Pollutant, classified in "FC","CO2","CO","NOx","VOC","PM10","EC","CH4","NH3","N2O". "FC" means Fuel Consumption. }
#' \item{Vmin}{Minimum speed for emission factor estimation, in km/h.}
#' \item{Vmax}{Maximum speed for emission factor estimation, in km/h.}
#' \item{Alpha, Beta, Gamma, Delta, Epsilon, Zita, Hta, Thita}{Constant parameters.}
#' \item{RF}{Reduction Factor; In percentage (%) units.}
#' \item{k}{Constante factor.}
#' }
#' @source \url{"https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/
#' 
#' https://www.eea.europa.eu/publications/emep-eea-guidebook-2016/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-3-b-i-1/"}
#' 
"ef_europe_emep_db" # data(ef_europe_emep_db)



#' EMFAC
#' Running exhaust emissions factors from EMFAC2017 model
#'
#' @format A data.table with 106920 rows and 7 variables:
#' \describe{
#' \item{pol}{Character; Pollutants: CH4(Methane), CO(Carbon Monoxide), 
#' CO2(Carbon Dioxide), N2O(Nitrous Oxide), NOx(Oxides of Nitrogen),
#'  PM10(Primary Exhaust PM10 - Total), PM25(Primary Exhaust PM2.5 - Total), SOX(Oxides of Sulfur),
#'  TOG(Total Organic Gases), ROG (Reactive Organic Gases)}
#' \item{reference_year}{Numeric; Calendar Year between 2010 - 2020}
#' \item{fuel}{character; Type of fuel: 'D' (Diesel),'G' (Gasoline),'CNG' (Compressed Natural Gas).}
#' \item{model_year}{Model year.}
#' \item{speed}{Units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50"
#'   "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85-90", ">90" mph (miles/h)}
#' }
#' @source \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/view}
#' 
"ef_usa_emfac_db" # data(ef_usa_emfac_db)

#' MOVES
#' Running exhaust emissions factors from United States (MOVES3 model)
#' 
#' Data.frame of emission factors for buses based on 
#' values from the [MOVES3 Model](https://www.epa.gov/moves).
#' Estimates expressed in units 'g/km'.
#'
#' @format  A data.table:
#' \describe{
#' \item{pollutant}{character; Pollutants: CH4(Methane), CO(Carbon Monoxide), 
#' CO2(Carbon Dioxide), EC(Energy Consumption), HONO(Nitrous Acid), N2O(Nitrous Oxide), NH3(Ammonia ), 
#' NH4(Ammonium), NO(Nitrogen Oxide), NO2(Nitrogen Dioxide), NO3(Nitrate), NOx(Oxides of Nitrogen),
#'  PM10(Primary Exhaust PM10 - Total), PM25(Primary Exhaust PM2.5 - Total), SO2(Sulfur Dioxide), 
#'  THC(Total Gaseous Hydrocarbons ), TOG(Total Organic Gases) and VOC (Volatile Organic Compounds)}
#' \item{fuel}{character; Type of fuel: 'D' (Diesel),'G' (Gasoline),'CNG' (Compressed Natural Gas).}
#' \item{reference_year}{Numeric; Calendar Year between 2015 - 2022. Year in which the emissions
#' inventory is estimated, in order to consider the effect of degradation.}
#' \item{model_year}{numeric; Model year of vehicle.}
#' \item{speed}{units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as " - 2.5", "2.5 - 7.5", "7.5 - 12.5", "12.5 - 17.5", "17.5 - 22.5", "22.5 - 27.5",
#'  "27.5 - 32.5","32.5 - 37.5","37.5 - 42.5","42.5 - 47.5","47.5 - 52.5",
#'  "52.5 - 57.5", "57.5 - 62.5", "62.5 - 67.5", "67.5 - 72.5", ">72.5" mph (miles/h).}
#'  }
#' @source \url{https://www.epa.gov/moves}
"ef_usa_moves_db"  # data(ef_usa_moves_db)


#' CETESB
#' Emission factor for buses in Brazil
#' 
#' Emission factors for buses based on
#' estimates from the 
#' [Environment Company of Sao Paulo, Brazil (CETESB) 2017](https://cetesb.sp.gov.br/veicular/),
#' and obtained via vein package.
#' Estimates are expressed in units 'g/km'.
#'
#' @format  A data.table:
#' \describe{
#' \item{pollutant}{ character; Pollutants: "CH4", "CO2", "PM10", "N2O", "KML", 
#' "FC" (Fuel Consumption), "gD/KWH", "gCO2/KWH", "CO", "HC" (Total Hydrocarbon), 
#' "NMHC" (Non-Methane Hydrocarbon), "NOx", "NO2", "NO", "RCHO", "ETOH",
#'  "FS"(Fuel Sales) and "NH3"}
#' \item{veh_type}{character; Vehicle categories by fuel:"BUS_URBAN_D", "BUS_MICRO_D", 
#' "BUS_COACH_D" and "BUS_ARTIC_D".}
#' \item{model_year}{numeric; Filter the emission factor to start from a specific 
#' base year. }
#' \item{as_list}{logical; Returns emission factors as a list, instead of data.table
#'  format.}
#' }
#'@source \url{https://cetesb.sp.gov.br/veicular/}
"ef_brazil_cetesb_db" # data(ef_brazil_cetesb_db)