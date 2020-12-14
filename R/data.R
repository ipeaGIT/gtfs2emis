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
#'   \item{Category}{Buses.}
#'   \item{Fuel}{ Fuel type, classified in "D" (Diesel),"DHD" (Diesel Hybrid ~ Diesel),
#' "DHE" (Diesel Hybrid ~ Electricity), "CNG" (Compressed Natural Gas), "BD" (Biodiesel). }
#'   \item{Segment}{character; Bus type, classified in "Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t",
#' "Coaches Std <=18 t","Coaches Artic >18 t".}
#'   \item{Euro}{character; Euro period of vehicle, classified in "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV".}
#'   \item{Technology}{character; After treatment technology, classified in "SCR" (Selective Catalytic Reduction), 
#' "EGR" (Exhaust Gas Recirculation), and "DPF+SCR" (Diesel Particulate Filter + SCR, for Euro VI). Default is "SCR" for "IV" and "V". There are 
#' no available after treatment technology associated with euro standards "Conventional", "I", "II" and "III". }
#'   \item{Pol}{character; Pollutant, classified in "FC","CO2","CO","NOx","VOC","PM10","EC","CH4","NH3","N2O". "FC" means Fuel Consumption. }
#'   \item{Vmin}{Minimum speed for emission factor estimation, in km/h.}
#'   \item{Vmax}{Maximum speed for emission factor estimation, in km/h.}
#'   \item{Alpha, Beta, Gamma, Delta, Epsilon, Zita, Hta, Thita}{Constant parameters.}
#'   \item{RF}{In \% units.}
#'   \item{k}{Constante factor.}
#' }
#' @source \url{"https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/
#' 
#' https://www.eea.europa.eu/publications/emep-eea-guidebook-2016/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-3-b-i-1/"}
#' 
"europe" # data(europe)
#' EMFAC
#' Running exhaust emissions factors from EMFAC2017 model
#'
#' @format A data.table with 106920 rows and 7 variables:
#' \describe{
#' \item{pol}{Character; Pollutants: Carbon monoxide (CO); Nitrogen oxides (NOx); 
#'   Hydrocarbons as TOG (total organic gases), ROG (reactive organic gases), 
#'   THC (total hydrocarbon), or CH4 (methane); 
#'   Particulate matter as particulate matters 10 microns or less in diameter (PM10), 
#'   and particulate matters 2.5 microns or less in diameter (PM2.5);
#'   Sulfur oxides (SOx); Carbon Dioxide (CO2); 
#'   Nitrous Oxide (N2O) and Methane (CH4).}
#'   \item{calendar_year}{Numeric; Calendar Year between 2010 - 2020}
#'   \item{fuel}{Character; Type of fuel: 'Diesel','Gasoline','Natural Gas'}
#'   \item{model_year}{Model year.}
#'   \item{speed}{Units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50"
#'   "50-55", "55-60", "60-65", "65-70", "70-75", "75-80", "80-85", "85-90", ">90" mph (miles/h)}
#' }
#' @source \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/view}
#' 
"usa" # data(usa)