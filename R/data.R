#' Emission factors from EMEP/EEA
#'
#' Hot emission factors are speed dependent and are expressed in g/km. They differ by fuel, vehicle class
#' and engine technology. In previous versions of this chapter a number of functions were provided to 
#' calculate hot emission factors for the different vehicle categories. All these functions are now 
#' consolidated into a single equation, given by
#'
#' EF = (Alpha x V^2 + Beta x V + Gamma + Delta / V) / (Epsilon x V^2 + Zeta x V + Eta) x (1 - RF)
#'
#' where Alpha, Beta, Gamma, Delta, Epsilon, Zera, Eta are constant parameters that depends on
#' pollutant, speed, technology, vehicle category.
#' 
#' The emissions factors are derived from the EMEP/EEA air pollutant emission inventory guidebook (formerly 
#' called the EMEP CORINAIR emission inventory guidebook). The guide provides guidance on estimating emissions 
#' from both anthropogenic and natural emission sources.
#'
#' @format A data table with 6431 rows and 22 variables:
#' \describe{
#'   \item{Category}{Buses.}
#'   \item{Fuel}{Diesel, Compressed Natural Gas (CNG) or Biodiesel.}
#'   \item{Segment}{Urban Buses Midi <=15 t, Urban Buses Standard 15 - 18 t, Urban Buses Articulated >18 t, Coaches Standard <=18 t,
#'   Coaches Articulated >18 t,Urban CNG Buses,Urban Biodiesel Buses.}
#'   \item{Euro.Standard}{Conventional, Euro I,Euro II,Euro III,Euro IV,Euro V,Euro VI,EEV.}
#'   \item{Technology}{NA,SCR,EGR,DPF+SCR.}
#'   \item{Pollutant}{CO,NOx,VOC,PM,FC,CH4,NH3,N2O.}
#'   \item{Mode}{NA,Urban Peak,Urban Off Peak,Rural,Highway.}
#'   \item{Road.Slope}{-0.06,-0.04,-0.02, 0.00,0.02, 0.04,0.06,NA.}   
#'   \item{Min.Speed}{Minimum speed for emission factor estimation, in km/h.}
#'   \item{Max.Speed}{Maximum speed for emission factor estimation, in km/h.}
#'   \item{Alpha, Beta, Gamma, Delta, Epsilon, Zita, Hta, Thita}{Constant parameters.}
#'   \item{Reduction.Factor}{in \% units.}
#'   \item{Bio.Reduction.Factor}{in \% units.}
#' }
#' @source \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/view}
#' 
"europe" # data(europe)
#' EMFAC
#' Running exhaust emissions factors from EMFAC2017 model
#'
#' @format A data table with 106920 rows and 7 variables:
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
#'  such as "5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45","45-50"
#'   "50-55","55-60","60-65","65-70","70-75","75-80","80-85","85-90".">90" mph (miles/h)}
#' }
#' @source \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/view}
#' 
"usa" # data(usa)