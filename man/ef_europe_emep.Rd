% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ef_europe_emep.R
\name{ef_europe_emep}
\alias{ef_europe_emep}
\title{Speed-dependent emission factor from the European Environment Agency (EMEP/EEA) model}
\usage{
ef_europe_emep(
  speed,
  veh_type,
  euro,
  pollutant,
  fuel = "D",
  tech = "SCR",
  slope = 0,
  load = 0.5,
  fcorr = 1,
  as_list = TRUE
)
}
\arguments{
\item{speed}{units; Speed in 'km/h'.}

\item{veh_type}{character; Bus type, classified in "Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t",
"Coaches Std <=18 t","Coaches Artic >18 t".}

\item{euro}{character; Euro period of vehicle, classified in "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV".}

\item{pollutant}{character; Pollutant, classified in "FC","CO2","CO","NOx","VOC","PM10","EC","CH4","NH3","N2O". "FC" means Fuel Consumption.}

\item{fuel}{character; Fuel type, classified in "D" (Diesel),"DHD" (Diesel Hybrid ~ Diesel),
"DHE" (Diesel Hybrid ~ Electricity), "CNG" (Compressed Natural Gas), "BD" (Biodiesel).}

\item{tech}{character; After treatment technology, classified in "SCR" (Selective Catalytic Reduction),
"EGR" (Exhaust Gas Recirculation), and "DPF+SCR" (Diesel Particulate Filter + SCR, for Euro VI). Default is "SCR" for "IV" and "V". There are
no available after treatment technology associated with euro standards "Conventional", "I", "II" and "III".}

\item{slope}{numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and
0.06. Negative gradients means downhills and positive uphills. Default is 0.0.}

\item{load}{numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.}

\item{fcorr}{numeric; Correction based on fuel composition. The length must be one per
each euro standards. Default is 1.0.}

\item{as_list}{logical; Returns emission factors as a list, instead of data.table format. Default is TRUE.}
}
\value{
List. emission factors in units 'g/km' (list or a data.table).
}
\description{
Returns a list or data.table of emission factors for buses based on EMEP/EEA air pollutant
emission inventory guidebooks. The function uses four emission factor databases
published by EMEP/EEA, considering the editions of 2019, 2016, 2013 and 2007.
Estimates are expressed in units 'g/km'. See more in @details.
}
\details{
The new convention for vehicles names are translated from the EMEP/EEA report:\tabular{ll}{
   vehicle category \tab description \cr
   Ubus Midi <=15 t \tab Urban Bus Midi size, Gross Vehicle Weight (GVW) <= 15 tons \cr
   Ubus Std 15 - 18 t \tab Urban Bus Standard size, GVW between 15 - 18 tons \cr
   Ubus Artic >18 t \tab Urban Bus Articulated size, GVW >= 18 tons \cr
   Coaches Std <=18 t \tab Coach (inter-state) Standard size, GVW <= 18 tons \cr
   Coaches Artic >18 t \tab Coach (inter-state) Articulated size, GVW > 18 tons \cr
}


When the information of vehicle technology does not match the existing database,
the function display a message mentioning the returned technology.
User can either select an existing data for the combining variables
(\code{euro}, \code{tech}, \code{veh_type} and \code{pollutant}), or accept the assumed change
in vehicle technology.

In order to cover more pollutants, vehicle technologies, and fuel consumption data,
the function uses four emission factor databases published by EMEP/EEA,
considering the editions of 2019, 2016, 2013 and 2007.

The R scripts used to download and pre-process 4 EMEP/EEA editions (2019, 2016, 2013 and 2007)
can be accessed in the 'gtfs2emis' GitHub repository at
<\url{https://github.com/ipeaGIT/gtfs2emis/blob/master/data-raw/ef_europe_emep_db.R}>

EMEP/EEA data and reports can be accessed in the following links:
\itemize{
\item 2019 edition \url{https://www.eea.europa.eu/themes/air/air-pollution-sources-1/emep-eea-air-pollutant-emission-inventory-guidebook},
\item 2016 edition \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2016/},
\item 2013 edition \url{https://www.eea.europa.eu/publications/emep-eea-guidebook-2013/}, and
\item 2007 edition \url{https://www.eea.europa.eu/publications/EMEPCORINAIR5/}.
}
}
\examples{
ef_europe_emep( speed = units::set_units(1:100,"km/h"),
                veh_type = c("Ubus Midi <=15 t","Ubus Std 15 - 18 t","Ubus Artic >18 t"),
                euro = c("IV","V"),
                fuel = "D",
                pollutant = c("CO","PM10","CH4","NOx"),
                as_list = FALSE) 
}
\seealso{
Other Emission factor model: 
\code{\link{ef_brazil_cetesb}()},
\code{\link{ef_scaled_euro}()},
\code{\link{ef_usa_emfac}()},
\code{\link{ef_usa_moves}()},
\code{\link{emi_europe_emep_wear}()}
}
\concept{Emission factor model}
