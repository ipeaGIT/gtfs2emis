rm(list=ls())
library(data.table)
library(magrittr)
`%nin%` = Negate(`%in%`)
emfac <- data.table::fread("../../Dropbox/IPEA/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")
#emfac <- data.table::fread("../../Dropbox/Literature/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20201019132524.csv")
emfac <- emfac[`Vehicle Category` %in% 'UBUS',]
emfac[Fuel %in% "Natural Gas", Fuel := "CNG"]
emfac[,VMT := VMT %>% 
        units::set_units('mile') %>% 
        units::set_units("km")]
# ------------------------------------------------------------
# based on EMFAC2017 "Volume III – Technical Documentation V1.0.2 July 20, 2018"
# ------------------------------------------------------------
# Table 4.3-55 Speed Correction Factors for Diesel Buses
# ------------------------------------------------------------
# Diesel Bus | HD Diesel Truck | Speed Correction Curve
# Pre-2003   | Pre-2003        | SCF for Pre-2003 HDDT
# 2003-2006  | 2006-2006       | SCF for 2003-2006 HDDT
# 2007-2009  | 2007-2009       | SCF for 2007-2009 HDDT
# 2020+      | 2013+           | SCF for 2013+ HDDT
# ------------------------------------------------------------
# Table 4.3-56 Speed Correction Factors for CNG Buses
# ------------------------------------------------------------
# Diesel Bus | HD Diesel Truck | Speed Correction Curve
# Pre-2003   | 2003-2006       | SCF for 2003-2006 HDDT
# 2003-2007  | 2007-2009       | SCF for 2007-2009 HDDT
# 2008+      |  ----           | SCF based on CNG HDT data

rm(table4349a)

# ---------------------------------------------------------
# Eq.4.3-10 SCF for all pollutants and speed below 18.8 mph
# for PM, only Eq. 4.3.11 is applied
# Eq.4.3.11 SCF for all pollutants and speed between 18.8 and 65 mph
# ---------------------------------------------------------

# ---------------------------------------------------------
# Table 4.3-57: Speed Correction Factors for 2008+ Model Year CNG Buses
# ---------------------------------------------------------
thc <- function(speed){-1.031 * log(speed) + 5.906}
co <- function(speed){-2.076 * log(speed) + 16.22}
nox <- function(speed){-0.130 * log(speed)+ 0.727}
pm <- function(speed){6.34*10^(-6) * speed^2 - 6.16*10^(-4) * speed + 1.74*10^(-2)}
co2 <- function(speed){-549 * log(speed) + 3597}

scf <- function(pol,speed,year_group,fuel){
  # ---------------------------------------------------------
  # Table 4.3-49: Coefficients for EMFAC2017 HHDDT Speed Corrections Factors
  # ---------------------------------------------------------
  table4349 <- data.table::data.table(
    pollutant = c("HC","CO","NOx","NOx","PM","CO2"),
    from_year_group = c(2010,2010,2013,2010,2010,2010),
    to_year_group   = c(2020,2020,2012,2020,2020,2020),
    a = c(0.553,3.64,21.7,21.3, 0,7450),
    b = c(-1.15,-1.2,-0.527,-0.702,0,-0.469),
    c = c(3.77*10^(-2),.35,10.2,6.14,7.34,3610),
    d = c(-1.33*10^(-3),-1.24*10^(-2),-3.85,-0.225,-0.297,99.8),
    e = c(1.48*10^(-5),1.19*10^(-4),4.28*10^(-3),2.25*10^(-3),7.34*10^(-3),1.07)
  )
  table4349_above <- data.table::copy(table4349)[pollutant %nin% "PM",]
  table4349_above[ , `:=`(speed = "18.8-65mph",
                          a = 0, b = 0)]
  table4349[pollutant %in% "PM",`:=`(speed = "all",
                                     a = 0,
                                     b = 0)]
  table4349[pollutant %nin% "PM",`:=`(speed = "<18.8mph",
                                      c = 0,
                                      d = 0,
                                      e = 0)]
  table4349 <- data.table::rbindlist(list(table4349,table4349_above))
  # equations
  function_scf <- function(a,b,c,d,e,speed,temp_speed){
    switch(temp_speed,
           "<18.8mph" = (a * speed ^ b) / (a * 18.8 ^ b),
           "18.8-65mph" = (c + d * speed + e * speed^2)/(c + d * 18.8 + e * 18.8^2),
           "all" = (c + d * speed + e * speed^2)/(c + d * 18.8 + e * 18.8^2))
    }
    # speed
  temp_speed <- ifelse(speed < 18.8 ,"<18.8mph","18.8-65mph")
  
  
  # my table
  temp_4349 <- data.table::copy(table4349)[pollutant %in% pol & 
                                             from_year_group <= year_group & 
                                             to_year_group >= year_group & 
                                             speed %in% temp_speed,]
    # equation
    myscf <- function_scf(a = temp_4349$a,
                         b = temp_4349$b,
                         c = temp_4349$c,
                         d = temp_4349$d,
                         e = temp_4349$e,
                         speed = speed,
                         temp_speed = temp_speed)
# return
  return(myscf)

}

pol = "CO"
year_group = 2012
speed = 20
scf(pol = "CO2",speed = 15,year_group = 2010)


colpol <- names(emfac)[names(emfac) %like% "RUNEX"]
emfac[,(colpol) := get(colpol) * VMT]
emfac <- lapply(pol,function(i){ # i = pol[1]
  ef_pol <- emfac[,c('Calendar Year','Model Year','Speed','Fuel',i),with = FALSE]
  ef_pol[,Pollutant := gsub("_RUNEX","",i)]
  names(ef_pol) <- c(names(ef_pol)[1:4],"EF","Pollutant")
  return(ef_pol)
}) %>% data.table::rbindlist()
# speed intervals
emfac[,lower_speed_interval := Speed - 5]
emfac[,upper_speed_interval := Speed]
emfac[Speed %in% 90,upper_speed_interval := 130]
emfac[,Speed := NULL]
# units
emfac[,lower_speed_interval := lower_speed_interval %>% 
        units::set_units('mile/h') %>% 
        units::set_units('km/h')]
emfac[,upper_speed_interval := upper_speed_interval %>% 
        units::set_units('mile/h') %>% 
        units::set_units('km/h')]
emfac[,EF := EF %>% 
        units::set_units('g/mile') %>% 
        units::set_units('g/km')]
emfac[as.numeric(EF) > 0 & Pollutant %in% "NOx" & `Model Year` == '2020' & Fuel == "Diesel",EF] %>% 
  as.numeric() %>% mean()

ef_brazil(pollutant = "NOx",veh_type = "BUS_URBAN_D",years = 2015)

# write
#data.table::fwrite(emfac,"data/emfac.txt")
usa <- emfac
usethis::use_data(usa,overwrite = TRUE)
