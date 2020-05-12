library(data.table)
library(magrittr)
emfac <- data.table::fread("../../../../Dropbox/IPEA/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")
emfac <- emfac[`Vehicle Category` %in% 'UBUS',]
# --
# based on EMFAC2017 "Volume II – Handbook for Project-level Analyses V1.0.2",
# from March 1, 2018
# --
pol <- names(emfac)[names(emfac) %like% "RUNEX"]
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
# write
#data.table::fwrite(emfac,"data/emfac.txt")

usethis::use_data(emfac,overwrite = TRUE)
