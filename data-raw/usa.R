rm(list=ls())
#devtools::load_all()
library(data.table)
library(magrittr)
`%nin%` = Negate(`%in%`)
emfac <- data.table::fread("../../Dropbox/IPEA/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")
#emfac <- data.table::fread("../../../joaobazzo/Downloads/EMFAC2017-EI-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20201112220725.csv")
#emfac <- data.table::fread("../../Dropbox/IPEA/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20201116093432.csv")
#emfac
emfac <- emfac[`Vehicle Category` %in% 'UBUS',]
emfac[Fuel %in% "Natural Gas", Fuel := "CNG"]
emfac[,VMT := VMT %>% 
        units::set_units('mile') %>% 
        units::set_units("km")]


colpol <- names(emfac)[names(emfac) %like% "RUNEX"]
#colpol <- gsub("_RUNEX","",colpol)
#emfac[,(colpol) := get(colpol) * VMT]
emfac <- lapply(colpol,function(i){ # i = colpol[1]
  ef_pol <- emfac[,c('Calendar Year','Model Year','Speed','Fuel','VMT',i),with = FALSE]
  ef_pol[,Pollutant := gsub("_RUNEX","",i)]
  names(ef_pol) <- c(names(ef_pol)[1:5],"EF","Pollutant")
  return(ef_pol)
}) %>% data.table::rbindlist()
# speed intervals
emfac[,lower_speed_interval := Speed - 5]
emfac[,upper_speed_interval := Speed]
emfac[Speed %in% 90,upper_speed_interval := 130]
emfac[,Speed := NULL]

# stretch
emfac01 <- lapply(1:nrow(emfac),function(i){ # i = 1
  speed_tmp <- emfac[i,lower_speed_interval]:(emfac[i,upper_speed_interval]-1)
  # update
  tmp_dt <- data.table::data.table(emfac[i,],"speed" = speed_tmp)
  # as.factor
  #coltemp <- colnames(emfac[i,])
  #tmp_dt[,(coltemp) := lapply(.SD,as.factor), .SDcols = coltemp]
  return(tmp_dt)
}) %>% data.table::rbindlist()

emfac02 <- data.table::copy(emfac01)[EF > 0,]
emfac02[,EF_1s := data.table::frollmean(EF,n = 2), by = .(`Calendar Year`,
                                                          `Model Year`,
                                                          Fuel,Pollutant)]
emfac02[is.na(EF_1s),EF_1s := EF]

break()
emfac02 %>% head(10)
emfac02[,.SD[1],by = .(`Calendar Year`,
                      `Model Year`,
                      Fuel,Pollutant,
                      speed)] %>% head(7)
emfac02[1:15,]

# units
#emfac[,lower_speed_interval := lower_speed_interval %>% 
#        units::set_units('mile/h') %>% 
#        units::set_units('km/h')]
#emfac[,upper_speed_interval := upper_speed_interval %>% 
#        units::set_units('mile/h') %>% 
#        units::set_units('km/h')]
emfac[,EF := EF %>% 
        units::set_units('g/mile') %>% 
        units::set_units('g/km')]

emfac <- emfac[as.numeric(EF) > 0,]
#emfac[,EF_VMT := VMT * EF]
emfac[,EF := units::set_units(EF,'g/mile')]

data_speed <- data.table::data.table("speed" = 1:120)

emfac01 <- emfac[data_speed,on = ("lower_speed_interval == speed")]

emfac_speed <- emfac
effsped <- data.table::copy(emfac_speed)[Pollutant %in% "CO2" & `Model Year` == '2002' &
                                           Fuel == "Diesel" & `Calendar Year` == 2016, ] 
effnos <- data.table::copy(emfac)[Pollutant %in% "CO2" & `Model Year` == '2002' & 
                                    Fuel == "Diesel"  & `Calendar Year` == 2016, ] 

emfac$Pollutant %>% unique()
plot(x = effsped$lower_speed_interval,y = effsped$EF)
effnos$EF %>% units::set_units("g/km")

ef_brazil(pollutant = "CO2",veh_type = "BUS_URBAN_D",
          years = 2007,as_list = FALSE) %>% units::set_units("g/km")

# write
# data.table::fwrite(emfac,"data/emfac.txt")
usa <- emfac
usethis::use_data(usa,overwrite = TRUE)



