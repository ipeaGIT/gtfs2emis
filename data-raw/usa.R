#
# prep EF from EMFAC (USA)----
#

rm(list=ls())
library(data.table)
library(magrittr)
library(vein)
`%nin%` = Negate(`%in%`)


emfac <- data.table::fread("../../Dropbox/IPEA/gtfs2gps/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")


#
# generate fuel correction
#

# source("data-raw/usa_fc.R")
# scf <- scf_emfac()
# scf[,speed := round(speed,0)]
#
#
#
#emfac <- data.table::fread("../../../joaobazzo/Downloads/EMFAC2017-EI-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20201112220725.csv")
#emfac <- data.table::fread("../../Dropbox/IPEA/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20201116093432.csv")


# fix categories
emfac <- emfac[`Vehicle Category` %in% 'UBUS',]


# 1) fix fuel names ----
emfac$Fuel %>% unique()
emfac[Fuel %in% "Natural Gas", Fuel := "CNG"]
emfac[Fuel %in% "Diesel", Fuel := "D"]
emfac[Fuel %in% "Gasoline", Fuel := "G"]

# 2) units of VMT----
emfac[,VMT := VMT %>% 
        units::set_units('mile') %>% 
        units::set_units("km")]


# 3) rearrange data cols----
colpol <- names(emfac)[names(emfac) %like% "RUNEX"]
emfac <- lapply(colpol,function(i){ # i = colpol[1]
  ef_pol <- emfac[,c('Calendar Year','Model Year','Speed','Fuel','VMT',i),with = FALSE]
  ef_pol[,Pollutant := gsub("_RUNEX","",i)]
  names(ef_pol) <- c(names(ef_pol)[1:5],"EF","Pollutant")
  return(ef_pol)
}) %>% data.table::rbindlist()

# 4) add speed intervals----
emfac[,lower_speed_interval := Speed - 5]
emfac[,upper_speed_interval := Speed]
emfac[Speed %in% 90,upper_speed_interval := 130]
emfac[,Speed := NULL]



# 5) remove EF equal to zero----
emfac <- emfac[EF > 0,]



# 6) adjust to all speeds-----
emfac01 <- lapply(1:nrow(emfac),function(i){ # i = 1
  tmp_dt <- data.table::data.table(emfac[i,]
                                   ,"speed" = emfac[i,lower_speed_interval]:(emfac[i,upper_speed_interval]-1)
                                   )
  return(tmp_dt)
}) %>% data.table::rbindlist()


# 7) add a moving average----

emfac02 <- data.table::copy(emfac01)
emfac02[,EF_1s := data.table::frollmean(EF,n = 2), by = .(`Calendar Year`,
                                                          `Model Year`,
                                                          Fuel,Pollutant)]
emfac02[is.na(EF_1s),EF_1s := EF]
emfac02[,EF_1s := EF_1s %>% 
          units::set_units("g/mile") %>% 
          units::set_units("g/km")]

# 8) setnames
usa <- data.table::copy(emfac02)
usa[,EF := NULL]

data.table::setnames(usa,"EF_1s","EF")
usethis::use_data(usa,overwrite = TRUE)







# # add scf into emission factor
# 
# emfac03 <- emfac02[scf, on = c("Pollutant" = "pollutant",
#                                "speed" = "speed",
#                                "Fuel" = "fuel",
#                                "Model Year" = "year")]
# emfac03 <- emfac03[complete.cases(emfac03)]
# emfac03 <- emfac03[,cor_ef := EF_1s * scf]
# emfac03
# break()
# library(ggplot2)
# p = "CO2"
# ef_br <- vein::ef_cetesb(p = p,veh = "BUS_URBAN_D",year = 2014,agemax = 1)
# ef_sc_br <- vein::ef_hdv_scaled(dfcol = ef_br,v = "Ubus",t = "Std",g = ">15 & <=18",
#                                 eu = "IV",p = p,gr = 0.00,l = 0.5) 
# tmpSpeed <- 1:65 %>% units::set_units("mile/h") %>% units::set_units("km/h") %>% as.numeric()
# ef_sc_br <- data.table::data.table("speed" = 1:65,"ef" = ef_sc_br[[1]](tmpSpeed))
# 
# 
# ggplot() + 
#   geom_line(data = emfac03[Pollutant %in% p & Fuel %in% "Diesel" & 
#                              `Model Year` == 2015 & `Calendar Year` == 2014,],
#             aes(x = speed,y = EF_1s))+
#   geom_point(data = emfac03[Pollutant %in% p & Fuel %in% "Diesel" & 
#                               `Model Year` == 2015 & `Calendar Year` == 2014,],
#              aes(x = speed,y = cor_ef),color = "red")+
#   geom_hline(yintercept = as.numeric(ef_br),col = "darkgreen") +
#   geom_line(data = ef_sc_br,aes(x = speed, y = ef_sc_br$ef),col="purple") +
#   labs(x = "speed (miles/h)", y= "Ef (g/km)",title = p)
# 
# 
# 
# geom_point(data = emfac03[Pollutant %in% "CO" & Fuel %in% "Diesel" & 
#                             `Model Year` == 2015 & `Calendar Year` == 2014,],
#            aes(x = speed,y = cor_ef),color = "red")
# # 
# # link_vein <- "https://github.com/atmoschem/vein/blob/master/R/sysdata.rda?raw=true"
# # download.file(url = link_vein,destfile = "sysdata.rda")
# load("sysdata.rda")
# # library(data.table)
# # emfac_agg <- sysdata$emfac_agg
# # colPol <- colnames(emfac_agg)[colnames(emfac_agg) %like% paste0("CO2","_RUNEX")]
# # emfac_agg[Vehicle_Category %like% "UBUS" & Fuel %in% "Diesel" & 
# #             `Model_Year` == 2015 & `Calendar_Year` == 2020,]
# # library(vein)
# # vein::ef
# # vein::ef_emfac(veh = "UBUS", fuel = "Diesel",pol = "CO2_RUNEX")
# # emfac02 %>% head(10)
# # emfac02[,.SD[1],by = .(`Calendar Year`,
# #                        `Model Year`,
# #                        Fuel,Pollutant,
# #                        speed)] %>% head(7)
# # emfac02[1:15,]
# 
# 
# 
# emfac <- emfac[as.numeric(EF) > 0,]
# #emfac[,EF_VMT := VMT * EF]
# emfac[,EF := units::set_units(EF,'g/mile')]
# 
# data_speed <- data.table::data.table("speed" = 1:120)
# 
# emfac01 <- emfac[data_speed,on = ("lower_speed_interval == speed")]
# 
# emfac_speed <- emfac
# effsped <- data.table::copy(emfac_speed)[Pollutant %in% "CO2" & `Model Year` == '2002' &
#                                            Fuel == "Diesel" & `Calendar Year` == 2016, ] 
# effnos <- data.table::copy(emfac)[Pollutant %in% "CO2" & `Model Year` == '2002' & 
#                                     Fuel == "Diesel"  & `Calendar Year` == 2016, ] 
# 
# emfac$Pollutant %>% unique()
# plot(x = effsped$lower_speed_interval,y = effsped$EF)
# effnos$EF %>% units::set_units("g/km")
# 
# ef_brazil(pollutant = "CO2",veh_type = "BUS_URBAN_D",
#           years = 2007,as_list = FALSE) %>% units::set_units("g/km")
# 
# # write
# data.table::fwrite(emfac,"data/emfac.txt")



