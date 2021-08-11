#
# prep EF from EMFAC (USA)----
#

rm(list=ls())
library(data.table)
library(magrittr)
library(vein)
`%nin%` = Negate(`%in%`)


#emfac <- data.table::fread("../../Dropbox/IPEA/gtfs2gps/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")
emfac <- data.table::fread("data-raw/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")
names(emfac) <- janitor::make_clean_names(names(emfac))

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
emfac <- emfac[vehicle_category %in% 'UBUS',]


# 1) fix fuel names ----
emfac$fuel %>% unique()
emfac[fuel %in% "Natural Gas", fuel := "CNG"]
emfac[fuel %in% "Diesel", fuel := "D"]
emfac[fuel %in% "Gasoline", fuel := "G"]

# 2) units of VMT----
emfac[,vmt := vmt %>% 
        units::set_units('mile') %>% 
        units::set_units("km")]


# 3) rearrange data cols----
colpol <- names(emfac)[names(emfac) %like% "runex"]
emfac <- lapply(colpol,function(i){ # i = colpol[1]
  ef_pol <- emfac[,c('calendar_year','model_year','speed','fuel','vmt',i),with = FALSE]
  ef_pol[,Pollutant := gsub("_runex","",i)]
  names(ef_pol) <- c(names(ef_pol)[1:5],"ef","pollutant")
  return(ef_pol)
}) %>% data.table::rbindlist()

# 3.1) rename pollutant
emfac$pollutant %>% unique()
emfac[,pollutant := stringr::str_remove_all(pollutant,"\\_") %>% toupper()]
emfac[pollutant %in% "NOX", pollutant := "NOx"]


# 4) add speed intervals----
emfac[,lower_speed_interval := speed - 5]
emfac[,upper_speed_interval := speed]
emfac[speed %in% 90,upper_speed_interval := 150]
emfac[,speed := NULL]



# 5) remove EF equal to zero----
emfac <- emfac[ef > 0,]

# 6) add units to lower and upper speed interval
emfac[,lower_speed_interval := lower_speed_interval %>% 
        units::set_units("mile/h") %>% 
        units::set_units("km/h") %>% 
        round(0)]
emfac[,upper_speed_interval := upper_speed_interval %>% 
        units::set_units("mile/h") %>% 
        units::set_units("km/h") %>% 
        round(0)]


# units of EF
emfac[,ef := ef %>% 
        units::set_units("g/mile") %>% 
        units::set_units("g/km")]


emfac[as.numeric(lower_speed_interval) > 0
      , lower_speed_interval := lower_speed_interval + units::set_units(1,"km/h")]


# check and adjust size of speed bins------

# The final data has keep speed bins intervals (SBI) with total size of 14.
# However, there are cases where only 11 or 13 SBI are available.
# In these cases, we need to fulfill the last two or three maximum SBI
# which are 
# 90 - 97 km/h,
# 98 - 105 km/h and
# 106 - 113 km/h.

# check cases with SBI < 14
tmp_emfac <- data.table::copy(emfac)
tmp_emfac <- tmp_emfac[,.N,by=.(calendar_year,model_year,fuel,pollutant )]
tmp_emfac <- tmp_emfac[N<14,]

extra_sbi <- lapply(1:nrow(tmp_emfac),function(i){ # i = 1
  get_ef <- emfac[calendar_year == tmp_emfac$calendar_year[i] &
                    model_year == tmp_emfac$model_year[i] &
                    fuel == tmp_emfac$fuel[i] &
                    pollutant == tmp_emfac$pollutant[i]]
  # number of rep
  n_rep <- 14 - tmp_emfac$N[i]
  # speeds
  ls <- units::set_units(c(106,98,90),"km/h")
  us <- units::set_units(c(113,105,97),"km/h")
  # create data.table
  dt <- data.table::data.table(
     "calendar_year" = rep(tmp_emfac$calendar_year[i],n_rep)
    ,"model_year" = rep(tmp_emfac$model_year[i],n_rep)
    ,"fuel" = rep(tmp_emfac$fuel[i],n_rep)
    ,"vmt" = rep(units::set_units(0,km),n_rep)
    ,"ef"= rep(get_ef[.N,ef],n_rep)
    ,"pollutant" = rep(tmp_emfac$pollutant[i],n_rep)
    ,"lower_speed_interval" = ls[1:n_rep]
    , "upper_speed_interval"= us[1:n_rep]
  )
  # export dt
  return(dt)
})
extra_sbi <- extra_sbi %>% 
  data.table::rbindlist()

# add extra_sbi on emfac

emfac_new <- rbind(emfac,extra_sbi)

# order 
data.table::setkeyv(emfac_new,cols = c("calendar_year"
                                       ,"model_year"
                                       ,"fuel"
                                       ,"pollutant"
                                       ,"lower_speed_interval"
                                       ,"upper_speed_interval"))

emfac_new[calendar_year == 2010 & 
        model_year == 1990 & 
        pollutant == "NOx" & fuel == "G"]


# 7) export-----
break()
usa <- data.table::copy(emfac_new)
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



