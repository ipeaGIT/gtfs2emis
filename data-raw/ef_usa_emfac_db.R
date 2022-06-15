#
# prep EF from EMFAC (USA)----
#

rm(list=ls())
library(data.table)
library(magrittr)
#library(vein)
#`%nin%` = Negate(`%in%`)


#emfac <- data.table::fread("../../Dropbox/IPEA/gtfs2gps/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")
emfac <- data.table::fread("../../Pessoal/IPEA/gtfs2gps/data-raw/emission_factors/EMFAC/EMFAC2017-ER-2011Class-Statewide2010-2011-2012-2013-2014-2015-2016-2017-2019-2020-Annual-20200512000741.csv")
names(emfac) <- janitor::make_clean_names(names(emfac))

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
data.table::setnames(x = emfac
                     ,old = "vmt"
                     ,new = "vkt")

# change names
data.table::setnames(x = emfac
                     ,old = "calendar_year"
                     ,new = "reference_year")

# 3) rearrange data cols----
colpol <- names(emfac)[names(emfac) %like% "runex"]

emfac <- lapply(colpol,function(i){ # i = colpol[1]
  
  ef_pol <- emfac[,c('reference_year','model_year','speed','fuel','vkt',i),with = FALSE]
  
  ef_pol[,Pollutant := gsub("_runex","",i)]
  
  names(ef_pol) <- c(names(ef_pol)[1:5],"ef","pollutant")
  
  return(ef_pol)
  
}) %>% data.table::rbindlist()

# 3.1) rename pollutant
emfac$pollutant %>% unique()
emfac[,pollutant := stringr::str_remove_all(pollutant,"\\_") %>% toupper()]
emfac[pollutant %in% "NOX", pollutant := "NOx"]
table(emfac$pollutant)

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
      , lower_speed_interval := lower_speed_interval +
        units::set_units(1,"km/h")]

# check and adjust size of speed bins------

# The final data has kept speed bins intervals (SBI) with a total size of 14.
# However, there are cases where only 11 or 13 SBI are available.
# In such cases, we fulfill the last two or three maximum SBI, which are: 
#           90 - 97 km/h,
#           98 - 105 km/h, and
#           106 - 113 km/h.

# check cases with SBI < 14
tmp_emfac <- data.table::copy(emfac)
tmp_emfac <- tmp_emfac[,.N,by=.(reference_year,model_year,fuel,pollutant )]
tmp_emfac <- tmp_emfac[N<14,]

extra_sbi <- lapply(1:nrow(tmp_emfac),function(i){ # i = 1
  
  get_ef <- emfac[
    reference_year == tmp_emfac$reference_year[i] &
      model_year   == tmp_emfac$model_year[i] &
      fuel         == tmp_emfac$fuel[i] &
      pollutant    == tmp_emfac$pollutant[i]
  ]
  # number of rep
  n_rep <- 14 - tmp_emfac$N[i]
  # speeds
  ls <- units::set_units(c(106,98,90),"km/h")
  us <- units::set_units(c(113,105,97),"km/h")
  
  # create data.table
  dt <- data.table::data.table(
    "reference_year"        = rep(tmp_emfac$reference_year[i],n_rep)
    ,"model_year"           = rep(tmp_emfac$model_year[i]   ,n_rep)
    ,"fuel"                 = rep(tmp_emfac$fuel[i]         ,n_rep)
    ,"vkt"                  = rep(units::set_units(0,km)    ,n_rep)
    ,"ef"                   = rep(get_ef[.N,ef]             ,n_rep)
    ,"pollutant"            = rep(tmp_emfac$pollutant[i]    ,n_rep)
    ,"lower_speed_interval" = ls[1:n_rep]
    ,"upper_speed_interval" = us[1:n_rep]
  )
  # export dt
  return(dt)
})

extra_sbi <- data.table::rbindlist(extra_sbi)

# Rbind 'extra_sbi'  in 'emfac'

emfac_new <- rbind(emfac,extra_sbi)

# order 
data.table::setkeyv(emfac_new,cols = c("reference_year"
                                       ,"model_year"
                                       ,"fuel"
                                       ,"pollutant"
                                       ,"lower_speed_interval"
                                       ,"upper_speed_interval"))

# check
emfac_new[
  reference_year == 2010 & 
    model_year == 1990 & 
    pollutant == "NOx" & 
    fuel == "G"
]


# 7) export-----
ef_usa_emfac_db <- data.table::copy(emfac_new)
usethis::use_data(ef_usa_emfac_db,overwrite = TRUE)
