# ---
#
# emission factor analysis of HDV
#
# comparison between EMEP/EEA 2019 and EMEP/EEA 2016 (incorpored into VEIN)
#
# ---
library(vein)
library(dplyr)
library(data.table)
source("prep/09_emep-eea_emission-factor.R")
ef <- openxlsx::read.xlsx("../../emission_routes_joao/references/copert/1.A.3.b.i-iv Road transport hot EFs Annex 2018_Dic.xlsx") %>% 
  data.table::as.data.table()
ef <- ef[Category %in% "Buses",]


buses_type_aut <- unique(ef$Segment)[1:3]
buses_type_vein <- c("Midi","Std","RT")
g_vein <- c( "<=15",">15 & <=18",">18")
euro_vein <- c("PRE", "I", "II", "III", "IV", "V")
euro_aut <- c("Conventional","Euro I","Euro II","Euro III","Euro IV","Euro V")
  
e = 6; i = 1
#sapply(1:nrow(buses_type), function(i){
  message(buses_type[i])
vein::ef_hdv_speed(v = "Ubus",
                   speed = vein::Speed(86,"km/h"),
                              t = buses_type_vein[i],
                              g = g_vein[i],
                              eu = euro_vein[e],
                              gr = 0,
                              l = 1.0,
                              p = "CO",
                              show.equation = FALSE) 
  
  ef_hdv_speed_2019(vel = 86,
                  ef = ef,
                  veh = "Buses",
                  fuel = "Diesel",
                  segment = buses_type[i],
                  euro = euro_aut[e],
                  tech = "EGR",
                  pol = "CO",
                  slope = 0,
                  load = 0.5)
  ef[Category %in% veh & 
       Fuel %in% "Diesel" & 
       Segment %in% buses_type[i] & 
       Euro.Standard %in% euro_aut[e] & 
       Technology %in% "EGR" &
       Pollutant %in% "CO" &
       Road.Slope %in% 0 &
       #Mode %in% mode &
       Load %in% 1.0,`EF.[g/km].or.ECF.[MJ/km]`]
  
})



lapply(c(1,2,3,4,5,6,7,8,9), function(i){table(ef[,..i],useNA = "always")})
vein::ef_ldv_speed()


