#function(pol = say_pol)
gps_line <- list.files(paste0("../../data/gps_linestring/",proj_cities$abrev_city),
                       recursive = FALSE,
                       full.names = TRUE)
# euro equivalence
euro_eq <- data.table("euro_vein" = 
                        c(rep("I",4),rep("II",5),rep("III",6),rep("IV",3),rep("V",8)),
                      "euro" = c(rep("Euro I",4),rep("Euro II",5),rep("Euro III",6),rep("Euro IV",3),rep("Euro V",8)),
                      "ano" = c(1994:2019))
# read line
dtline <- readr::read_rds(gps_line[1])

unique(dtline$categoria)
unique(dtline$frota_ano)
unique(dtline$modelo_chassi)
unique(dtline$tipo_de_veiculo)
# ----
#
# tipos de onibus
#
# https://www.urbs.curitiba.pr.gov.br/transporte/rede-integrada-de-transporte/24
#
# "ARTICULADO" (28t), "ARTIC. BIO" (28t): ARTICULADO
# "MICRO ESPECIAL" (12-15t), "MICRO" (8.5t): MICROONIBUS
# "SEMI PADRON" (18t), "PADRON" (18t), "COMUM" (17t): COMUM
# "BIARTICULADO" (40.5 t), "BIARTIC. BIO" (40.5 t): BIARTICULADO
#
# "HIBRIDO BIO"    "HIBRIDO": HIBRIDO
#  The term ‘full’ refers to hybrids that can start only powered by their electric motor.
#  Hybrid is the combination of an internal combustion engine and an electric motor. 
#
#
# for the sake of simplicity, I'm adding:
# a) "Hybric" vehicles as "Buses Standard"
# b) "Biarticulated" as "Articulated"
# 
# further research should be done to find out how to use
# hybrid vehicles and biarticulated
#
# ----
pol = "CO"
if(unique(dtline$tipo_de_veiculo) %in% c("SEMI PADRON","PADRON","COMUM","HIBRIDO BIO","HIBRIDO")){
  veh_type = "BUS_URBAN_D" # for CETESB emission factor data base
  veh_segment = "Urban Buses Standard 15 - 18 t"
  veh_vein_type = "Std"
  veh_gweight = ">15 & <=18"
}
if(unique(dtline$tipo_de_veiculo) %in% c("MICRO ESPECIAL","MICRO")){
  veh_type = "BUS_MICRO_D" # for CETESB emission factor data base
  veh_segment = "Urban Buses Midi <=15 t"
  veh_vein_type = "Midi"
  veh_gweight = "<=18"
}
if(unique(dtline$tipo_de_veiculo) %in% c("ARTICULADO","ARTIC. BIO","BIARTICULADO","BIARTIC. BIO")){
  veh_type = "BUS_ARTIC_D"  # for CETESB emission factor data base
  veh_segment = "Urban Buses Articulated >18 t"
  veh_vein_type = "TT"
  veh_gweight = ">18"
}

#
# emission factor
#
FE_local <- vein::ef_cetesb(p = pol,
                            veh = veh_type,
                            year = unique(dtline$frota_ano),
                            agemax = 1)
FE_speed <- ef_hdv_speed_2019(vel = dtline$speed,ef = ef,veh = "Buses",
                              segment = veh_segment,fuel = "Diesel",
                              euro = euro_eq[ano %in% unique(dtline$frota_ano),euro],
                              tech = "SCR",pol = pol,slope = 0.0,load = 0.5)
FE_vein <- vein::ef_hdv_speed(v = "Ubus",t = veh_vein_type,g = veh_gweight,
                              eu = euro_eq[ano %in% unique(dtline$frota_ano),euro_vein],
                              gr = 0.0,l = 0.5,p = pol,show.equation = TRUE,
                              speed = vein::Speed(dtline$speed,km/h),fcorr = 1,k = 1)



FE_scaled <- vein::ef_hdv_scaled(dfcol = FE_local,
                                 v = "Ubus", 
                                 t = "Std",
                                 g = ">15 & <=18", eu = "V", 
                                 gr = 0, l = 0.5, p = "NOx")

}
if(unique(file$tipo_de_veiculo %in% c("MICRO ESPECIAL"))) 
  