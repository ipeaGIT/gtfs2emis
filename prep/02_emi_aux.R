function(pol = say_pol)


gps_line <- list.files(paste0("../../data/gps_linestring/",proj_cities$abrev_city),
                       recursive = FALSE,
                       full.names = TRUE)

file <- readr::read_rds(gps_line[1])

unique(fleet$categoria)
unique(fleet$Ano_fabricacao)
unique(fleet$modelo_chassi)
unique(fleet$tipo_de_veiculo)

#
# tipos de onibus
#
# https://www.urbs.curitiba.pr.gov.br/transporte/rede-integrada-de-transporte/24
#
# "ARTICULADO", "ARTIC. BIO": ARTICULADO
# "MICRO ESPECIAL", "MICRO": MICROONIBUS
# "SEMI PADRON", "PADRON", "COMUM": COMUM
# "BIARTICULADO", "BIARTIC. BIO": BIARTICULADO
# "HIBRIDO BIO"    "HIBRIDO": HIBRIDO
#  eletricidade e biodiesel B100
#
#
if(unique(file$tipo_de_veiculo) %in% c("SEMI PADRON","PADRON","COMUM")){
  #
  # fator de emissao CETESB
  #
  FE_local <- vein::ef_cetesb(p = "CO2",
                              veh = "BUS_URBAN_D",
                              year = unique(file$frota_ano),
                              agemax = 1)
  FE_scaled <- vein::ef_hdv_scaled(dfcol = FE_local,
                                   v = "Ubus", 
                                   t = "Std",
                                   g = ">15 & <=18", eu = "V", 
                                   gr = 0, l = 0.5, p = "NOx")
  
}
  if(unique(file$tipo_de_veiculo %in% c("MICRO ESPECIAL"))) 
