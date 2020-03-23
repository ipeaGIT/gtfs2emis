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
emis <- function(pol_list = pol, input_folder = input_folder, output_folder = ".",emission_factor = ef){
  #
  # files in input_folder
  gps_line <- list.files(input_folder,recursive = FALSE,full.names = TRUE)
  gps_line_names <- list.files(input_folder,recursive = FALSE,full.names = FALSE) %>% stringr::str_remove_all(".rds")
  #
  # check existing files in output_folder and output 'gps_line'
  # files
  #
  check_files <- paste0(input_folder,"/",list.files(output_folder))
  if(length(check_files) > 0){
    gps_line <- gps_line[gps_line %nin% check_files]
  }
  # euro equivalence
  euro_eq <- data.table("euro_vein" = c(rep("I",4),rep("II",5),rep("III",6),rep("IV",3),rep("V",8)),
                        "euro" = c(rep("Euro I",4),rep("Euro II",5),rep("Euro III",6),rep("Euro IV",3),rep("Euro V",8)),
                        "ano" = c(1994:2019))
  # loop per line
  lapply(seq_along(gps_line),function(i){ # i = 5
    # introduction message
    message(paste0("shape_id #",gps_line_names[i]," range ",i,"/",length(gps_line)))
    # read line
    dtline <- readr::read_rds(gps_line[i])
    dtline[,dist := units::set_units(dist / 1000,km)]
    #
    # check fleet completness
    if(length(dim(dtline$frota_ano)) > 0){
      message(paste0("shape_id #",gps_line_names[i]," with issues"))
      return(NULL)
    }
    # fleet
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
    
    lapply(pol_list,function(pol){ # pol = pol_list[1]
      #
      # emission factor
      #
      FE_local <- vein::ef_cetesb(p = pol,
                                  veh = veh_type,
                                  year = unique(dtline$frota_ano),
                                  agemax = 1)
      FE_speed <- ef_hdv_scaled_2019(dfcol = FE_local,vel = dtline$speed,ef = ef,veh = "Buses",
                                     segment = veh_segment,fuel = "Diesel",
                                     euro = euro_eq[ano %in% unique(dtline$frota_ano),euro],
                                     pol = pol,show.equation = FALSE)
      # FE_vein <- vein::ef_hdv_scaled(dfcol = FE_local,v = "Ubus",t = veh_vein_type,g = veh_gweight,
      #                                eu = euro_eq[ano %in% unique(dtline$frota_ano),euro_vein],
      #                                p = pol)[[1]](dtline$speed)
      # allocate into data.table frame
      dtline[,paste0("EM_",pol) := FE_speed * dist]
      return(NULL)
    })
    #
    # save file
    #
    readr::write_rds(dtline,paste0(output_folder,"/",unique(dtline$shape_id),".rds"))
  })
}
