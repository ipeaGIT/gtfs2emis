#' @title Estimate hot exhaust emissions 
#'
#' @description Transform intervals of GPS data of constant speed into
#' a linestring segment, in order to reduce data size of observations.
#' In Curitiba, for the sake of simplicity, I'm adding:
# a) "Hybric" vehicles as "Buses Standard"
# b) "Biarticulated" as "Articulated"
#'
#' @param pol_list Character; Vector of list of pollutants
#' @param input_folder Character;
#' @param output_folder Character;
#' @param overwrite Logical;
#' 
#' @export
emis <- function(pol_list, input_folder, output_folder = ".",overwrite = TRUE){
  #
  # input_folder = "test_joao/lines"
  # output_folder = "test_joao/emis"
  # pol_list = c("CO","NOx")
  # emission_factor = ef
  #
  # files in input_folder
  gps_line <- list.files(input_folder,recursive = FALSE)
  #gps_line_names <- list.files(input_folder,recursive = FALSE,full.names = FALSE) %>% 
  #  gsub(".txt","",gps_line[i])
  #
  # check existing files in output_folder and output 'gps_line'
  # files
  #
  check_files <- paste0(input_folder,"/",list.files(output_folder,recursive = TRUE))
  #check_files_names <- stringr::str_remove_all(list.files(output_folder),".txt")
  if(length(check_files) > 1 & overwrite == FALSE){
    gps_line <- gps_line[gps_line %nin% list.files(output_folder)]
  }
  # euro equivalence
  euro_eq <- data.table::data.table("euro_vein" = c(rep("I",4),rep("II",5),rep("III",6),rep("IV",3),rep("V",8)),
                                    "euro" = c(rep("Euro I",4),rep("Euro II",5),rep("Euro III",6),rep("Euro IV",3),rep("Euro V",8)),
                                    "ano" = c(1994:2019))
  # loop per line
  lapply(seq_along(gps_line),function(i){ # i = 1
    # introduction message
    message(paste0("shape_id #",gsub(".txt","",gps_line[i])," range ",i,"/",length(gps_line)))
    # read line
    dtline <- data.table::fread(paste0(input_folder,"/",gps_line[i]))
    dtline[,dist := units::set_units(dist,km)]
    #
    # check fleet completeness
    if(is.na(dtline$frota_ano[1])){
      message(paste0("shape_id #",gsub(".txt","",gps_line[i])," without fleet data"))
      return(NULL)
    }else{
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
        # emission factor
        FE_local <- vein::ef_cetesb(p = pol,
                                    veh = veh_type,
                                    year = unique(dtline$frota_ano),
                                    agemax = 1)
        FE_speed <- ef_hdv_scaled(dfcol = FE_local,vel = dtline$speed,veh =  veh_segment,fuel = "Diesel",
                                  euro = euro_eq[ano %in% unique(dtline$frota_ano),euro],
                                  pol = pol,show.equation = FALSE)
        # FE_vein <- vein::ef_hdv_scaled(dfcol = FE_local,v = "Ubus",t = veh_vein_type,g = veh_gweight,
        #                                eu = euro_eq[ano %in% unique(dtline$frota_ano),euro_vein],
        #                                p = pol)[[1]](dtline$speed)
        # allocate into data.table frame
        dtline[,paste0("EM_",pol) := FE_speed * dist]
        return(NULL)
      })
    }
    
    #
    # save file
    #
    data.table::fwrite(dtline,paste0(output_folder,"/",unique(dtline$shape_id),".txt"))
    return(NULL)
  })
}
