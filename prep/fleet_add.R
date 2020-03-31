#' @title Add fleet information into a column of Linestring data type
#'
#' @description Information of year of usage, engine and total gross weight is added
#' into a Linestring data based on 'shape_id' common characteristics
#' 
#' @param input_filepath Character; location of exported Linestring-data type
#' @param fleet_data Data.table; fleet data with information of age, engine size, weight gross total
#' @param output_filepath Character; location for export Line-data type with fleet information. 
#' @export
fleet_add <- function(input_filepath,fleet_data,output_filepath){
  #
  # list gps files in 'input_folder'
  #
  #input <- list.files(input_filepath, recursive = FALSE,
  #                    include.dirs = FALSE,full.names = TRUE)
  #output_name <- list.files(input_filepath, recursive = FALSE,
  #                          include.dirs = FALSE,
  #                          full.names = FALSE) %>% 
  #  stringr::str_replace_all(".txt",".rds")
  #
  # initial status
  fleet_data[,hora_liberacao := data.table::as.ITime("00:00:00")]
  lapply(1:length(input),function(i){ # i = 2
    # message
    message(paste0('shape_id: #', stringr::str_remove(output_name[i],".rds")," , ", i," out of ",length(input)))
    # read_file
    dt <- data.table::fread(paste0(input[i]))
    #
    # check occupancy
    #
    real_fleet <- fleet_data[SHP %in% unique(dt$shape_id) &
                               hora_liberacao < dt[,.SD[1]][,departure_time],]
    # sample and occupancy time
    #
    placa <- real_fleet$Placa[sample(nrow(real_fleet),1)]
    fleet_data[Placa %in% placa, hora_liberacao := dt[,.SD[.N]][,departure_time]]
    
    dt$frota_ano <- fleet_data[Placa %in% placa,Ano_fabricacao][1]
    dt$tipo_de_veiculo <- fleet_data[Placa %in% placa, tipo_de_veiculo][1]
    dt$categoria <- fleet_data[Placa %in% placa, categoria][1]
    dt$modelo_chassi <- fleet_data[Placa %in% placa,modelo_chassi][1]
    # 
    # create output dir and save
    #
    file_output <- paste0(output_filepath,"/",output_name[i])
    readr::write_rds(x = dt,path = file_output)
    return(NULL)
  })
}