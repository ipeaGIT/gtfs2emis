#' @title Convert GPS to Lines-data type
#'
#' @description Transform intervals of GPS data of constant speed into
#' a linestring segment, in order to reduce data size of observations.
#'
#' @param input_filepath Character; location of exported GPS files
#' @param output_filepath Character; location for export Line-data type
#' @param fleet_data Data.table; fleet data associated with information of age, engine size,
#' weight gross total and shape_id's. This input associated with shape_id's allows the function to relate each
#' shape_id to a vehicle, by creating columns of fleet specification in the output.
#' Missing entry means no fleet allocation
#' @param  overwrite Logical; overwrites files in the output_filepath
#' @export
gps_to_linestring <- function(input_filepath,output_filepath,fleet_data,overwrite = TRUE){
  # input_filepath = paste0("../../data/gps/",proj_cities$abrev_city)
  # output_filepath <- paste0("../../data/gps_linestring/",proj_cities$abrev_city)
  # fleet_data = readr::read_rds(paste0("../../data/fleet/",proj_cities$abrev_city,"/",proj_cities$abrev_city,".rds"))
  #
  # list gps files in 'input_folder'
  #
  input <- list.files(input_filepath, recursive = FALSE,
                      include.dirs = FALSE,full.names = TRUE)

  output_name <- list.files(input_filepath, recursive = FALSE,
                            include.dirs = FALSE,
                            full.names = FALSE) %>%
    stringr::str_replace_all(".txt",".rds")
  #
  # check existing files in output_filepath
  # files
  #
  check_files <- paste0(input_folder,"/",list.files(output_folder))
  check_files_names <- stringr::str_remove_all(list.files(output_folder),".rds")
  if(length(check_files) > 0 & overwrite == FALSE){
    gps_line <- gps_line[gps_line %nin% check_files]
    gps_line_names <- gps_line_names[gps_line_names %nin% check_files_names]
  }
  #
  # fleet_data
  #
  if(!missing(fleet_data)){  fleet_data[,hora_liberacao := data.table::as.ITime("00:00:00")] }
  #
  # interation of all trip_id's
  #
  lapply(1:length(input),function(i){ # i = 2
    # message
    message(paste0('shape_id: #', stringr::str_remove(output_name[i],".rds")," , ", i," out of ",length(input)))
    # read
    dt <- data.table::fread(paste0(input[i]))
    dt[,id := 1:nrow(dt)]
    # stop division
    id0 <- c(1,which(!is.na(dt$stop_sequence)))
    id1 <- c(id0[-1],nrow(dt))
    list_ids <- lapply(seq_along(id0),function(i){data.table::data.table(range = i,id = (id0[i]:id1[i]))}) %>%
      data.table::rbindlist()
    dt[list_ids,on = "id",range_id := i.range]  # add range
    # first change
    dt1 <- dt[,.SD[1],by = .(trip_id,range_id)]
    dt1 <- data.table::setcolorder(dt1,names(dt))
    dt1 <- dt1[,c("id","range_id") := list(id - 0.1,range_id - 1)]
    dt2 <- data.table::rbindlist(list(dt,dt1))[order(id)]
    # shape
    dt3 <- dt2[,.SD[1],by = .(range_id,trip_id)]
    geom <- dt2[,{
      geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon,shape_pt_lat),ncol = 2)) %>%
        sf::st_sfc() %>% sf::st_sf()
    },by = .(range_id,trip_id)][,"geometry"]
    dt3$geometry <- sf::st_sf(geometry = geom,crs = 4326)
    # as.Itime
    dt3$departure_time <- data.table::as.ITime(dt3$departure_time)
    dt3$dist <- sf::st_length(dt3$geometry)
    dt3 <- dt3[-which(units::drop_units(dt3$dist) == 0),]
    #
    # test
    #
    # dt4 <- dt3 %>% sf::st_as_sf()
    # dt4 <- dt4[-which(units::drop_units(dt3$dist) == 0),]
    # mapview(dt4$geometry)
    # dt5 <- dt4[dt4$shape_id %in% "1709",]
    # mapview(dt5$geometry)
    #
    # condition of fleet data
    #
    if(!missing(fleet_data)){
      real_fleet <- fleet_data[SHP %in% unique(dt3$shape_id) &
                                 hora_liberacao < dt3[,.SD[1]][,departure_time],]
      # sample and occupancy time
      #
      placa <- real_fleet$Placa[sample(nrow(real_fleet),1)]
      fleet_data[Placa %in% placa, hora_liberacao := dt3[,.SD[.N]][,departure_time]]

      dt3$frota_ano <- fleet_data[Placa %in% placa,Ano_fabricacao][1]
      dt3$tipo_de_veiculo <- fleet_data[Placa %in% placa, tipo_de_veiculo][1]
      dt3$categoria <- fleet_data[Placa %in% placa, categoria][1]
      dt3$modelo_chassi <- fleet_data[Placa %in% placa,modelo_chassi][1]
    }
    #
    # create output dir and save
    #
    filepath <- paste0(output_filepath,"/",output_name[i])
    readr::write_rds(x = dt3,path = filepath)
    return(NULL)
  })
  return(message('Files exported'))
}
