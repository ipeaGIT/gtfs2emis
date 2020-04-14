#' @title Convert GPS to Lines-data type
#'
#' @description Transform intervals of GPS data of constant speed into
#' a linestring segment, in order to reduce data size of observations.
#'
#' @param input_filepath Character; location of exported GPS files
#' @param output_filepath Character; location for export Line-data type
#' @param fleet_path Character; path of fleet data associated with information of age, engine size,
#' weight gross total and shape_id's. This input associated with shape_id's allows the function to relate each
#' shape_id to a vehicle, by creating columns of fleet specification in the output.
#' Missing entry means no fleet allocation
#' @param  overwrite Logical; overwrites files in the output_filepath
#' @export
gps2line <- function(input_filepath,output_filepath,fleet_path = NA,overwrite = TRUE){
  #
  # tests
  # input_filepath = "test_joao/gps/"
  # output_filepath = "test_joao/lines/"
  # fleet_path = "inst/extdata/cur_fleet.tar.xz"
  # overwrite = TRUE
  #
  # list gps files in 'input_folder'
  #
  gps_line <- list.files(input_filepath, recursive = FALSE)
  #
  # check existing files in output_filepath files
  #
  check_files <- paste0(output_filepath,list.files(output_filepath,recursive = TRUE))
  #check_files_names <- gsub(".txt","",list.files(output_filepath))
  if(length(check_files) > 1 & overwrite == FALSE){
    gps_line <- gps_line[gps_line %nin% list.files(output_filepath)]
  }
  #
  # fleet_data
  #
  if(!is.na(fleet_path)){
    tempd <- file.path(tempdir(), "fleet") # create tempr dir to save GTFS unzipped files
    unlink(normalizePath(paste0(tempd, "/", dir(tempd)), mustWork = FALSE), recursive = TRUE) # clean tempfiles in that dir
    utils::untar(tarfile = fleet_path,exdir = tempd) # untar files
    f <- list.files(tempd)
    # read fleet
    fleet_data = data.table::fread(paste0(tempd,"/",f))
    fleet_data[,hora_liberacao := data.table::as.ITime("00:00:00")]
    }
  #
  # interation of all trip_id's
  #
  lapply(1:length(gps_line),function(i){ # i = 1
    # message
    message(paste0('shape_id: #', gsub(".txt","",gps_line[i])," , ", i," out of ",length(gps_line)))
    # read
    dt <- data.table::fread(paste0(input_filepath,gps_line[i]))
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
    if(!is.na(fleet_path)){
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
    filepath <- paste0(output_filepath,gps_line[i])
    data.table::fwrite(x = dt3,file = filepath)
    return(NULL)
  })
  return(message('Files exported'))
}
