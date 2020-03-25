#
#
# function built to export data from point to linestring
# 
#
read_gps <- function(input_folder,fleet_data){
  # input_folder = paste0("../../data/gps/",proj_cities$abrev_city)
  # fleet_data = readr::read_rds(paste0("../../data/fleet/",proj_cities$abrev_city,"/",proj_cities$abrev_city,".rds"))
  #
  # list gps files in 'input_folder'
  #
  input <- list.files(input_folder, recursive = FALSE,
                      include.dirs = FALSE,full.names = TRUE)
  
  output_name <- list.files(input_folder, recursive = FALSE,
                            include.dirs = FALSE,
                            full.names = FALSE) %>% 
    stringr::str_replace_all(".txt",".rds")
  #
  # fleet status
  #
  fleet_data[,hora_liberacao := data.table::as.ITime("00:00:00")]
  #
  # create output folder
  #
  output_folder <- paste0("../../data/gps_linestring/",proj_cities$abrev_city)
  dir.create(output_folder,showWarnings = FALSE)
  #
  # interation of all trip_id's
  #
  lapply(1:length(input),function(i){ # i = 2
    # message
    message(paste0('shape_id: #', stringr::str_remove(output_name[i],".rds")," , ", i," out of ",length(input)))
    # read
    dt <- data.table::fread(paste0(input[i]))
    dt[,id := 1:nrow(dt)]
    # trip division
    #mdist1 <- which(dt$cumdist == max(dt$cumdist))
    #mdist0 <- c(1,head(mdist1,-1)) 
    #list_dist <- lapply(seq_along(mdist0), function(i){data.table(range = i,id = mdist0[i]:mdist1[i])}) %>%
    #  data.table::rbindlist()
    #dt[list_dist, on= "id",range_trip := i.range]  # add range
    # stop division
    #d <- unique(dt$trip_id)
    #dt1 <- dt[range_trip %in% d,]
    id0 <- c(1,which(!is.na(dt$stop_sequence))) 
    id1 <- c(id0[-1],nrow(dt))
    list_ids <- lapply(seq_along(id0),function(i){data.table(range = i,id = (id0[i]:id1[i]))}) %>% 
      rbindlist()
    dt[list_ids,on = "id",range_id := i.range]  # add range
    # first change
    dt1 <- dt[,.SD[1],by = .(trip_id,range_id)]
    dt1 <- setcolorder(dt1,names(dt))
    dt1 <- dt1[,c("id","range_id") := list(id - 0.1,range_id - 1)]
    dt2 <- rbindlist(list(dt,dt1))[order(id)]
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
    # fleet adding
    #
    #
    # check occupancy
    #
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
    # 
    # create output dir and save
    #
    file_output <- paste0(output_folder,"/",output_name[i])
    readr::write_rds(x = dt3,path = file_output)
    return(NULL)
  })
  return(message('Files exported'))
}
