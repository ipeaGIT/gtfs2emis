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
  lapply(1:length(input),function(i){ # i = 200
    # message
    message(paste0('shape_id: #', stringr::str_remove(output_name[i],".rds")," , ", i," out of ",length(input)))
    # read
    dt <- data.table::fread(paste0(input[i]))
    dt[,id := 1:nrow(dt)]
    # trip division
    mdist1 <- which(dt$cumdist == max(dt$cumdist))
    mdist0 <- c(1,head(mdist1,-1) + 1) 
    list_dist <- lapply(seq_along(mdist0), function(i){data.table(range = i,id = mdist0[i]:mdist1[i])}) %>%
      data.table::rbindlist()
    dt[list_dist, on= "id",range_trip := i.range]  # add range
    # stop division
    id0 <- c(1,which(!is.na(dt$stop_sequence))) 
    id1 <- c(id0[-1],nrow(dt))
    list_ids <- lapply(seq_along(id0),function(i){data.table(range = i,id = id0[i]:id1[i])}) %>% 
      rbindlist()
    dt[list_ids,on= "id",range_id := i.range]  # add range
    # first change
    dt1 <- dt[,.SD[1],by=.(range_trip,range_id)]
    dt1 <- setcolorder(dt1,names(dt))
    dt1 <- dt1[,c("range_id","id") := list(range_id-1,id-0.1)][-c(1,.N),]
    dt <- rbindlist(list(dt,dt1))[order(id)]
    # shape
    dt2 <- dt[,.SD[1],by = .(range_trip,range_id)]
    geom <- dt[,{
      geometry <- sf::st_linestring(x=matrix(c(shape_pt_lon,shape_pt_lat),ncol=2)) %>% 
        sf::st_sfc() %>% sf::st_sf()
    },by = .(range_trip,range_id)][,"geometry"]
    dt2$geometry <- sf::st_sf(geometry = geom,crs = 4326)
    # as.Itime
    dt2$departure_time <- data.table::as.ITime(dt2$departure_time)
    # 
    # fleet adding
    #
    #
    # check occupancy
    #
    real_fleet <- fleet_data[SHP %in% unique(dt2$shape_id) &
                               hora_liberacao < dt2[,.SD[1]][,departure_time],]
    # sample and occupancy time
    #
    placa <- real_fleet$Placa[sample(nrow(real_fleet),1)]
    fleet_data[Placa %in% placa, hora_liberacao := dt2[,.SD[.N]][,departure_time]]
    
    dt2$frota_ano <- fleet_data[Placa %in% placa,Ano_fabricacao][1]
    dt2$tipo_de_veiculo <- fleet_data[Placa %in% placa, tipo_de_veiculo][1]
    dt2$categoria <- fleet_data[Placa %in% placa, categoria][1]
    dt2$modelo_chassi <- fleet_data[Placa %in% placa,modelo_chassi][1]
    # 
    # create output dir and save
    #
    file_output <- paste0(output_folder,"/",output_name[i])
    readr::write_rds(x = dt2,path = file_output)
    return(NULL)
  })
  return(message('Files exported'))
}