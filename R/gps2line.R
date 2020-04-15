#' @title Convert GPS to Lines-data type
#'
#' @description Transform intervals of GPS data of constant speed into
#' a linestring segment, in order to reduce data size of observations.
#'
#' @param input_file Data.frame/Data.table; location of exported GPS files or GPS files
#' @param output_filepath Character; location for export Line-data type. If missing it returns the values.
#' @export
gps2line <- function(input_file,output_filepath = NA){
  #
  # tests
  # input_filepath = "test_joao/gps/"
  # output_filepath = "test_joao/lines/"
  # fleet_path = "inst/extdata/cur_fleet.tar.xz"
  # overwrite = TRUE
  #
  # list gps files in 'input_folder'
  #
  if(is.character(input_file)){
    dt <- data.table::fread(input_file)}
  else{
    dt <- input_file
  }
  #
  # interation of all trip_id's
  #
  # read
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
  # create output dir and save
  #
  if(is.na(output_filepath)){
    return(dt3)
  }else{
    data.table::fwrite(x = dt3,file = output_filepath)
    return(message(paste0('Files exported ',output_filepath)))
  }
}
