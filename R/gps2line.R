#' @title Converts a GPS-like data.table to a MultiLineString Simple Feature (sf) object
#'
#' @description Every interval of GPS data points of constant speed for each trip_id is
#'  converted into a linestring segment.
#'
#' @param input_file A data.table with timestamp data.
#' @param crs A Coordinate Reference System. The default value is 4326 (latlong WGS84).
#' @return A simple feature (sf) object with MultiLineString data.
#' 
#' @export
#' @examples
#'cur_read <- gtfs2gps::read_gtfs(system.file("extdata/gtfs_cur.zip", package ="gtfs2emis"))
#'cur_gtfs <- gtfs2gps::filter_by_shape_id(gtfs_data = cur_read,shape_ids = c(1708))
#'cur_gtfs <- gtfs2gps::gtfs2gps(gtfs_data = cur_gtfs)
#'cur_gpslines <- gps_as_sflinestring(input_file = cur_gtfs) 
#'plot(cur_gpslines$geometry)
#'
gps_as_sflinestring  <- function(input_file, crs = 4326){
  
  
  if(is.character(input_file)){
    dt <- data.table::fread(input_file)}
  else{
    dt <- input_file
  }
  
  
  ### iteration over all trip_id's
  
  
  # create row ids
  #>>> isso nao teria q ser `by = trip_id``
  dt[, id := .I]
  
  ## stop division
  
  # get row potion of each stop
  id0 <- c(1, which(!is.na(dt$stop_sequence)))
  
  # get row position right before consecutive stop (eh isso mesmo ????)
  id1 <- c(id0[-1], nrow(dt))
  
  #>>> ~~~~~~~~~~~~ Improve documentation here ~~~~~~~~~~~~
  # create a data table grouping ids by unique intervals
  list_ids <- lapply(seq_along(id0),function(i){data.table::data.table(interval = i, id = (id0[i]:id1[i]))}) %>%
    data.table::rbindlist()
  
  # add interval
  dt[list_ids, on = "id", interval_id := i.interval]
  
  
  #>>> ~~~~~~~~~~~~ Improve documentation here ~~~~~~~~~~~~
  
  dt1 <- dt[,.SD[1],by = .(trip_id,interval_id)]    # add extra points in valid_id's 
  dt1 <- data.table::setcolorder(dt1,names(dt))
  dt1 <- dt1[,c("id","interval_id") := list(id - 0.1,interval_id - 1)] # create unique id's
  dt2 <- data.table::rbindlist(list(dt,dt1))[order(id)]
  
  # create unique id for each unique combinarion of interval_id & trip_id
  dt2[, grp := .GRP, by = .(interval_id,trip_id) ]
  
  # function to convert to multilnestring
  flines <- function(long,lat){
    exp <- matrix(c(long,lat),ncol = 2) %>% sfheaders::sf_linestring()
    return(exp$geometry)
  }
  # add projection
  # dt2_sf <-  sf::st_make_valid(dt2_sf) # I don't know why wee need this fix
  # sf::st_crs(dt2_sf) <- crs
  # plot(dt2_sf)
  # 
  dt2 <- dt2[,geometry := list(flines(shape_pt_lon,shape_pt_lat)),by = grp][, .SD[1], by = grp ] %>% 
    sf::st_as_sf() %>% sf::st_set_crs(4326)
  
  #dt3$geometry <- sf::st_sf(geometry = geom,crs = 4326)
  # as.Itime
  dt2$departure_time <- data.table::as.ITime(dt2$departure_time)
  dt2$dist <- sf::st_length(dt2$geometry)
  dt2 <- dt2[as.numeric(dt2$dist) > 0,]
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
  
  return(dt2)
}
