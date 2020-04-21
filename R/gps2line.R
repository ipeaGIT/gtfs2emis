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
#' library(gtfs2gps)
#' library(gtfs2emis)
#' library(dplyr)
#' 
#' poa <- read_gtfs(system.file("extdata/poa.zip", package="gtfs2gps"))
#' subset <- filter_by_shape_id(poa, "T2-1") %>% filter_single_trip()
#' 
#' poa_gps <- gtfs2gps(subset)

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
  
  # first change
  dt1 <- dt[,.SD[1],by = .(trip_id,interval_id)]   # O que essa linha faz exatamente?
  
  dt1 <- data.table::setcolorder(dt1,names(dt))
  dt1 <- dt1[,c("id","interval_id") := list(id - 0.1,interval_id - 1)]
  dt2 <- data.table::rbindlist(list(dt,dt1))[order(id)]
  
  # shape
  dt3 <- dt2[, .SD[1], by = .(interval_id,trip_id) ]
  
  # create unique id for each unique combinarion of interval_id & trip_id
  dt2[, grp := .GRP, by = .(interval_id,trip_id) ]
  
  
#### Tentativas com sfheaders
  
  # convert to multilnestring
  dt2_sf <-  sfheaders::sfc_multilinestring( obj = dt2, linestring_id = 'interval_id', multilinestring_id = 'trip_id', x='shape_pt_lon', y='shape_pt_lat')
 
  
  dt2_sf <-  sfheaders::sfc_linestring( obj = dt2, linestring_id = 'interval_id',  x='shape_pt_lon', y='shape_pt_lat')
  
  dt2_sf <-  sfheaders::sfg_multilinestring( obj = dt2, linestring_id = 'grp',  x='shape_pt_lon', y='shape_pt_lat')

  
  
 # add projection
  dt2_sf <-  sf::st_make_valid(dt2_sf) # I don't know why wee need this fix
  sf::st_crs(dt2_sf) <- crs
  plot(dt2_sf)
 
 
  geom <- dt2[,{
    geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2)) %>%
      sf::st_sfc() %>% sf::st_sf()
  },by = .(interval_id,trip_id)][,"geometry"]
  
  
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
