#
# data analysis sptrans
#
# data import
# 
read_gps <- function(filepath){
  
  
  # filepath <-  filepath1
  
  dt <- data.table::fread(filepath)
  # stop id
  id0 <- which(!is.na(dt$stop_sequence)); id0[1] <- 1
  id1 <- data.table::shift(id0,-1,fill=nrow(dt))
  list_ids <- lapply(seq_along(id0),function(i){id0[i]:id1[i]})
  # exclude single counts
  #dtf <- list()
  dtf <- future.apply::future_lapply(list_ids,function(i){
    sf::st_sf(dt[i,
                 c("trip_id","id","departure_time","stop_id","stop_sequence","speed")][1],
              geometry=sf::st_sfc(st_linestring(x=as.matrix(dt[i,
                                                               c("shape_pt_lon","shape_pt_lat")])),crs=4326))
  }) %>% 
    data.table::rbindlist() 

  dtf <- dtf[as.numeric(st_length(geometry))>0,][as.numeric(as.numeric(st_length(geometry)))<
                                                                 max(as.numeric(st_length(geometry))),]

  dtf[,dist := units::set_units(st_length(dtf$geometry),"km")]
  return(dtf)
}
#
#  read gps 2
#

# filepath <-  filepath1

dt <- data.table::fread(paste0(filepath,
                               ids[1]))
# stop id
id0 <- which(!is.na(dt$stop_sequence)); id0[1] <- 1
id1 <- data.table::shift(id0,-1,fill=nrow(dt))
dt <- lapply(seq_along(id0),function(i){
  dt[id0[i]:id1[i],seq:=as.character(i)]}) %>% data.table::rbindlist()

dt[which(length(dt$seq)),]
dt[-1,][,geometry:=sf::st_sfc(sf::st_linestring(x=as.matrix(c(shape_pt_lon,shape_pt_lat))),crs=4326),by=seq]
# exclude single counts
#dtf <- list()
dtf <- future.apply::future_lapply(list_ids,function(i){
  sf::st_sf(dt[i,
               c("trip_id","id","departure_time","stop_id","stop_sequence","speed")][1],
            geometry=sf::st_sfc(st_linestring(x=as.matrix(dt[i,
                                                             c("shape_pt_lon","shape_pt_lat")])),crs=4326))
}) %>% 
  data.table::rbindlist() 

dtf <- dtf[as.numeric(st_length(geometry))>0,][as.numeric(as.numeric(st_length(geometry)))<
                                                 max(as.numeric(st_length(geometry))),]

dtf[,dist := units::set_units(st_length(dtf$geometry),"km")]
