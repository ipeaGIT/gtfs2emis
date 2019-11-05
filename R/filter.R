#
# data analysis sptrans
#
# data import
#filepath <- "L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/"
read_gps <- function(filepath){
  dt <- fread(filepath)
  # stop id
  id0 <- which(!is.na(dt$stop_sequence))
  id1 <- data.table::shift(id0,-1,fill=nrow(dt))
  # exclude single counts
  dtf <- list()
  dtf <- lapply(1:length(id0),function(i){
    list_ids <- id0[i]:id1[i]
    st_sf(dt[list_ids,
             c("trip_id","id","departure_time","stop_id","stop_sequence","dist","speed")][1],
          geometry=st_sfc(st_linestring(x=as.matrix(dt[list_ids,
                                                       c("shape_pt_lon","shape_pt_lat")])),crs=4326))
  }) %>% 
    data.table::rbindlist() 
  
  
  dtf <- dtf[which(as.numeric(st_length(dtf$geometry))>0),]
  dtf <- dtf[-which(as.numeric(st_length(dtf$geometry))==
                     max(as.numeric(st_length(dtf$geometry)))),]
  dtf$dist <- set_units(st_length(dtf$geometry),"km")
  return(dtf)
}



dt <- read_gps("data/gps_spo_sptrans_2019/70912.txt") %>% st_as_sf()
mapview(dt["speed"])
# stop id
