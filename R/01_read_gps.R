#
# data analysis sptrans
#
# data import
# 
#
read_gps <- function(dt){
  dt[,id := 1:nrow(dt)]
  # --
  # trip division
  # --
  mdist1 <- which(dt$cumdist == max(dt$cumdist))
  mdist0 <- c(1,head(mdist1,-1)) 
  list_dist <- lapply(seq_along(mdist0), function(i){data.table(range = i,id = mdist0[i]:mdist1[i])}) %>%
    data.table::rbindlist()
  dt[list_dist, on= "id",range_trip := i.range]  # add range
  # --
  # stop division
  # --
  id0 <- c(1,which(!is.na(dt$stop_sequence))) 
  id1 <- c(id0[-1],nrow(dt))
  list_ids <- lapply(seq_along(id0),function(i){data.table(range = i,id = id0[i]:id1[i])}) %>% 
    rbindlist()
  dt[list_ids,on= "id",range_id := i.range]  # add range
  # --
  # first change
  # --
  dt1 <- dt[,.SD[1],by=.(range_trip,range_id)]
  setcolorder(dt1,names(dt))
  dt1[,c("range_id","id"):= list(range_id-1,id-0.1)][-c(1,.N),]
  dt <- rbindlist(list(dt,dt1))[order(id)]
  # shape
  dt2 <- dt[,.SD[1],by = .(range_trip,range_id)]
  geom <- dt[,{
    geometry <- sf::st_linestring(x=matrix(c(shape_pt_lon,shape_pt_lat),ncol=2)) %>% 
      sf::st_sfc() %>% sf::st_sf()
  },by = .(range_trip,range_id)][,"geometry"]
  dt2$geometry <- sf::st_sf(geometry = geom,crs=4326)
  return(dt2)
  
}
