#
# preprocessing emissions
#
# --
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
options(scipen = 999)
# --
# data import
# --
gtfs <- "gtfs_spo_sptrans_2019-10/"
pre_emi_speed_line <- function(gtfs){
  
  # gtfs <- "gtfs_for_etufor_2019-10/"
  filepath <- paste0("data/emi_speed_line/",gtfs)
  ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
  #
  # preprocessing
  #
  dt <- lapply(seq_along(ids),function(i){ # seq_along(ids)
    sf::read_sf(paste0("data/emi_speed_line/",gtfs,ids[i]))%>% st_transform(31983)
  }) %>% data.table::rbindlist() 
  if("h3_ddrs" %in% colnames(dt)){dt$id_hex <- dt$h3_ddrs; dt <- dt[,-1]}
  
  dt$time <- str_sub(dt$dprtr_t,1,2) %>% as.numeric() %>% +1
  dt <- dt[,emi_co:=sum(emi_co),by=time]
  dt <- dt[,emi_nox:=sum(emi_nox),by=time]
  dt <- dt[,emi_pm:=sum(emi_pm),by=time]
  dt <- dt[,em_nmhc:=sum(em_nmhc),by=time]
  dt <- dt[,emi_co2:=sum(emi_co2),by=time]
  dt <- dt[,.SD[1],by=time]
  dt <- dt[order(time)]
  # write
  sf::write_sf(dt,paste0(filepath,"all.shp"))
  #
  # return
  return(NULL)
}
vec <- c("gtfs_spo_sptrans_2019-10/","gtfs_cur_urbs_2019-10_newfleet/",
         "gtfs_cur_urbs_2019-10/","gtfs_for_etufor_2019-10/")
lapply(vec[1:3],function(i){pre_emi_speed_line(i)})


pre_emi_speed_grid <- function(gtfs){
  filepath <- paste0("data/emi_speed_grid/",gtfs)
  ids <- list.files(path=filepath,pattern = ".shp")
  ids <- ids[-which(ids %in% "all.shp")]
  # --
  # rbinding
  # --
  dd <- lapply(seq_along(ids),function(i){ # seq_along(ids)
    sf::read_sf(paste0("data/emi_speed_grid/",gtfs,ids[i]))%>% st_transform(31983)
  }) %>% data.table::rbindlist()
  if("h3_ddrs" %in% colnames(dd)){dd$id_hex <- dd$h3_ddrs; dd <- dd[,-1]}
  # sum of emission =s
  dd <- dd[,emi_co:=sum(emi_co),by=id_hex]
  dd <- dd[,emi_nox:=sum(emi_nox),by=id_hex]
  dd <- dd[,emi_pm:=sum(emi_pm),by=id_hex]
  dd <- dd[,em_nmhc:=sum(em_nmhc),by=id_hex]
  dd <- dd[,emi_co2:=sum(emi_co2),by=id_hex]
  
  # mudar para tipo de poluente
  dd <- dd[,.SD[1],by=id_hex] %>% sf::st_as_sf()
  # write
  sf::write_sf(dd,paste0(filepath,"all.shp"))
  #
  # return
  return(NULL)
}

dd <- lapply(vec,function(i){pre_emi_speed_grid(i)} )%>% 
  data.table::rbindlist() %>% sf::st_as_sf()
mapview(dd,zcol="emi_ch4")

pre_emi_speed_streets <- function(gtfs){
  
  # gtfs <- "gtfs_for_etufor_2019-10/"
  filepath <- paste0("data/emi_speed_line/",gtfs)
  ids <- list.files(path=filepath,pattern = ".shp")
  ids <- ids[-which(ids %in% "all.shp")]
  #
  # preprocessing
  #
  dt <- lapply(seq_along(ids),function(i){ # seq_along(ids)
    sf::read_sf(paste0("data/emi_speed_line/",gtfs,ids[i])) %>% 
      st_transform(31983) %>%
      st_union()
  }) %>% data.table::rbindlist() 
  if("h3_ddrs" %in% colnames(dt)){dt$id_hex <- dt$h3_ddrs; dt <- dt[,-1]}
  # write
  sf::write_sf(dt,paste0(filepath,"streets.shp"))
  #
  # return
  return(NULL)
}
lapply(vec[1],function(i){pre_emi_speed_streets(i)})
ã  