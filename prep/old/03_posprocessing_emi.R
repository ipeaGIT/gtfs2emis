# shapes_sf <- gtfs_shapes_as_sf(gtfs_data)
# 
# codigo_urbs  <- c(503,202,302,602,"X12","X20","X11",203,303,502,603,200,200,500) %>% as.character()
# expr <- gtfs_data$routes$route_id[which(unique(gtfs_data$routes$route_short_name) 
#                                         %in% 
#                                           codigo_urbs)]
# trips <- gtfs_data$trips$shape_id[which(gtfs_data$trips$route_id %in% expr)] %>% unique()
# --
# data import
# --
#
# merge all speed_line into hourly emissions
#
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
#
# merge all speed_grid into grid emissions
#
pre_emi_speed_grid <- function(gtfs,corr){
  filepath <- paste0("data/emi_speed_grid/",gtfs)
  ids <- list.files(path=filepath,pattern = ".shp")
  ids <- ids[-which(ids %in% "all.shp")] 
  ids <- ids[-which(ids %in% paste0(corr,".shp"))]
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
  # sf::write_sf(dd,paste0(filepath,"all.shp"))
  sf::write_sf(dd,paste0(filepath,"all_corr.shp"))
  #
  # return
  return(NULL)
}
#
# union
#
union_emi_strts <- function(gtfs){
  
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