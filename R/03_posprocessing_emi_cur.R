devtools::install_github("ipeaGIT/gtfs2gps")
rm(list=ls())
library(gtfs2gps)
#devtools::load_all('.')

gtfszip = "L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/data_gtfs/gtfs_cur_urbs_2019-10.zip"
filepath = "tests_joao/data/output/gtfs_cur_urbs_2019-10/"
gtfs <- "gtfs_cur_urbs_2019-10/"
week_days = F
progress = TRUE
spatial_resolution = 200

# Read GTFS data
gtfs_data <- read_gtfs(gtfszip = gtfszip)

# Filter trips
if(week_days){
  gtfs_data <- filter_week_days(gtfs_data) 
}

# Convert all shapes into sf objects
shapes_sf <- gtfs_shapes_as_sf(gtfs_data)

codigo_urbs  <- c(503,202,302,602,"X12","X20","X11",203,303,502,603,200,200,500) %>% as.character()
expr <- gtfs_data$routes$route_id[which(unique(gtfs_data$routes$route_short_name) 
                                        %in% 
                                          codigo_urbs)]
trips <- gtfs_data$trips$shape_id[which(gtfs_data$trips$route_id %in% expr)] %>% unique()

break()
pre_emi_speed_grid <- function(gtfs,trips){
  filepath <- paste0("data/emi_speed_grid/",gtfs)
  ids <- list.files(path=filepath,pattern = ".shp")
  #ids <- ids[-which(ids %in% paste0(c("all","all1","all_corr","no_corr"),".shp"))] 
  #ids <- ids[which(ids %in% paste0(trips,".shp"))]
   #ids <- ids[-which(ids %in% paste0(trips,".shp"))]
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
  # sf::write_sf(dd,paste0(filepath,"no_corr.shp"))
  # sf::write_sf(dd,paste0(filepath,"no_corr.shp"))
  #
  # return
  return(NULL)
}

gtfs <- "gtfs_cur_urbs_2019-10_newfleet/"
pre_emi_speed_grid(gtfs,trips)
