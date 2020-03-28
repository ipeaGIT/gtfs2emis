#
# pos-processing functions
#

hex <- readr::read_rds(paste0("../../data-raw/hex/",proj_cities$abrev_city,"/full09.rds")) %>% 
  data.table::as.data.table() %>% sf::st_as_sf() %>% sf::st_transform(31982)
#
# read emi_lines
#
input_folder <- paste0("../../data/gps_linestring_emis/",proj_cities$abrev_city)
emi_line <- list.files(input_folder,recursive = FALSE,full.names = TRUE)
emi_line_name <- list.files(input_folder,recursive = FALSE,full.names = FALSE) %>% 
  stringr::str_remove_all(".rds")
# loop
emi <- lapply(1:length(emi_line),function(i){ # i = 1
  # message
  message(paste0("shape_id #",emi_line_name[i],", ",i," out of ",length(emi_line)))
  # main
  temp_emi <- readr::read_rds(emi_line[i]) %>% sf::st_as_sf() %>% sf::st_transform(31982) 
  e_grid <- vein::emis_grid(spobj = temp_emi[c("EM_CO","EM_NOx")], g = hex)
  # add extra infor
  e_grid$id <- hex$id_hex
  e_grid$tipo_veiculo <- temp_emi$tipo_de_veiculo[1]
  e_grid$frota_ano <- temp_emi$frota_ano[1]
  e_grid$categoria <- temp_emi$categoria[1]
  # remove unuseful data
  e_grid <- e_grid[as.numeric(e_grid$EM_CO) > 0 & as.numeric(e_grid$EM_NOx) > 0,]
  return(e_grid)
}) %>% data.table::rbindlist()
#readr::write_rds(x = emi,path = "../../data/hex_emi/cur/cur.rds")
emi <- readr::read_rds(path = "../../data/hex_emi/cur/cur.rds")
emi <- emi[,`:=`(EM_CO = sum(EM_CO),EM_NOx = sum(EM_NOx)),by = id][,.SD[1],by = id] %>% sf::st_as_sf()
emi$EM_CO <- emi$EM_CO * units::set_units(sf::st_area(emi),km^2) 
emi$EM_NOx <- emi$EM_NOx * units::set_units(sf::st_area(emi),km^2)
#
# line
#
line <- lapply(1:length(emi_line),function(i){
  temp_emi <- readr::read_rds(emi_line[i])
}) %>% data.table::rbindlist() %>% sf::st_as_sf() %>% sf::st_transform(31982) 
readr::write_rds(x = line,path = "../../data/linestring_emi/cur/cur.rds")

