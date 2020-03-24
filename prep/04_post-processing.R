#
# pos-processing functions
#
dat <- sf::read_sf("../../../master-thesis-repo/dados/Pesquisa_OD_IPPUC/arquivos_saida/shp/estendida/amostras_por_zona.shp") %>% 
  sf::st_set_crs(31982) %>% filter(MUNICIPIO == "CURITIBA") %>% sf::st_buffer(5) %>% sf::st_union()

# produce hexagons
source("prep/01.2_prep_hex.R")
hex <- make_hex(resolution = 09,dat)

#
# read emi_lines
#
input_folder <- paste0("../../data/gps_linestring_emis/",proj_cities$abrev_city)
emi_line <- list.files(input_folder,recursive = FALSE,full.names = TRUE)
emi_line_name <- list.files(input_folder[1],recursive = FALSE,full.names = FALSE) %>% 
   stringr::str_remove_all(".rds")
temp_emi <- lapply(emi_line,function(i){
  temp_emi <- readr::read_rds(i)
}) %>% data.table::rbindlist() %>% sf::st_as_sf()
#
# first plot
# 


ggplot() +
  geom_sf(data = dat, colour = "black", fill = NA, size = 0.3)
  geom_sf(data = temp_emi,aes(color = "shape_id"))
