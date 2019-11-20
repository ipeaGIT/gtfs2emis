#
# hexagon generation
#
rm(list=ls())
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
make_hex <- function(resolution, muni){
  # data prep
  #muni <- read_sf(paste0("data-raw/shp/",as.character(cod_muni),".shp"))
  #muni <- muni$geometry %>% st_transform(4326)
  # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
  hex_ids <- h3jsr::polyfill(geometry = muni, res = resolution, simple = FALSE)
  
  #head(as.vector(unlist(hex_ids[[1]]$h3_polyfillers)))
  #head(hex_ids[1])
  # pass the h3 ids to return the hexagonal grid
  hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
    h3jsr::h3_to_polygon(simple = FALSE) %>%
    dplyr::rename(id_hex = h3_address) %>%
    tibble::as_tibble() %>% 
    sf::st_sf()
  
  
  # salvar ------------------------------------------------------------------
  
  # rsolution
  if (nchar(resolution) == 1) res_fim <- paste0("0", resolution) else res_fim <- resolution
  
  # salvar no disco
  #readr::write_rds(hex_grid, 
  #          paste0(filepath,"hex/",cod_muni,"_",res_fim,".rds"))
  
  return(hex_grid)
}
muni <- geobr::read_municipality(2304400) %>% 
  sf::st_as_sf() %>% 
  sf::st_transform(4326) %>% 
  sf::st_transform(32725) %>% 
  sf::st_buffer(0.003) %>% 
  sf::st_transform(4326)
muni
resolution <- 9
break()
hex_grid <- make_hex(resolution,muni) #%>% readr::write_rds("data/hex/2304400_09.rds")
mapview(muni)+mapview(hex_grid)

