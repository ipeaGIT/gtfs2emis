#
# hexagon generation
#
make_hex <- function(resolution, cod_muni,filepath){
  # data prep
  muni <- read_sf(paste0("data-raw/shp/",as.character(cod_muni),".shp"))
  muni <- muni$geometry %>% st_transform(4326)
  # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
  hex_ids <- h3jsr::polyfill(muni, res = resolution, simple = FALSE)
  
  #head(as.vector(unlist(hex_ids[[1]]$h3_polyfillers)))
  #head(hex_ids[1])
  # pass the h3 ids to return the hexagonal grid
  hex_grid <- unlist(hex_ids[[1]]$h3_polyfillers) %>% 
    h3jsr::h3_to_polygon(simple = FALSE) %>%
    #plyr::rename(id_hex = h3_address) %>%
    as_tibble() %>% 
    st_sf()
  
  
  # salvar ------------------------------------------------------------------
  
  # rsolution
  if (nchar(resolution) == 1) res_fim <- paste0("0", resolution) else res_fim <- resolution
  
  # salvar no disco
  readr::write_rds(hex_grid, 
            paste0(filepath,"hex/",cod_muni,"_",res_fim,".rds"))
  
  return(hex_grid)
}



