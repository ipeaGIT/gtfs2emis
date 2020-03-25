make_hex <- function(resolution, muni){
  #
  # get the unique h3 ids of the hexagons intersecting your polygon at a given resolution
  hex_ids <- h3jsr::polyfill(geometry = muni, res = resolution, simple = FALSE)
  
  
  # pass the h3 ids to return the hexagonal grid
  hex_grid <- unlist(hex_ids$h3_polyfillers) %>% 
    h3jsr::h3_to_polygon(simple = FALSE) %>%
    dplyr::rename(id_hex = h3_address) %>%
    tibble::as_tibble() %>% 
    sf::st_sf()
  
  # salvar ---------------------------------------------------------------------
  
  # rsolution
  if (nchar(resolution) == 1) res_fim <- paste0("0", resolution) else res_fim <- resolution
  
  
  return(hex_grid)
}
nc <- sf::st_read(system.file("shape/nc.shp", package="sf"), quiet = TRUE)
nc1 <- nc[1, ]
nc1$geometry %>% mapview()
fillers <- polyfill(geometry = nc1$geometry, res = 5)
