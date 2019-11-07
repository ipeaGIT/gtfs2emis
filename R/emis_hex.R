#
# emission grid
#
#reprex({
rm(list=ls())
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission")
library(vein)
library(reprex)
library(stringr)
library(ggplot2)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
library(h3jsr)
library(plyr)
library(geobr)
library(tibble)
emis_hex <- function(hex_grid,dt){

  # reading
  hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()
  dt <- sf::read_sf("data/emi/3550308.shp")%>% st_transform(31983) 
  # intersection
  its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
  colnames(its) <- c("emi_id","hex_id")
  its$hex_id <- hex_grid$h3_address[its$hex_id]
  hex_city <- hex_grid[hex_grid$h3_address %in% unique(its$hex_id),]
  # rebuild vector based on its df
  #its[,,by=emi_id]
  unico <-  unique(its$emi_id) 
  its <- lapply(1:length(unico),function(i){ #length(unico)
    aux <- unico[i]
    its[emi_id == aux,"total_dist"] <- dt$dist[aux]
    its[emi_id == aux,"total_emi"] <-  dt$emissns[aux]
    return(its[emi_id == aux,])
  }) %>% data.table::rbindlist()
  # distance of each 'emi_id' inside the intersected hexagons
  its1 <- its
  its1[,"perc_dist":=]
  its <- lapply(1:nrow(its),function(i){
    its[i,"perc_dist"] <- sf::st_intersection(dt$geometry[its$emi_id[i]],
                                              hex_city[hex_city$h3_address %in% its$hex_id[i],"geometry"])%>% sf::st_length()
    return(its[i,])
  }) %>% data.table::rbindlist()
  # relative emission
  its$rel_dist <- units::set_units(its$perc_dist,"m") / units::set_units(its$total_dist,"km")
  its$rel_emi <- its$total_emi * its$rel_dist
  # hex_grid emissions
  hex_city$emissions  <- lapply(1:nrow(hex_city),function(i){
    sum(its$rel_emi[its$hex_id %in% hex_city$h3_address[i]])
    }) %>% as.data.table() %>% t() %>% as.vector()
  return(hex_city)
}
