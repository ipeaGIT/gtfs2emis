#
# emiss hex as simple as possible
#
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
#emis_hex <- function(hex_grid,dt){

emi <- list.files(path="data/emi",pattern = ".shp"); trip_ids <- str_remove_all(emi,".shp")
hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()
# reading
dt <-  lapply(1:10,function(i){
  sf::read_sf(paste0("data/emi/",emi[i]))
  }) %>% data.table::rbindlist() %>%  st_as_sf() %>% st_transform(31983) 
# intersection
its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
colnames(its) <- c("emi_id","hex_id")
its$hex_id <- hex_grid$h3_address[its$hex_id]
hex_city <- hex_grid[hex_grid$h3_address %in% unique(its$hex_id),]
#
gridded <- emis_grid(spobj = dt["emissns"],g = hex_city)
mapview(gridded["emissns"])+mapview(dt$geometry)





grid_merge <- lapply(1:length(emi),hex_grid,function(i){
  dt <- sf::read_sf("data/emi/3550308.shp")%>% st_transform(31983) 
  # intersection
  its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
  colnames(its) <- c("emi_id","hex_id")
  its$hex_id <- hex_grid$h3_address[its$hex_id]
  hex_city <- hex_grid[hex_grid$h3_address %in% unique(its$hex_id),]
  #
  gridded <- emis_grid(spobj = dt["emissns"],g = hex_city)
  plot(gridded["emissns"])
})

