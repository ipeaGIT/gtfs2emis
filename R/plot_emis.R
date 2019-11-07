#
# plot analysis 
#
rm(list=ls())
library(vein)
library(stringr)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
library(h3jsr)
library(plyr)
library(geobr)
library(tibble)
library(future.apply)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# data import
filepath <- "data/emi_grid//"
ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()# hex reading
# export couple files
all_hex <- lapply(3:10, function(i){
  # read
  dt <- sf::read_sf(paste0("data/emi/",ids[i]))%>% st_transform(31983)
  # intersect
  its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
  colnames(its) <- c("emi_id","hex_id")
  its$hex_id <- hex_grid$h3_address[its$hex_id]
  hex_city <- hex_grid[hex_grid$h3_address %in% unique(its$hex_id),]
  hex_city$emi <- emis_grid(spobj = dt["emissns"],g = hex_city)$emissns
  # salve
  sf::write_sf(hex_city,paste0("data/emi_grid/",ids[i]))
  return(hex_city)
}) %>% data.table::rbindlist() %>% st_as_sf()
head(all_hex)
#
# plotting
dd <- future.apply::future_lapply(1:2,function(i){
  sf::read_sf(paste0("data/emi_grid/",ids[i]))%>% st_transform(31983)
}) %>% data.table::rbindlist() 
dd
#dd %>% data.table::as.data.table() #%>% st_as_sf()
dd <- dd[,emi:=sum(emi),by=h3_ddrs][,.SD[1],by=h3_ddrs] %>% st_as_sf()
mapview(dd["emi"])
#d1 <- dd[dd$h3_ddrs %in% unique(dd$h3_ddrs),]
dd$emi1 %>% sum()
mapview(dd$geometry)
mapview(dd["h3_rslt"])
plot(dd["geometry"])
# intersect

lapply(1:10, function(i){sqrt(i)})
