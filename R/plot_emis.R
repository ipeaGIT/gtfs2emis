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
library(plyr)
library(geobr)
library(tibble)
library(future.apply)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# data import
filepath <- "data/emi_grid//"
ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
break()
# plotting
system.time({
dd <- future.apply::future_lapply(1:length(ids),function(i){
  sf::read_sf(paste0("data/emi_grid/",ids[i]))%>% st_transform(31983)
}) %>% data.table::rbindlist() 
})
#dd %>% data.table::as.data.table() #%>% st_as_sf()
dd <- dd[,emi:=sum(emi),by=h3_ddrs][,.SD[1],by=h3_ddrs] %>% st_as_sf()
sync(mapview(dd["emi"]),mapview(dd$geometry,alpha.regions = 0))
#d1 <- dd[dd$h3_ddrs %in% unique(dd$h3_ddrs),]
dd$emi1 %>% sum()
mapview(dd$geometry[1],color="red",alpha.regions = 0)
mapview(dd["h3_rslt"])
plot(dd["geometry"])
# intersect

lapply(1:10, function(i){sqrt(i)})
