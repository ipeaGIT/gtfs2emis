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
library(ggplot2)
library(plyr)
library(geobr)
library(tibble)
library(future.apply)
library(ggalt)
library(hrbrthemes)
library(ggnewscale)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# --
# data import
# --
filepath <- "data/emi_speed_grid/gtfs_for_etufor_2019-10/"
ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
break()
# --
# rbinding
# --
system.time({
dd <- future.apply::future_lapply(1:length(ids),function(i){
  sf::read_sf(paste0("data/emi_speed_grid/gtfs_for_etufor_2019-10/",ids[i]))%>% st_transform(31983)
}) %>% data.table::rbindlist() 
})
# sum of emission =s
dd <- dd[,emi:=sum(emi),by=id_hex][,.SD[1],by=id_hex] %>% st_as_sf()
# --
# plot
# inpired on acesso_oportu/IPEA git
# --
ggplot(dd)+
  geom_sf(aes(fill=emi/1000), color = NA, alpha=.7)+
  viridis::scale_fill_viridis( direction = -1)+
  labs(fill = "CO (kg/dia)",
       title = "Emissões de CO da frota \n de ônibus de Fortaleza")

ggsave(file= sprintf("./maps/etufor/hex_emi.jpg"), 
       dpi = 300, width = 14, height = 10, units = "cm")
# ---
# plot 1
# --
ggplot(dd,aes(x="",y=emi/1000))+
  geom_boxplot()+ylab("Emissões por hexágono (kg/dia)")+
  xlab("")

ggsave(file= sprintf("./maps/etufor/boxplot_hex_emi.jpg"), 
       dpi = 300, width = 7.5, height = 10, units = "cm") 
