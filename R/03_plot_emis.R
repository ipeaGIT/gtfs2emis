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
library(stringr)
library(stringi)
library(ggplot2)
library(viridis)
library(data.table)
library(BAMMtools) # fast calculation of jenks natural breaks
#library(ggt)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# create Natural Jenks function
source("R/09_jenks_breaks.R")
   
# --
# data import
# --
gtfs <- "gtfs_spo_sptrans_2019-10/"
#gtfs <- "gtfs_for_etufor_2019-10/"
filepath <- paste0("data/emi_speed_grid/",gtfs)
ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
map_tiles <- readr::read_rds("data-raw/map_tiles/map_tile_crop_spo.rds")
#map_tiles <- readr::read_rds("data-raw/map_tiles/map_tile_crop_for.rds")
#output_map <- "maps/etufor/"
output_map <- "maps/sptrans/"
breaksj <- 7
#cidade <- "S?o Paulo"
cidade <- "São Paulo"
#cidade <- "Fortaleza"
break()
# --
# rbinding
# --
dd <- sf::read_sf(paste0("data/emi_speed_grid/  ",gtfs,ids[i]))
dd <- lapply(seq_along(ids),function(i){ # seq_along(ids)
  sf::read_sf(paste0("data/emi_speed_grid/",gtfs,ids[i]))%>% st_transform(31983)
}) %>% data.table::rbindlist() 
if("h3_ddrs" %in% colnames(dd)){dd$id_hex <- dd$h3_ddrs; dd <- dd[,-1]}
# sum of emission =s
dd <- dd[,emi_co:=sum(emi_co)/1000,by=id_hex]
dd <- dd[,emi_nox:=sum(emi_nox)/1000,,by=id_hex]
dd <- dd[,emi_pm:=sum(emi_pm)/1000,,by=id_hex]
dd <- dd[,em_nmhc:=sum(em_nmhc)/1000,,by=id_hex]
dd <- dd[,emi_co2:=sum(emi_co2)/1000000,,by=id_hex]

# mudar para tipo de poluente
dd <- dd[order(emi_co2)] 
dd <- jenks_natural(dd,"emi_co2",breaksj) %>% sf::st_as_sf()
dd <- sf::st_transform(dd,3857)
dd$emi_jenks <- factor(dd$emi_co2_jenks, levels = unique(dd$emi_co2_jenks))
pol <- "co2"
exp_pol <- c(expression(CO~(kg/dia)),
             expression(CO[2]~(t/dia)),
             expression(CH[4]~(kg/dia)),
             expression(NMHC~(kg/dia)),
             expression(NO[x]~(kg/dia)))[2]
# --
# plot
# inpired on acesso_oportu/IPEA git
# --
break()
theme_for_TMI <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "right",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.size = unit(5,"line"),
      legend.key.width = unit(1,"line"),
      legend.key.height = unit(0.35,"cm"),
      legend.spacing.y = unit(0.5, "cm"),
      legend.text=element_text(size=rel(0.85)),
      legend.title=element_text(size=rel(1)),
      plot.title = element_text(hjust = 0, vjust = 0),
      strip.text = element_text(size = 6),
      #legend.spacing.y = unit(2.0, "cm"),
      # legend.key.height=unit(0.5,"cm")
      
    )
}  
temp_map <- ggplot()+
  geom_raster(data = map_tiles, aes(x, y,fill=hex), alpha = 1) +
  coord_equal() +
  scale_fill_identity() +
  # nova escala
  ggnewscale::new_scale_fill() +
  geom_sf(data = dd, aes(fill = factor(emi_jenks)), color = NA, alpha=.7)  +
  viridis::scale_fill_viridis(discrete = T,
                              direction = -1) +
  labs(fill = exp_pol,
       title = cidade)+
  theme_for_TMI()+
  theme(plot.title = element_text(hjust = 0.5,size=15))


ggsave(temp_map, file= paste0(output_map,pol,"_b",breaksj,"_hex_emi.jpg"), 
       dpi = 300, width = 16.5, height = 16.5, units = "cm")

# 
break()
leafsync::sync(mapview(dd,zcol="emi_nox"),mapview(dd$geometry,alpha.regions=0))
soma <- dd[,c("emi_co","emi_nox","emi_pm","em_nmhc","emi_co2","emi_ch4")] %>% as.data.table() 
soma <- soma[,-7] %>% colSums()
soma/1000
View(t(soma/1000) %>% round(2))
