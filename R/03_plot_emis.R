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
source("R/09_map_themes.R")
# --
# data import
# --
# gtfs <- "gtfs_spo_sptrans_2019-10/"
# gtfs <- "gtfs_for_etufor_2019-10/"
gtfs <- "gtfs_cur_urbs_2019-10/"
# gtfs <- "gtfs_cur_urbs_2019-10_newfleet/"
filepath <- paste0("data/emi_speed_grid/",gtfs)
ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
#map_tiles <- readr::read_rds("data-raw/map_tiles_ceramic/map_tile_crop_ceramic_spo.rds")
#map_tiles <- readr::read_rds("data-raw/map_tiles_ceramic/map_tile_crop_ceramic_for.rds")
map_tiles <- readr::read_rds("data-raw/map_tiles_ceramic/map_tile_crop_ceramic_cur.rds")
#output_map <- "maps/etufor/"
output_map <- "maps/urbs/"
#output_map <- "maps/sptrans/"
breaksj <- 7
# cidade <- "São Paulo"
# cidade <- "Fortaleza"
cidade <- "Curitiba"
dd <- sf::read_sf(paste0("data/emi_speed_grid/",gtfs,"all.shp")) %>% as.data.table()

#street <- sf::read_sf(paste0("data/emi_speed_line/",gtfs,"streets.shp"))
# --
# tipo de poluente
# --
dd <- dd[order(emi_pm)] 
dd$emi <- dd$emi_pm 
dd <- jenks_natural(dd,"emi",breaksj,round_test = FALSE) %>% sf::st_as_sf()
dd <- sf::st_transform(dd,3857)
pol <- "pm"
dd$emi_jenks <- factor(dd$emi_jenks, levels = unique(dd$emi_jenks))

exp_pol <- c(expression(CO~(kg/dia)),
             expression(CO[2]~(t/dia)),
             expression(CH[4]~(g/dia)),
             expression(NMHC~(kg/dia)),
             expression(NO[x]~(kg/dia)))[3]
# --
# plot
# inpired on acesso_oportu/IPEA git
# --
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

#temp_map
ggsave(temp_map, file= paste0(output_map,pol,"_b",breaksj,"_hex_emi.jpg"), 
       dpi = 300, width = 16.5, height = 12.5, units = "cm")

# 
break()
leafsync::sync(mapview(dd,zcol="emi_nox"),mapview(dd$geometry,alpha.regions=0))
soma <- dd[,c("emi_co","emi_nox","emi_pm","em_nmhc","emi_co2","emi_ch4")] %>% 
  as.data.table() %>% colSums() %>% t() %>% as.data.table()


