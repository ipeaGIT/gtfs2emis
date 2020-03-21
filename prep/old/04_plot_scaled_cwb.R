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
require(gridExtra)
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
gtfs <- "/gtfs_cur_urbs_2019-10/"
filepath <- paste0("data/emi_speed_grid/",gtfs)
dd <- sf::read_sf(paste0("data/emi_speed_grid/",gtfs,"all1.shp")) %>% as.data.table()
d_2017 <- sf::read_sf(paste0("data/emi_speed_grid/gtfs_cur_urbs_2019-10_newfleet/all.shp")) %>% as.data.table()
d_nocorr <- sf::read_sf(paste0("data/emi_speed_grid/",gtfs,"no_corr.shp")) %>% as.data.table()
map_tiles <- readr::read_rds("data-raw/map_tiles_ceramic/map_tile_crop_ceramic_cur.rds")
output_map <- "maps/urbs_corredor/"
breaksj <- 7

cidade <- "Frota Ano 2009 \n Emissões totais"
cidade1 <- "Frota  Ano 2017 \n Emissões totais"
cidade2 <- "Frota  Ano 2009 \n Veículos elétricos"
# --
# tipo de poluente
# --
dd <- dd[order(emi_co2)] 
d_2017 <- d_2017[order(emi_co2)] 
d_nocorr <- d_nocorr[order(emi_co2)] 

dd$emi <- dd$emi_co2 / 10^6
d_2017$emi <- d_2017$emi_co2 / 10^6
d_nocorr$emi <- d_nocorr$emi_co2 / 10^6

pol <- "co2"
exp_pol <- c(expression(CO~(kg/dia)),
             expression(CO[2]~(t/dia)),
             expression(CH[4]~(g/dia)),
             expression(NMHC~(kg/dia)),
             expression(MP~(g/dia)),
             expression(NO[x]~(kg/dia)))[2]

#dd <- jenks_natural(dd,"emi",breaksj,round_test = TRUE) %>% sf::st_as_sf()
dd <- sf::st_as_sf(dd) %>% sf::st_transform(3857)
d_2017 <-  sf::st_as_sf(d_2017) %>% sf::st_transform(3857)
d_nocorr <-  sf::st_as_sf(d_nocorr) %>% sf::st_transform(3857)
#dd$emi_jenks <- factor(dd$emi_jenks, levels = unique(dd$emi_jenks))
# --
# plot
# --
 p1 <- ggplot()+
   geom_raster(data = map_tiles, aes(x, y,fill=hex), alpha = 1) +
   coord_equal() +
   scale_fill_identity() +
  # # nova escala
   ggnewscale::new_scale_fill() +
  geom_sf(data = dd, aes(fill = emi), color = NA, alpha=.7)  +
   scale_fill_gradientn(colours = rev(cpt()),
                        limits=c(1.0*min(dd$emi),1.0*max(dd$emi)),
                        guide=FALSE)+
  #scale_fill_gradientn(colours = rev(cpt()),
  #                      limits=c(1.0*min(dd$emi),1.0*max(dd$emi)),
  #                      breaks=seq(min(dd$emi),1.0*max(dd$emi),length.out = 6),
  #                      labels = seq(min(dd$emi),1.0*max(dd$emi),length.out = 6) %>% round(1),
  #                      guide = guide_colourbar(barheight = 20,
  #                                              frame.colour = "black"))+
  labs(fill = exp_pol,
       title = cidade)+
  theme_for_TMI()+
  theme(plot.title = element_text(hjust = 0.5,size=10))
 

 p2 <- ggplot()+
    geom_raster(data = map_tiles, aes(x, y,fill=hex), alpha = 1) +
    coord_equal() +
    scale_fill_identity() +
   # # nova escala
    ggnewscale::new_scale_fill() +
   geom_sf(data = d_2017, aes(fill = emi), color = NA, alpha=.7)  +
   scale_fill_gradientn(colours = rev(cpt()),
                        limits=c(1.0*min(dd$emi),1.0*max(dd$emi)),
                        guide=FALSE)+
  #scale_fill_gradientn(colours = rev(cpt()),
  #                      limits=c(1.0*min(dd$emi),1.0*max(dd$emi)),
  #                      breaks=seq(min(dd$emi),1.0*max(dd$emi),length.out = 6),
  #                      labels = seq(min(dd$emi),1.0*max(dd$emi),length.out = 6) %>% round(1),
  #                      guide = guide_colourbar(barheight = 20,
  #                                              frame.colour = "black"))+
   labs(fill = exp_pol,
        title = cidade1)+
   theme_for_TMI()+
   theme(plot.title = element_text(hjust = 0.5,size=10))
 

 p3 <- ggplot()+
   geom_raster(data = map_tiles, aes(x, y,fill=hex), alpha = 1) +
   coord_equal() +
   scale_fill_identity() +
   # nova escala
   ggnewscale::new_scale_fill() +
   geom_sf(data = d_nocorr, aes(fill = emi), color = NA, alpha=.7)  +
   scale_fill_gradientn(colours = rev(cpt()),
                        limits=c(1.0*min(dd$emi),1.0*max(dd$emi)),
                                             breaks=seq(min(dd$emi),1.0*max(dd$emi),length.out = 6),
                                             labels = seq(min(dd$emi),1.0*max(dd$emi),length.out = 6) %>% round(1),
                                             guide = guide_colourbar(barheight = 12,
                                                                     frame.colour = "black"))+
   labs(fill = exp_pol,
        title = cidade2)+
   #theme_bw()+
   theme_for_TMI()+
   theme(plot.title = element_text(hjust = 0.5,size=10))


 pf <- grid.arrange(p1,p2,p3,ncol=3)

ggsave(plot = pf,filename = "maps/urbs_corredor/all.jpg",
       width = 20,height = 10,units = "cm",scale = 1.2,dpi = 300)
