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
options(scipen = 999)
# --
# data import
# --
 gtfs <- "gtfs_spo_sptrans_2019-10/"
# gtfs <- "gtfs_for_etufor_2019-10/"
filepath <- paste0("data/emi_speed_line/",gtfs)
ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
#output_map <- "maps/etufor/"
output_map <- "maps/sptrans/"
breaksj <- 7
cidade <- "Curitiba"
#cidade <- "São Paulo"
#cidade <- "Fortaleza"
dt <- sf::read_sf(paste0("data/emi_speed_line/",gtfs,"all.shp"))
street <- sf::read_sf(paste0("data/emi_speed_line/",gtfs,"streets.shp"))
# --
# define pollutant
# --
pol <- "CO2"
exp_pol <- c(expression(CO~(kg/dia)),
             expression(CO[2]~(10^3 * t/dia)),
             expression(CH[4]~(kg/dia)),
             expression(NMHC~(kg/dia)),
             expression(MP~(kg/dia)),
             expression(NO[x]~(kg/dia)))[2]
dt$emi <- dt$emi_co2
# --
ggplot(dt,aes(x = time, y = emi))+
  geom_bar(aes(fill=as.numeric(emi)),stat="identity",colour="black", size=.3, alpha=.8)+
  labs(x = "Hora", y = exp_pol)+
  #(guide=FALSE,colours = rev(cpt()))+
  scale_fill_gradientn(guide=FALSE,
                       colours = rev(cpt()))+
  scale_y_continuous(breaks=seq(0,1.1*max(dt$emi),length.out=6),
                     labels=seq(0,1.1*max(dt$emi),length.out=6) %>% round())+
  scale_x_continuous(breaks=seq(1,24,by=2),
                     labels=seq(1,24,by=2))+
  theme(axis.text.x = element_text(angle=0, hjust=0.5, vjust=1))#+
  #theme(legend.position = element_blank())+
  theme(text=element_text(size=12, family="LM Roman 10"))

ggsave(paste0(output_map,pol,"_horario.jpg"),
       width = 12,height = 6,units = "cm",scale = 1.2,dpi = 300)
