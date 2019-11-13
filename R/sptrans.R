#
#
#
#
# SPTRANS
#
rm(list=ls())
library(vein)
library(stringr)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
#library(h3jsr)
library(plyr)
library(geobr)
library(tibble)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# data import
filepath <- "L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/"
ids <- list.files(path=filepath);ids_saida <- str_remove(ids,".txt")

# script imports
source("R/read_gps.R")

# emission factor
ub_co <- ef_cetesb(p = "CO", veh = "UB", year = 2019)[1:7]
ub_co_2019 <- ub_co[1]
ef_ub_co <- ef_hdv_scaled(dfcol = ub_co_2019, 
                          v = "Ubus", 
                          t = "Std",
                          g = ">15 & <=18", eu = "V", 
                          gr = 0, l = 0.5, p = "CO")

# hex reading
hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()
#
# loop
i=2
break()
system.time({
future.apply::future_lapply(seq_along(ids),function(i){ # length(ids)
  # --
  # data preparation
  # --
  filepath <- paste0("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/",
                     ids[i])
  dt <- read_gps(filepath) %>% st_as_sf() %>% st_transform(31983) 
  # emissions
  dt$veh <- 1
  #dt$emissions <- dt$veh * ub_co_2019 * dt$dist
  # emissions speed
  dt$emi_speed <- dt$veh * ub_co_2019 * dt$dist * ef_ub_co[[1]](dt$speed)
  # --
  # gridded
  # --
  # intersect
  its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
  colnames(its) <- c("emi_id","hex_id")
  its$hex_id <- hex_grid$h3_address[its$hex_id]
  hex_city <- hex_grid[hex_grid$h3_address %in% unique(its$hex_id),]
  #hex_city$emi <- emis_grid(spobj = dt["emissions"],g = hex_city)$emissions
  hex_city$emi <- emis_grid(spobj = dt["emi_speed"],g = hex_city)$emi_speed
  # salve
  #sf::write_sf(hex_city,paste0("data/emi_grid/",ids_saida[i],".shp"))
  sf::write_sf(hex_city,paste0("data/emi_speed_grid/",ids_saida[i],".shp"))
})
})
