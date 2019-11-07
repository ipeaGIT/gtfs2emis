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
library(h3jsr)
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

# hex reading
hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()

#
# loop

# limits
#a <- seq(1,length(ids),by=1);a1 <- a[-1]-1; a1 <- c(a1,length(ids))
#limits <- lapply(1:length(a), function(i){a[i]:a1[i]})
#break()

system.time({
lapply(1:length(ids),function(i){ # length(a)
  # --
  # data preparation
  # --
  #limits <- a[i]:a1[i]
  filepath <- paste0("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/",
                     ids[i])
  #dt <- fread(filepath)
  dt <- read_gps(filepath) %>% st_as_sf() %>% st_transform(31983) 
  # emissions
  dt$veh <- 1
  dt$emissions <- dt$veh * ub_co_2019 * dt$dist
  # --
  # gridded
  # --
  # intersection
  its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
  colnames(its) <- c("emi_id","hex_id")
  its$hex_id <- hex_grid$h3_address[its$hex_id]
  hex_city <- hex_grid[hex_grid$h3_address %in% unique(its$hex_id),]
  gridded <- emis_grid(spobj = dt["emissns"],g = hex_city)
  # salve
  sf::write_sf(gridded,paste0("data/emi_grid/",ids_saida[i],".shp"))
  #sf::write_sf(dt,paste0("data/emi/",ids_saida[i],".shp"))
})
})
