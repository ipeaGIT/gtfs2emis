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
gtfs <- "gtfs_cur_urbs_2019-10/"
filepath <- paste0("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/",gtfs)
ids <- list.files(path=filepath);ids_saida <- str_remove(ids,".txt")
# hex reading
hex_grid <- readRDS("data/hex/4106902_09.rds") %>% st_transform(31983) %>% st_sf()
# script imports
source("R/01_read_gps.R")

# emission factor
ub_co <- ef_cetesb(p = "CO", veh = "UB", year = 2017)[1]
ub_nox <- ef_cetesb(p = "NOx", veh = "UB", year = 2017)[1]
ub_pm <- ef_cetesb(p = "PM", veh = "UB", year = 2017)[1]
ub_nmhc <- ef_cetesb(p = "NMHC", veh = "UB", year = 2017)[1]
ub_co2 <- ef_cetesb(p = "CO2", veh = "UB", year = 2017)[1]
ub_ch4 <- ef_cetesb(p = "CH4", veh = "UB", year = 2017)[1]

# scaled emission factor
ef_ub_co <- ef_hdv_scaled(dfcol = ub_co, 
                          v = "Ubus", 
                          t = "Std",
                          g = ">15 & <=18", eu = "V", 
                          gr = 0, l = 0.5, p = "CO")
ef_ub_nox <- ef_hdv_scaled(dfcol = ub_nox, 
                           v = "Ubus", 
                           t = "Std",
                           g = ">15 & <=18", eu = "V", 
                           gr = 0, l = 0.5, p = "NOx")
ef_ub_pm <- ef_hdv_scaled(dfcol = ub_pm, 
                          v = "Ubus", 
                          t = "Std",
                          g = ">15 & <=18", eu = "V", 
                          gr = 0, l = 0.5, p = "PM")
ef_ub_nmhc <- ef_hdv_scaled(dfcol = ub_nmhc, 
                            v = "Ubus", 
                            t = "Std",
                            g = ">15 & <=18", eu = "V", 
                            gr = 0, l = 0.5, p = "NMHC")
ef_ub_co2 <- ef_hdv_scaled(dfcol = ub_co2, 
                           v = "Ubus", 
                           t = "Std",
                           g = ">15 & <=18", eu = "V", 
                           gr = 0, l = 0.5, p = "CO2")
ef_ub_ch4 <- vein::ef_hdv_scaled(dfcol = ub_ch4, 
                                 v = "Ubus", 
                                 t = "Std",
                                 g = ">15 & <=18", eu = "V", 
                                 gr = 0, l = 0.5, p = "CH4")

#plot(hex_grid$geometry)
#
# loop
break()
idsok <- list.files(path="data/emi_speed_grid/gtfs_cur_urbs_2019-10_newfleet/",pattern = ".shp") %>% 
  str_remove(".shp")
ids <- ids_saida[-which(ids_saida %in% idsok)]
system.time({
  future.apply::future_lapply(seq_along(ids),function(i){ # seq_along(ids)
    # --
    # data preparation
    # --
    filepath1 <- paste0(filepath,
                        ids[i],".txt")
    dt <- data.table::fread(filepath1)
    dt <- read_gps(filepath1) %>% st_as_sf() %>% st_transform(31983) 
    # emissions
    dt$veh <- 1
    # emissions speed
    dt$emi_co <- dt$veh * ub_co * dt$dist * ef_ub_co[[1]](dt$speed)
    dt$emi_nox <- dt$veh * ub_nox * dt$dist * ef_ub_nox[[1]](dt$speed)
    dt$emi_pm <- dt$veh * ub_pm * dt$dist * ef_ub_pm[[1]](dt$speed)
    dt$emi_nmhc <- dt$veh * ub_nmhc * dt$dist * ef_ub_nmhc[[1]](dt$speed)
    dt$emi_co2 <- dt$veh * ub_co2 * dt$dist * ef_ub_co2[[1]](dt$speed)
    dt$emi_ch4 <- dt$veh * ub_ch4 * dt$dist * ef_ub_ch4[[1]](dt$speed)
    # --
    # gridded
    # --
    # intersect
    its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
    colnames(its) <- c("emi_id","hex_id")
    its$hex_id <- hex_grid$id_hex[its$hex_id]
    hex_city <- hex_grid[hex_grid$id_hex %in% unique(its$hex_id),]
    # emis grid
    hex_city$emi_co <- emis_grid(spobj = dt["emi_co"],g = hex_city)$emi_co
    hex_city$emi_nox <- emis_grid(spobj = dt["emi_nox"],g = hex_city)$emi_nox
    hex_city$emi_pm <- emis_grid(spobj = dt["emi_pm"],g = hex_city)$emi_pm
    hex_city$emi_nmhc <- emis_grid(spobj = dt["emi_nmhc"],g = hex_city)$emi_nmhc
    hex_city$emi_co2 <- emis_grid(spobj = dt["emi_co2"],g = hex_city)$emi_co2
    hex_city$emi_ch4 <- emis_grid(spobj = dt["emi_ch4"],g = hex_city)$emi_ch4
    # salve
    sf::write_sf(hex_city,paste0("data/emi_speed_grid/gtfs_cur_urbs_2019-10_newfleet/",ids[i],".shp"))
    sf::write_sf(dt,paste0("data/emi_speed_line/gtfs_cur_urbs_2019-10_newfleet/",ids[i],".shp"))
  })
})

