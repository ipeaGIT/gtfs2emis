#
# ---------------
library(openxlsx)
library(rjson)
library(jsonlite)
library(dplyr)
library(data.table)
library(gtfs2gps)
rm(list=ls())
#
# leitura tipo de frota
#
df <- openxlsx::read.xlsx("../../data-raw/fleet/cur_frota/URBS_plan.xlsx") %>% 
  data.table::as.data.table()
#
# leitura gtfs
#
gtfs <- gtfs2gps::read_gtfs("../../data-raw/gtfs/gtfs_BRA_cur_201910.zip")
# 
# leitura dado de GPS
#
veh_folders = list.files("../../data-raw/gps/cur/",pattern = "veiculos.json.xz",full.names = TRUE)
#
# [1] "../../data-raw/gps/cur//2019_10_09_veiculos.json.xz" "../../data-raw/gps/cur//2019_10_23_veiculos.json.xz"
# [3] "../../data-raw/gps/cur//2019_11_20_veiculos.json.xz"
#
veh_acum <- lapply(1:length(veh_folders),function(i){ # i = 1
  # message
  message(paste0("reading folder ",veh_folders[i]))
  # data intro
  veiculos1 <- jsonlite::stream_in(file(veh_folders[i]))
  #veiculos1 <- data.table::fread("../../data-raw/gps/cur/veiculos.txt")
  veiculos2 <- veiculos1 %>% data.table::as.data.table()
  veiculos3 <- veiculos2[,`:=`(Departure_DTHR = last(DTHR),
                               Arrival_DTHR = first(DTHR)),
                         by = .(COD_LINHA,VEIC)][,.SD[1],by = .(COD_LINHA,VEIC)]
  veiculos3 <- veiculos3[,.(COD_LINHA,VEIC,Departure_DTHR,Arrival_DTHR)]
  # join
  veiculos4 <- df[veiculos3,on = c("Prefixo" = "VEIC")]
  # deal with data
  veiculos4[,`:=`(Departure_DTHR = data.table::as.ITime(substr(Departure_DTHR,12,19)),
                  Arrival_DTHR = data.table::as.ITime(substr(Arrival_DTHR,12,19)))]
  # associate 'shape_id' with 'route_short_name'
  veiculos5 <- veiculos4
  veiculos5[gtfs$routes,on = c('COD_LINHA' = 'route_short_name'),route_id := i.route_id]
  veiculos5[gtfs$trips,on = c('route_id' = 'route_id'),shape_id := i.shape_id]
  veiculos5 <- veiculos5[!is.na(veiculos5$shape_id),]
  #
  return(veiculos5)
}) %>% data.table::rbindlist()

#
#
# write
#
veh_acum1 <- veh_acum[,.SD[1],by = .(Placa,shape_id)]
readr::write_rds(veh_acum1,"../../data/fleet/cur/cur.rds")
