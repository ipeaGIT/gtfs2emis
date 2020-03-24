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
veh_folders = list.files("../../data-raw/gps/cur/",pattern = "tabelaVeiculo.json.xz",full.names = TRUE)
veh_acum <- lapply(1:length(veh_folders),function(i){ # i = 1
  message(i)
  veic <- jsonlite::fromJSON(file(veh_folders[i])) %>% data.table::as.data.table()
  veic <- veic[,.SD[1],by = .(COD_LINHA,VEICULO)]
  return(veic)
}) %>% data.table::rbindlist()
veh_acum <- veh_acum[,.SD[1],by = .(COD_LINHA,VEICULO)][,.(COD_LINHA,VEICULO)]
#
# join
#
veiculos4 <- df[veh_acum,on = c("Prefixo" = "VEICULO")]
#
# associate 'shape_id' with 'route_short_name'
#
veiculos5 <- veiculos4[-which(is.na(veiculos4$Data)),]
veiculos5[gtfs$routes,on = c('COD_LINHA' = 'route_short_name'),route_id := i.route_id]
veiculos5[gtfs$trips,on = c('route_id' = 'route_id'),shape_id := i.shape_id]
veiculos5 <- veiculos5[!is.na(veiculos5$shape_id),]
#
# write
#
readr::write_rds(veiculos5,"../../data/fleet/cur/cur.rds")
