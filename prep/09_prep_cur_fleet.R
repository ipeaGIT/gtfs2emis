#
# ---------------
library(openxlsx)
library(rjson)
library(jsonlite)
library(dplyr)
library(data.table)
rm(list=ls())
#
# leitura tipo de frota
#
df <- openxlsx::read.xlsx("../../data-raw/fleet/cur_frota/URBS_plan.xlsx") %>% 
  data.table::as.data.table()
# 
# leitura dado de GPS
#
veiculos <- "../../data-raw/gps/cur/2019_11_20_veiculos.json/2019_11_20_veiculos.json"
veiculos1 <- jsonlite::stream_in(file(veiculos))
veiculos2 <- veiculos1 %>% data.table::as.data.table()
veiculos3 <- veiculos2[,`:=`(Departure_DTHR = last(DTHR),
                             Arrival_DTHR = first(DTHR)),
                       by = .(COD_LINHA,VEIC)][,.SD[1],by = .(COD_LINHA,VEIC)]
veiculos3 <- veiculos3[,.(COD_LINHA,VEIC,Departure_DTHR,Arrival_DTHR)]
#
# join
#
veiculos4 <- df[veiculos3,on = c("Prefixo"="VEIC")]
#
# write
#
readr::write_rds(veiculos4,"../../data/fleet/cur/cur.rds")
