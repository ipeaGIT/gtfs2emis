#
# ---------------
rm(list=ls())
#
# leitura tipo de frota
#
df <- openxlsx::read.xlsx("../../data-raw/fleet/cur_frota/URBS_plan.xlsx") %>% 
  data.table::as.data.table()
# 
# leitura dado de GPS (tabelaVeiculo)
#
veh_folders = list.files("../../data-raw/gps/cur/",pattern = "tabelaVeiculo.json.xz",full.names = TRUE)
veh_acum <- lapply(1:length(veh_folders),function(i){ # i = 33
  message(i)
  veic <- jsonlite::fromJSON(file(veh_folders[i])) %>% data.table::as.data.table()
  veic <- veic[,.SD[1],by = .(COD_LINHA,VEICULO)]
  return(veic)
}) %>% data.table::rbindlist()
veh_acum1 <- veh_acum[,.SD[1],by = .(COD_LINHA,VEICULO)][,.(COD_LINHA,VEICULO)]
# 
# leitura dado de GPS (shapeLinha)
#
shp_folders = list.files("../../data-raw/gps/cur/",pattern = "shapeLinha.json.xz",full.names = TRUE)
shp_acum <- lapply(1:length(shp_folders),function(i){ # i = 1
  message(i)
  shp <- jsonlite::fromJSON(file(shp_folders[i])) %>% data.table::as.data.table()
  shp <- shp[,.SD[1],by = .(SHP,COD)]
  return(shp)
}) %>% data.table::rbindlist()
shp_acum1 <- shp_acum[,.SD[1],by = .(SHP,COD)][,.(SHP,COD)]
#
# join
#
veh <- lapply(1:max(table(shp_acum1$COD)),function(i){ # i = 8
  aux_shp <- shp_acum1[,.SD[i],by = COD][!is.na(SHP),]
  la <- veh_acum1[aux_shp,on = c("COD_LINHA" = "COD"),SHP := i.SHP][!is.na(SHP),][VEICULO != "",]
  return(la)
}) %>% data.table::rbindlist()
df3 <- veh[df,on = c("VEICULO" = "Prefixo")][!is.na(COD_LINHA),][,.SD[1],by = .(COD_LINHA,VEICULO,SHP)]
#
# write
#
readr::write_rds(df3,"../../data/fleet/cur/cur.rds")
