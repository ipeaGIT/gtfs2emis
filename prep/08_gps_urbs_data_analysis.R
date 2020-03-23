"../../data-raw/gps/cur/"

linhas <- jsonlite::fromJSON(file("../../data-raw/gps/cur/2019_11_22_linhas.json.xz"))
head(linhas)
linhas$COD %>% length()
pois <- jsonlite::fromJSON(file("../../data-raw/gps/cur/2019_11_22_pois.json.xz"))
head(pois)
shapeLinha <- jsonlite::fromJSON(file("../../data-raw/gps/cur/2019_11_22_shapeLinha.json.xz"))
head(shapeLinha)
unique(shapeLinha$COD) %>% length()
tabelaLinha <- jsonlite::fromJSON(file("../../data-raw/gps/cur/2019_11_22_tabelaLinha.json.xz"))
head(tabelaLinha)
unique(tabelaLinha$COD) %>% length()
tabelaVeiculo <- jsonlite::fromJSON(file("../../data-raw/gps/cur/2019_11_22_tabelaVeiculo.json.xz")) %>% as.data.table()
head(tabelaVeiculo)
tabelaVeiculo[,.SD[1],by = .(VEICULO,COD_LINHA)]
tabelaVeiculo[VEICULO %in% "KB499",]
tabelaVeiculo[COD_LINHA %in% 509,]
trechosItinerarios <- jsonlite::fromJSON(file("../../data-raw/gps/cur/2019_11_22_trechosItinerarios.json.xz"))
trechosItinerarios

