data <- c(paste0("2019_10_0",1:9),paste0("2019_10_",10:30))
data <- c(paste0("2019_11_0",1:9),paste0("2019_11_",10:30))
for(i in 1:length(data)){
  cdg_url <- paste0("http://dadosabertos.c3sl.ufpr.br/curitibaurbs/",data[i],"_tabelaVeiculo.json.xz")
  file <- paste0(data[i],"_tabelaVeiculo.json.xz")
  download.file(cdg_url, destfile = paste0("../../data-raw/gps/cur/",file), 
                quiet = FALSE, mode = "w")
  message(paste0("download: ", file))
}
for(i in 1:length(data)){
  cdg_url <- paste0("http://dadosabertos.c3sl.ufpr.br/curitibaurbs/",data[i],"_shapeLinha.json.xz")
  file <- paste0(data[i],"_shapeLinha.json.xz")
  download.file(cdg_url, destfile = paste0("../../data-raw/gps/cur/",file), 
                quiet = FALSE, mode = "w")
  message(paste0("download: ", file))
}


