Sys.setenv(TZ='UTC') # Fuso horario local

# carregar bibliotecas
library(gtfs2gps)
library(data.table)   # manipulacao de dados
library(sf)           # leitura e manipulacao de dados espaciais
library(furrr)
library(purrr)
library(forcats)
library(future.apply) # Aplicar funcoes em paralelo
library(h3jsr) # H3 grade hexagonal
library(dplyr)
library(beepr)
library(vein) # vehicle inventory package
library(sf)
library(units)
library(plyr)
library(tibble)
library(ggplot2)      # visualizacao de dados
library(ggthemes)     # temas para visualizacao de dados
library(mapview)      # visualizacao interativa dos dados
library(geobr)        # dados espaciais do brasil
library(pbapply)      # progress bar
library(patchwork)    # data visuazation
library(readr)        # rapida leitura de dados 
library(tidyr)        # manipulacao de dados
library(osmdata) # Download de dados do OpenStreeteMaps (OSM)
library(openxlsx)     # abre planilha de frota
library(jsonlite)     # download dados de gps

# library(hrbrthemes)
# library(read.dbc)     # leitura de bases relacionais em Microsoft Access
# library(stringr)      # operacoes em strings
# library(lubridate)    # dados em data/horario
# library(fasttime)     # rapido processamento deddados em data/horario
# library(RColorBrewer) # paleta de cores
# library(extrafont)    # fontes de texto
# library(bit.64)       # lidar com numeros em 64bits
# library(knitr)
# library(opentripplanner) # Usar OTP de dentro do R: https://github.com/ITSLeeds/opentripplanner
# library(ggmap) # geocoding
# library(bit64) # viz large numbers
# library(quantreg)
# library(Hmisc) # calcular quantis ponderados

options(scipen=10000)

`%nin%` = Negate(`%in%`)

`%nlike%` = Negate(`%like%`)



# Use GForce Optimisations in data.table operations
# details > https://jangorecki.gitlab.io/data.cube/library/data.table/html/datatable-optimize.html
options(datatable.optimize=Inf)

# set number of threads used in data.table
data.table::setDTthreads(percent = 100)

to_spatial <- function(df1, coordenada = c("lon", "lat")) {
  x <- st_as_sf(df1, coords = coordenada, crs = 4326)
}


rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

