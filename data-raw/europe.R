library(openxlsx)
library(magrittr)
#
# download from 
#
# https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/view
#
filepath <- "https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/at_download/file"

europe <- openxlsx::read.xlsx(xlsxFile = filepath, sheet = 2)
europe <- data.table::as.data.table(europe)
europe <- europe[Category %in% "Buses",]
europe <- europe[Pollutant %in% "PM Exhaust",Pollutant := "PM"]
europe <- europe[Mode %in% "", Mode := NA]
europe <- europe[Technology %in% "", Technology := NA]
europe <- europe[,`EF.[g/km].or.ECF.[MJ/km]`:= NULL]
europe[is.na(Load), Load := 0.5]
europe[is.na(Road.Slope), Road.Slope := 0.0]

usethis::use_data(europe,overwrite = TRUE)
