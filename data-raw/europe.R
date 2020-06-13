library(openxlsx)
library(magrittr)
#
# download from 
#
# https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/view
#
europe <- openxlsx::read.xlsx("../gtfs2emis/test_joao/references/copert/1.A.3.b.i-iv Road transport hot EFs Annex 2018_Dic.xlsx") %>% 
  data.table::as.data.table()
europe <- europe[Category %in% "Buses",]
europe <- europe[Pollutant %in% "PM Exhaust",Pollutant := "PM"]
europe <- europe[Mode %in% "", Mode := NA]
europe <- europe[Technology %in% "", Technology := NA]
europe <- europe[,`EF.[g/km].or.ECF.[MJ/km]`:= NULL]
#data.table::fwrite(europe,"data/europe.txt")

usethis::use_data(europe,overwrite = TRUE)
