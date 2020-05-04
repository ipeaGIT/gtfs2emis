library(openxlsx)
library(magrittr)
#
# download from 
#
# https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/view
#
ef <- openxlsx::read.xlsx("../gtfs2emis/test_joao/references/copert/1.A.3.b.i-iv Road transport hot EFs Annex 2018_Dic.xlsx") %>% 
  data.table::as.data.table()
ef <- ef[Category %in% "Buses",]
ef <- ef[Pollutant %in% "PM Exhaust",Pollutant := "PM"]
ef <- ef[Mode %in% "", Mode := NA]
ef <- ef[Technology %in% "", Technology := NA]
ef <- ef[,`EF.[g/km].or.ECF.[MJ/km]`:= NULL]
data.table::fwrite(ef,"data/ef.txt")

usethis::use_data(ef,overwrite = TRUE)
