library(openxlsx)
ef <- openxlsx::read.xlsx("../joao_gtfs2emis/test_joao/references/copert/1.A.3.b.i-iv Road transport hot EFs Annex 2018_Dic.xlsx") %>% 
  data.table::as.data.table()
ef <- ef[Category %in% "Buses",]
ef <- ef[Pollutant %in% "PM Exhaust",Pollutant := "PM"]
ef <- ef[Mode %in% "", Mode := NA]
ef <- ef[Technology %in% "", Technology := NA]
ef <- ef[,`EF.[g/km].or.ECF.[MJ/km]`:= NULL]
data.table::fwrite(ef,"data/ef.txt")

ef$Category %>% unique()
ef$Fuel %>% unique()
ef$Segment %>% unique()
ef$Euro.Standard %>% unique()
ef$Technology %>% unique()
ef$Pollutant %>% unique()
ef$Mode %>% unique()
ef$Road.Slope %>% unique()
ef$Load %>% unique()
