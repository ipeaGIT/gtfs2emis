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

#
# fleet cur
#
tempd <- file.path(tempdir(), "fleet") # create tempr dir to save GTFS unzipped files
unlink(normalizePath(paste0(tempd, "/", dir(tempd)), mustWork = FALSE), recursive = TRUE) # clean tempfiles in that dir
utils::untar(tarfile = "inst/extdata/cur_fleet.tar.xz",exdir = tempd) # untar files
# how to read later on
unzippedfiles <- list.files(tempd) # list of unzipped files
cur_fleet <- data.table::fread(paste0(tempd,"/cur_fleet.txt"))
#
# gtfs2gps::cur
#
gtfs <- gtfs2gps::read_gtfs("../../data-raw/gtfs/gtfs_BRA_cur_201910.zip")
gtfs <- gtfs2gps::filter_week_days(gtfs_data = gtfs)
gtfs <- check_valid_shapeid(gtfs)
gtfs <- gtfs2gps::filter_by_shape_id(gtfs,unique(gtfs$shapes$shape_id)[1:40])
gtfs2gps::write_gtfs(gtfs,zipfile = "inst/extdata/gtfs_cur.zip")
gtfs1 <- gtfs2gps::read_gtfs("inst/extdata/gtfs_cur.zip")
object.size(gtfs1)/10^6
