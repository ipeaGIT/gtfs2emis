create_gps_outputs <- function(city_abrev){
  # city_abrev = proj_cities$abrev_city
  # city_abrev 
  message(paste0('Working on city: ', city_abrev,', ', which(proj_cities$abrev_city == city_abrev)),
          ' out of ', nrow(proj_cities))
  
  # get raw gtfs
  f <- list.files("../../data-raw/gtfs/", pattern = city_abrev)
  gtfs_file <- paste0("../../data-raw/gtfs/", f)
  
  # read gtfs to memory
  gtfs <- gtfs2gps::read_gtfs(gtfs_file)
  
  # filter time of the day and weekdays
  gtfs <- gtfs2gps::filter_week_days(gtfs_data = gtfs)
  
  # check valid shapeids
  source("prep/01.02_check_valid_shapeids.R")
  gtfs <- check_valid_shapeid(gtfs_data = gtfs)
  #gtfs <- gtfs2gps::filter_day_period(gtfs, period_start = period_start, period_end = period_end)
  
  # create output directory gtfs 2 gps
  filepath <- paste0("../../data/gps/",city_abrev)
  dir.create(filepath, showWarnings = FALSE)
  
  # convert gtfs to gps-like format
  #gps_df <- gtfs2gps::gtfs2gps(gtfs_data = gtfs,filepath = filepath,continue = TRUE,
  #                             spatial_resolution = 15, progress = TRUE)
  source("../../../gtfs2gps/tests_joao/gtfs2gps_tests.R")
  source("../../../gtfs2gps/tests_joao/mod_updates_test.R")
  gps_df <- gtfs2gps(gtfs_data = gtfs,filepath = filepath,continue = TRUE,parallel = FALSE,
                     spatial_resolution = 15, progress = TRUE)
  gps_df$dt <- stringr::str_sub(gps_df$departure_time,1,2)
  # table(gps_df$dt)
  # gtfs$stop_times %>% dim()
  # gtfs$[shape_id %in% gtfs$shapes$shape_id[1],]
  # gps_sf <- gtfs2gps::gps_as_sf(gtfs)
  
  # save outputs
  return(gps_sf)
  
}
