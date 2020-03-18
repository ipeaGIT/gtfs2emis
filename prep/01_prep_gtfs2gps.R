create_gps_outputs <- function(city_abrev, day_start, day_end){
  
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
  gtfs <- gtfs2gps::filter_day_period(gtfs, period_start = day_start, period_end = day_end)
  
  # create output directory gtfs 2 gps
  filepath <- paste0("../../data/gps/",city_abrev)
  dir.create(filepath, showWarnings = FALSE)
  
  # convert gtfs to gps-like format
  gps_df <- gtfs2gps::gtfs2gps(gtfs_data = gtfs,filepath = filepath,
                               spatial_resolution = 50, progress = F)
  #gps_sf <- gtfs2gps::gps_as_sf(gtfs)
  
  # save outputs
  return(gps_sf)
  
}
