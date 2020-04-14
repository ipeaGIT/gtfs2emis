#' @title Convert GTFS to GPS-like data given a spatial resolution
#' 
#' @description Convert GTFS data to GPS format by sampling points using a spatial resolution. The
#' function uses functions from gtfs2gps package, than read gtfs files, 
#' filter by week day (default option), by a fixed spatial resolution of 15 m.
#' 
#' @param raw_gtfs Character; a path to a GTFS file to be converted to GPS
#' @param filepath Character; output file path.
#' @param filter_weekdays Logical; When TRUE (default) is filter the gtfs file, removing the
#'  trips operating only saturday or sunday.
#'   
#' @export
gtfs2gps <- function(raw_gtfs,filepath,filter_weekdays = TRUE){
  # read gtfs to memory
  gtfs <- gtfs2gps::read_gtfs(raw_gtfs)
  
  # filter time of the day and weekdays
  if(filter_weekdays == TRUE){  gtfs <- gtfs2gps::filter_week_days(gtfs_data = gtfs) }
  
  # check valid shapeids
  gtfs <- check_valid_shapeid(gtfs_data = gtfs)
  
  #gtfs <- gtfs2gps::filter_day_period(gtfs, period_start = period_start, period_end = period_end)
  
  # create output directory gtfs 2 gps
  dir.create(filepath, showWarnings = FALSE)
  
  gps_df <- gtfs2gps::gtfs2gps(gtfs_data = gtfs,filepath = filepath,parallel = FALSE,
                     spatial_resolution = 15, progress = TRUE)
  gps_df$dt <- substr(gps_df$dt,1,2)
  # table(gps_df$dt)
  # gtfs$stop_times %>% dim()
  # gtfs$[shape_id %in% gtfs$shapes$shape_id[1],]
  # gps_sf <- gtfs2gps::gps_as_sf(gtfs)
  
  # save outputs
  return(NULL)
  
}
