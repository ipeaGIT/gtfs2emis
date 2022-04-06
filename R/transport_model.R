#' @title Urban bus transport model 
#' 
#' @description Creates the transport model based on the input GTFS.
#' 
#' @param gtfs_data  A path to a GTFS file to be converted to GPS, or a GTFS data
#' represented as a list of data.tables.
#' @param spatial_resolution Numeric. The spatial resolution in meters.
#' @param gps_raw_path Character. Filepath to receive the GPS files.
#' @param gps_adjust_path Character. Filepath to receive the GPS files with speed correction.
#' @param gps_line_path Character. Filepath to receive the GPS files as sf_linestring.
#' @param parallel Logical. Decides whether the function should run in parallel. Defaults is FALSE.
#' @param snap_method The method used to snap stops to the route geometry. There
#'        are two available methods: `nearest1` and `nearest2`. Defaults to 
#'        `nearest2`. See `gtfs2gps::gtfs2gps()` for more info.
#' @param workers Numeric. Number of workers available for parallel processing. 
#' @param continue Logical. Argument that can be used only with filepath. When TRUE, it
#'        skips processing the shape identifiers that were already saved into 
#'        files. It is useful to continue processing a GTFS file that was stopped
#'        for some reason. Default value is FALSE.
#' @param compress Logical. Argument that can be used only with filepath. When TRUE, it
#' compresses the output files by saving them using rds format. Default value is FALSE.
#' Note that compress guarantees that the data saved will be read in the same way as it
#' was created in R. If not compress, the txt extension requires the data to be converted
#' from ITime to string, and therefore they need to manually converted back to ITime to 
#' be properly handled by gtfs2gps.
#' @param min_speed Units. Minimum speed to be considered as valid. It can be 
#' a numeric (in km/h) or a units value able to be converted to km/h. Values
#'  below minimum speed will be adjusted. Defaults to 2 km/h.
#' @param max_speed Units. Maximum speed to be considered as valid. It can be a
#'  numeric (in km/h) or a units value able to be converted to km/h. Values above 
#'  maximum speed will be adjusted. Defaults to 80 km/h.
#' @param new_speed Units. Speed to replace missing values as well as values outside 
#' min_speed and max_speed range. It can be a numeric (in km/h) or a units value 
#' able to be converted to km/h. By default, 'new_speed = NULL' and the function
#'  considers the average speed of the entire gps data.
#' @param clone Logical. Use a copy of the gps_data? Defaults to TRUE.
#' @param crs Numeric. A Coordinate Reference System. The default value is 4326
#'  (latlong WGS84).
#' @details .........
#' @return NULL
#' 
#' @export
#' @examples
#' # Input parameters -----                           # Function            | Data Source | M / . / NA |
#' gtfs =   gtfspath or file                          # readgtfs            |    GTFS     |  Required  |
#' parallel = TRUE                                    # gtfs2gps            |    User     |  Required  |
#' gps_raw_path = "article/data/gps_spo/"             # gtfs2gps            |    User     |  Required  |
#' gps_adjust_path = "article/data/gps_spo_adjusted/" # adjust_speed        |    User     |  Required  |
#' gps_line_path = "article/data/gps_spo_linestring/" # gps_as_sflinestring |    User     |  Required  |
#' spatial_resolution = 50                            # gtfs2gps            |    User     |  Default   |
#' snap_method = "nearest2"                           # gtfs2gps            |    User     |  Default   |
#' workers = 1                                        # gtfs2gps            |    User     |  Default   |
#' continue = FALSE                                   # gtfs2gps            |    User     |  Default   |
#' compress = TRUE                                    # gtfs2gps            |    User     |  Default   |
#' min_speed = 2                                      # adjust_speed        |    User     |  Default   |
#' max_speed = 80                                     # adjust_speed        |    User     |  Default   |
#' new_speed = NULL                                   # adjust_speed        |    User     |  Default   |
#' clone = TRUE                                       # adjust_speed        |    User     |  Default   |
#' crs = 4326                                         # gps_as_sflinestring |    User     |  Default   |
transport_model <- function(gtfs
                            ,spatial_resolution
                            ,parallel
                            ,gps_raw_path
                            ,gps_adjust_path
                            ,gps_line_path
                            ,snap_method = "nearest2"
                            ,workers = 1
                            ,continue = FALSE
                            ,compress = TRUE 
                            ,min_speed = 2
                            ,max_speed = 80
                            ,new_speed = NULL 
                            ,clone = TRUE ){
  
  
  # Read gtfs------
  
  message("Reading GTFS")
  message("------------")
  
  if(as.character(city_gtfs)){
    city_gtfs <- gtfstools::read_gtfs(path = city_gtfs)
  }
  
  # convert frequency to stop_times
  if(gtfs2gps::test_gtfs_freq(city_gtfs) == "frequency"){
    city_gtfs <- gtfstools::frequencies_to_stop_times(gtfs = city_gtfs)
  }
  
  # fix times 
  city_gtfs$stop_times[,arrival_time := data.table::as.ITime(arrival_time)]
  city_gtfs$stop_times[,departure_time := data.table::as.ITime(departure_time)]
  
  
  # generate gps
  if(parallel){
    future::plan(session = "multisession",workers = workers)
  }
  
  # gtfs2gps
  message("Converting GTFS to GPS-like data")
  message("------------")
  
  gtfs2gps::gtfs2gps(gtfs_data = city_gtfs
                     ,snap_method = snap_method
                     ,spatial_resolution = spatial_resolution
                     ,parallel = parallel
                     ,filepath = gps_path
                     ,continue = continue
                     ,compress = compress)
  
  #  Adjust gps speed---------
  
  message("Adjusting the speeds of a gps-like table")
  message("------------")
  files_gps <- list.files(gps_path,full.names = TRUE)
  files_gps_names <- list.files(gps_path,full.names = FALSE)
  
  
  
  gps_speed_fix <- furrr::future_map(seq_along(files_gps),function(i){ # i =1 
    
    #message(paste0("adjust gps speed of file '",files_gps_names[i],"'")) 
    
    tmp_gps <- readr::read_rds(files_gps[i])
    tmp_gps[, dist := units::set_units(dist,"m")]
    tmp_gps[, cumdist := units::set_units(cumdist,"m")]
    tmp_gps[, cumtime := units::set_units(cumtime,"s")]
    tmp_gps_fix <- gtfs2gps::adjust_speed(gps_data = tmp_gps)
    readr::write_rds(x = tmp_gps_fix
                     ,file = paste0(gps_adjust_path
                                    ,files_gps_names[i]),compress = "gz")
    return(NULL)
  })
  
  # GPS as Sf_Linestring ------
  message("Converting a GPS-like data.table to a LineString Simple Feature (sf)")
  message("------------")
  
  dir.create(gps_line_path)
  files_gps <- list.files(gps_adjust_path,full.names = TRUE)
  files_gps_names <- list.files(gps_adjust_path,full.names = FALSE)
  
  
  gpsLine <- furrr::future_map(seq_along(files_gps),function(i){ # i = 1
    
    #message(paste0("Gps points to Linestring file '",files_gps_names[i],"'"))
    
    tmp_gps <- readr::read_rds(files_gps[i])
    tmp_gps_fix <- gtfs2gps::gps_as_sflinestring(gps = tmp_gps)
    
    readr::write_rds(x = tmp_gps_fix
                     ,file = paste0(gps_line_path,files_gps_names[i])
                     , compress = "gz")
    return(NULL)
  },.options = furrr::furrr_options(seed = 123))
  
  return(NULL)
}