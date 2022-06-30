#' @title 
#' Transport model 
#' 
#' @description
#'  This function converts a public transport data set in GTFS format into a 
#'  GPS-like table with the space-time positions and speeds of public transport 
#'  vehicles. The function also allow users to set the spatial resolution of the
#'  output and to adjust the speed of public transport vehicles given a 
#'  `min_speed` and `max_speed` range.
#' 
#' @param gtfs_data A path to a GTFS file or a GTFS data organized as a list of 
#'        `data.tables` created with `gtfstools::read_gtfs()`.
#' @param min_speed numeric (in km/h) or a speed units value. Minimum speed to 
#'        be considered as valid. Values below minimum speed will be updated 
#'        according to the `new_speed` parameter, which can affect the arrival 
#'        and  departure times of vehicles at  transit stops. Defaults to `2` 
#'        km/h.
#' @param max_speed numeric (in km/h) or a speed units value. Maximum speed to 
#'        be considered as valid. Values above maximum speed will be  updated 
#'        according to the `new_speed` parameter, which can affect the arrival 
#'        and departure times of vehicles at transit stops. Defaults to `80` 
#'        km/h.
#' @param new_speed numeric (in km/h) or a speed units value. Speed value used 
#'        to replace the speeds that fall outside the `min_speed` and `max_speed` 
#'        range or which are missing from the GTFS input. When `new_speed = NULL` 
#'        (the default), the function uses the average speed of the entire GTFS 
#'        data feed.
#' @param parallel Decides whether the function should run in parallel. Defaults 
#'        to `TRUE`. When `TRUE`, it will use all cores available minus one 
#'        using `future::plan()` with strategy "multisession" internally.
#' @param spatial_resolution The spatial resolution in meters. Defaults to `100`.
#'        The function only creates points in order to guarantee that the 
#'        minimum distance between two consecutive points will be at most the 
#'        `spatial_resolution` value. If a given GTFS shape_id has two 
#'        consecutive points with a distance smaller than the spatial resolution, 
#'        the algorithm will not remove such points. 
#' @param output_path character. A directory path. If `NULL` (Default), the 
#'        function returns the output. If the user passes a valid `passed`, the 
#'        output will be saved in the `output_path` dir. Note that that the 
#'        output of each public transport `shape_id` is saved separately in 
#'        different files. Setting an `output_path` is recommended when working
#'        with large public transport system because the output of the function 
#'        can be significantly large.
#'        
#' @return A `data.table sf_linestring` object or `NULL`.
#' 
#' @family Core function
#' @export
#' 
#' @examples if (interactive()) {
#' library(gtfs2emis)
#' library(gtfstools)
#' 
#' # read GTFS
#' gtfs_file <- system.file("extdata/bra_cur/bra_cur_gtfs.zip", package = "gtfs2emis")
#' gtfs <- gtfstools::read_gtfs(gtfs_file) 
#' 
#' # keep a single trip_id to speed up this example
#' gtfs_small <- gtfstools::filter_by_trip_id(gtfs, trip_id ="4439181")
#'   
#' # run transport model
#' tp_model <- transport_model(gtfs_data = gtfs_small,
#'                              parallel = TRUE)
#'}
transport_model <- function(gtfs_data,
                            min_speed = 2,
                            max_speed = 80, 
                            new_speed = NULL,
                            parallel = TRUE,
                            spatial_resolution = 100,
                            output_path = NULL){
  
  #gtfs_data = gtfs
  #min_speed = 2
  #max_speed = NULL
  #new_speed = NULL
  #parallel = FALSE
  #spatial_resolution = 30
  #output_path = NULL
  
  # Check inputs GTFS ----
  
  # output_path
  if(!is.null(output_path)){
    if(!is.character(output_path)){
      stop("User should provided a valid 'output_path' argument.")
    }
  }
  
  # speed
  checkmate::assert_numeric(min_speed, lower = 2, finite = TRUE, null.ok = TRUE)  
  checkmate::assert_numeric(max_speed, lower = 3, finite = TRUE, null.ok = TRUE)  
  checkmate::assert_numeric(new_speed, lower = 1, finite = TRUE, null.ok = TRUE)
  checkmate::assert_true(min_speed < max_speed)
  
  
  # Read GTFS ------------
  
  if (is.character(gtfs_data)) {
    city_gtfs <- gtfstools::read_gtfs(path = gtfs_data)
  }else{
    city_gtfs <- gtfs_data
  }
  
  
  # parallel condition
  checkmate::assert_logical(parallel)  
  
  if(parallel){
    future::plan(session = "multisession", workers = data.table::getDTthreads() - 1)
  }else{
    future::plan(session = "sequential")
  }
  
  # gtfs2gps ------------

  gps_path <-  paste0(tempdir(),"/gps")
  suppressWarnings(dir.create(gps_path))
  
  gtfs2gps::gtfs2gps( gtfs_data = city_gtfs
                     , filepath = gps_path
                     , parallel = parallel
                     , compress = TRUE
                     , spatial_resolution = spatial_resolution)

  
    
  #  Adjusting the speeds of a gps-like table ---------
  

  # create new dirs
  gps_adjust_path <-  paste0(tempdir(), "/gps_adjust")
  suppressWarnings(dir.create(gps_adjust_path)) 
  
  # find gps files
  files_gps <- list.files(gps_path, full.names = TRUE)
  files_gps_names <- list.files(gps_path,full.names = FALSE)
  
  gps_speed_fix <- function(i){ # i =1 
    tmp_gps <- readRDS(files_gps[i])
    tmp_gps_fix <- gtfs2gps::adjust_speed(gps_data = tmp_gps
                                          ,min_speed = min_speed
                                          ,max_speed = max_speed
                                          ,new_speed = new_speed)
    saveRDS(object = tmp_gps_fix
            ,file = paste0(gps_adjust_path
                           ,"/",files_gps_names[i]))
    return(NULL)
  }
  if(parallel){
    future::plan(future::multisession)
    furrr::future_map(.x = seq_along(files_gps)
                      ,.f = gps_speed_fix
                      ,.options = furrr::furrr_options(seed = 123)) 
    #lapply(seq_along(files_gps),gps_speed_fix)
  }else{
    lapply(seq_along(files_gps),gps_speed_fix)
  }
  
  # GPS as Sf_Linestring ------
  # Converting a GPS-like data.table to a LineString Simple Feature (sf)
  
  # Checking
  if (! is.null(output_path)) {
    dir.create(output_path)
  }
  
  files_gps <- list.files(gps_adjust_path,full.names = TRUE)
  files_gps_names <- list.files(gps_adjust_path,full.names = FALSE)
  
  # function gps_as_sflinestring
  f_gps_as_sflinestring <- function(i){ # i = files_gps[1]
    tmp_gps <- readRDS(i)
    tmp_gps[, dist := units::set_units(dist, "km") ]
    tmp_gps_fix <- gtfs2gps::gps_as_sflinestring(gps = tmp_gps)
    return(tmp_gps_fix)
  }
  
  # Return conditions ----
  if(parallel){
    gpsLine <- furrr::future_map(files_gps
                                 ,f_gps_as_sflinestring
                                 ,.options = furrr::furrr_options(seed = 123)) # why ???
  }else{
    gpsLine <- lapply(files_gps,f_gps_as_sflinestring) 
  }
  
  
  # output
  if(missing(output_path)){
    gpsLine <- data.table::rbindlist(gpsLine) 
    suppressWarnings( gpsLine <- sf::st_sf(gpsLine, crs = 4326) )
    class(gpsLine) <- c("sf", "data.frame")
    return(gpsLine)
  }else{
    gpsLine <- lapply(1:length(gpsLine),function(i){
      temp <- sf::st_sf(gpsLine[[i]], crs = 4326)
      class(temp) <- c("sf", "data.frame")
      saveRDS(object =  temp, 
              file = paste0(output_path,files_gps_names[i]))
      return(NULL)
    })
  }
}