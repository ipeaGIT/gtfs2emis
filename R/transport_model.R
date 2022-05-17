#' @title Transport model 
#' 
#' @description Creates the transport model based on a GTFS data input, and exports
#'  in an `sf_linestring` format. Note that each public transport shape_id is saved 
#'  separately in a a different file. It has four main steps: i) Process the GTFS; 
#'  ii) Convert the data to a GPS-like data.table ; iii) Fix speeds; 
#'  iv) Convert GPS to sf_linestring format. These steps uses the functions of 
#'  gtfs2gps package 'read_gtfs', gtfs2gps', 'adjust_speed', and 'gps_as_sflinestring',
#'   respectively.
#' 
#' 
#' @param gtfs_data A path to a GTFS file to be converted to GPS, or a GTFS data
#'                  represented organized as a list of `data.tables` created 
#'                  with `gtfs2gps::read_gtfs()`.
#' @param output_path character. Filepath where the output will be saved. Note 
#'                    that each public transport `shape_id` are saved separately 
#'                    in different files. If `NULL` (Default), the function 
#'                    returns the data to user.  
#' @param min_speed numeric (in km/h) or a speed units value. Minimum speed to be considered as valid. 
#'                  Values below minimum speed will be adjusted. Defaults to 2 km/h.
#' @param max_speed numeric (in km/h) or a speed units value. Maximum speed to be considered as valid. 
#'                  Values above maximum speed will be adjusted. Defaults to 80 km/h.
#' @param new_speed numeric (in km/h) or a speed units value. Speed to replace missing values as well 
#'                  as values outside min_speed and max_speed range. By default, 
#'                  'new_speed = NULL' andthe function considers the mean speed of the entire gps data.
#' @param parallel logical. Decides whether the function should run in parallel. 
#'                 Defaults to TRUE.
#' @param spatial_resolution numeric. The spatial resolution in meters.
#' 
#' @details If the user wants to process the all routes in the GTFS, we suggest 
#'          using the `output_path` argument because the output of the function
#'          can be significantly large for public transport networks with many
#'          routes. This function is a more friendly approach to generate the
#'          transport model. For more advanced users, we recommend reading out
#'          vignette at <<http://www.github.com/ipeaGIT/gtfs2emis/>>.
#' 
#' @return A `sf_linestring` object or `NULL`.
#' 
#' @export
#' @examples if (interactive()) {
#' library(gtfs2emis)
#' library(gtfs2gps)
#' library(magrittr)
#' 
#' gtfs <- gtfs2gps::read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>% 
#'   gtfs2gps::filter_by_shape_id(., "T2-1") %>%
#'   gtfs2gps::filter_single_trip()
#' 
#' tp_model <- transport_model(gtfs_data = gtfs, parallel = TRUE)
#'}
transport_model <- function(gtfs_data, output_path = NULL
                            , min_speed = NULL, max_speed = NULL, new_speed = NULL
                            , parallel = TRUE, spatial_resolution = 100
){
  
  # Check inputs GTFS ----
  
  message("Checking inputs")
  message("---------------")
  
  # output_path
  if(!is.null(output_path)){
    if(!is.character(output_path)){
      stop("User should provided a valid 'output_path' argument.")
    }
  }
  
  # speed
  if(!missing(min_speed) & !is.numeric(min_speed)){
    stop("'min_speed' should be a numeric input.")
  }
  if(!missing(max_speed) & !is.numeric(max_speed)){
    stop("'max_speed' should be a numeric input.")
  }
  if(!missing(new_speed) & !is.numeric(new_speed)){
    stop("'new_speed' should be a numeric input.")
  }
  
  
  # Read GTFS
  
  message("Reading GTFS")
  message("------------")
  
  if(is.character(gtfs_data)){
    city_gtfs <- gtfs2gps::read_gtfs(path = gtfs_data)
  }else{
    city_gtfs <- gtfs_data
  }
  
  # convert frequency to stop_times
  if (gtfs2gps::test_gtfs_freq(city_gtfs) == "frequency") {
    city_gtfs <- gtfstools::frequencies_to_stop_times(gtfs = city_gtfs)
  }
  
  
  # parallel condition
  if(parallel){
    future::plan(session = "multisession", workers = data.table::getDTthreads() - 1)
  }else{
    future::plan(session = "sequential")
  }
  
  # gtfs2gps
  message("Converting GTFS to GPS-like data")
  message("------------")
  
  gps_path <-  paste0(tempdir(),"/gps")
  suppressWarnings(dir.create(gps_path))
  
  gtfs2gps::gtfs2gps(  gtfs_data = city_gtfs
                       , filepath = gps_path
                       , parallel = TRUE
                       , compress = TRUE
                       , spatial_resolution = 100)
  #gtfs2gps::gtfs2gps(  gtfs_data = city_gtfs
  #                      , filepath = gps_path
  #                     , ...)
  
  #  Adjust gps speed---------
  
  message("------------")
  message("Adjusting the speeds of a gps-like table")
  message("------------")
  
  # create new dirs
  gps_adjust_path <-  paste0(tempdir(), "/gps_adjust")
  suppressWarnings(dir.create(gps_adjust_path)) 
  
  # find gps files
  files_gps <- list.files(gps_path, full.names = TRUE)
  files_gps_names <- list.files(gps_path,full.names = FALSE)
  
  gps_speed_fix <- function(i){ # i =1 
    tmp_gps <- readRDS(files_gps[i])
    tmp_gps_fix <- gtfs2gps::adjust_speed(gps_data = tmp_gps
                                         ,min_speed = ifelse(is.null(min_speed),02,min_speed)
                                         ,max_speed = ifelse(is.null(max_speed),80,max_speed)
                                         ,new_speed = new_speed)
   saveRDS(object = tmp_gps_fix
          ,file = paste0(gps_adjust_path
                         ,"/",files_gps_names[i]))
    return(NULL)
  }
  if(parallel){
    future::plan(future::multisession)
    #furrr::future_map(.x = seq_along(files_gps),.f = gps_speed_fix)
    lapply(seq_along(files_gps),gps_speed_fix)
  }else{
    lapply(seq_along(files_gps),gps_speed_fix)
  }
  # GPS as Sf_Linestring ------
  message("Converting a GPS-like data.table to a LineString Simple Feature (sf)")
  message("------------")
  
  # Checking
  if(!missing(output_path)){
    dir.create(output_path)
  }
  
  files_gps <- list.files(gps_adjust_path,full.names = TRUE)
  files_gps_names <- list.files(gps_adjust_path,full.names = FALSE)
  
  # function gps_as_sflinestring
  f_gps_as_sflinestring <- function(i){ # i = files_gps[1]
    tmp_gps <- readRDS(i)
    tmp_gps_fix <- gtfs2gps::gps_as_sflinestring(gps = tmp_gps)
    return(tmp_gps_fix)
  }
  
  # Return conditions ----
  # parallel
  if(parallel){
    gpsLine <- furrr::future_map(files_gps
                                 ,f_gps_as_sflinestring
                                 ,.options = furrr::furrr_options(seed = 123)) 
  }else{
    gpsLine <- lapply(files_gps,f_gps_as_sflinestring) 
  }
  # output
  if(missing(output_path)){
    gpsLine <- data.table::rbindlist(gpsLine) 
    gpsLine <- sf::st_sf(gpsLine)
    return(gpsLine)
  }else{
    gpsLine <- lapply(1:length(gpsLine),function(i){
      saveRDS(object =  sf::st_as_sf(gpsLine[[i]])
              , file = paste0(output_path,files_gps_names[i]))
      return(NULL)
    })
  }
}