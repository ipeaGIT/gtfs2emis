#' @title Transport model 
#' 
#' @description Creates the transport model based on a GTFS data input, and exports
#'  in an `sf_linestring` format. Note that 
#'  each public transport shape_id is saved separately in a a different file.
#'  It has four main steps: i) Process the GTFS; 
#'  ii) Convert the data to a GPS-like data.table ; iii) Fix speeds; iv) Convert GPS to sf_linestring
#'   format. These steps uses the functions of gtfs2gps package 'read_gtfs',
#'   'gtfs2gps', 'adjust_speed', and 'gps_as_sflinestring', respectively.
#' 
#' iii) Fix speeds [666] incluir parametros, min=2 max=80, med=mean
#' 
#' 
#' @param gtfs_data A path to a GTFS file to be converted to GPS, or a GTFS data
#'                  represented organized as a list of `data.tables` created 
#'                  with `gtfs2gps::read_gtfs()`.
#' @param output_path character. Filepath where the output will be saved. Note 
#'                    that each public transport `shape_id` are saved separately 
#'                    in different files. If `NULL` (Default), the function 
#'                    returns the data to user.  
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
transport_model <- function(gtfs_data, output_path = NULL, parallel = TRUE, spatial_resolution = 50){
  
  # 666 Check inputs 
  # 666 Check inputs 
  # 666 Check inputs 
  
  
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
  
  gps_path <-  paste0(tempdir(), "/gps")
  dir.create(gps_path)
  
  gtfs2gps::gtfs2gps(  gtfs_data = city_gtfs
                     , spatial_resolution = spatial_resolution
                     , parallel = parallel
                     , filepath = gps_path
                     , compress = TRUE)
  
  #  Adjust gps speed---------
  
  message("------------")
  message("Adjusting the speeds of a gps-like table")
  message("------------")
  
  # create new dirs
  gps_adjust_path <-  paste0(tempdir(), "/gps_adjust")
  dir.create(gps_adjust_path)
  
  # find gps files
  files_gps <- list.files(gps_path, full.names = TRUE)
  files_gps_names <- list.files(gps_path,full.names = FALSE)
  
  gps_speed_fix <- furrr::future_map(seq_along(files_gps),function(i){ # i =1 
    
    tmp_gps <- readRDS(files_gps[i])
    tmp_gps[, dist := units::set_units(dist,"m")]
    tmp_gps[, cumdist := units::set_units(cumdist,"m")]
    tmp_gps[, speed := units::set_units(speed,"km/h")]
    tmp_gps[, cumtime := units::set_units(cumtime,"s")]
    tmp_gps_fix <- gtfs2gps::adjust_speed(gps_data = tmp_gps)
    saveRDS(x = tmp_gps_fix
                     ,file = paste0(gps_adjust_path
                                    ,"/",files_gps_names[i]))
    return(NULL)
  })
  
  # GPS as Sf_Linestring ------
  message("Converting a GPS-like data.table to a LineString Simple Feature (sf)")
  message("------------")
  
  # Checking
  if(!missing(output_path)){
      dir.create(output_path)
  }

  files_gps <- list.files(gps_adjust_path,full.names = TRUE)
  files_gps_names <- list.files(gps_adjust_path,full.names = FALSE)
  
  # Conditions
  
  if(missing(output_path)){
    gpsLine <- furrr::future_map(seq_along(files_gps),function(i){
      tmp_gps <- readRDS(files_gps[i])
      tmp_gps_fix <- gtfs2gps::gps_as_sflinestring(gps = tmp_gps)
      return(tmp_gps_fix)
    },.options = furrr::furrr_options(seed = 123)) %>% 
      data.table::rbindlist()
    return(gpsLine)
  }else{
    gpsLine <- furrr::future_map(seq_along(files_gps),function(i){
      tmp_gps <- readRDS(files_gps[i])
      tmp_gps_fix <- gtfs2gps::gps_as_sflinestring(gps = tmp_gps)
      
      saveRDS(x = tmp_gps_fix
                       , file = paste0(output_path,files_gps_names[i])
                       )
      return(NULL)
    },.options = furrr::furrr_options(seed = 123))
  }
}