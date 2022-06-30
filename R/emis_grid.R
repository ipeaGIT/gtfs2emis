#' @title 
#' Allocate emissions into a spatial grid
#'
#' @description 
#' Aggregate emissions proportionally in an sf polygon grid, by performing an 
#' intersection operation between emissions data in `sf linestring` format and 
#' the input grid cells. User can also aggregate the emissions in the grid 
#' by time of the day.
#'
#' @param data Data.frame; Data.frame containing the emissions and time stamp column data.
#' @param emi Character; Column names of emissions in 'data'.
#' @param grid Sf polygon; Grid cell data to allocate emissions.
#' @param time_class Character; type of time aggregation in 'data', which can be 
#' an aggregation by 'all periods' (Default), 'hour' or 'hour-minute'.
#' @param time_column Vector; Column name with time stamp information (from 'data'). 
#' Argument required when time_class = 'hour' or 'hour-minute' is selected.  
#' @return An `"sf" "data.frame"` object with emissions estimates per grid cell.
#' @export
#' 
#' @examples if (interactive()) {
#' library(gtfs2emis)
#' library(gtfstools)
#' 
#' # read GTFS, and keep a single trip_id to speed up this example
#' gtfs_file <- system.file("extdata/bra_cur/bra_cur_gtfs.zip", package = "gtfs2emis")
#' gtfs <- gtfstools::read_gtfs(gtfs_file) 
#' gtfs_small <- gtfstools::filter_by_trip_id(gtfs, trip_id ="4439186")
#'   
#' # run transport model
#' tp_model <- transport_model(gtfs_data = gtfs_small,
#'                             min_speed = 2,
#'                             max_speed = 80,
#'                             new_speed = 20,
#'                             spatial_resolution = 100,
#'                             parallel = FALSE)
#'
#' # Fleet data, using Brazilian emission model and fleet
#' fleet_data_ef_cetesb <- data.frame(veh_type = "BUS_URBAN_D",
#'                                    model_year = 2010:2019,
#'                                    fuel = "D",
#'                                    fleet_composition = rep(0.1,10)
#'                                    )
#' # Emission model
#' emi_list <- emission_model(
#'                 tp_model = tp_model,
#'                 ef_model = "ef_brazil_cetesb",
#'                 fleet_data = fleet_data_ef_cetesb,
#'                 pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                 )
#'
#' # create spatial grid
#' mygrid <- sf::st_make_grid(
#'   x = sf::st_bbox(emi_list$tp_model$geometry)
#'   , cellsize = 0.25 / 200
#'   , crs= 4329
#'   , what = "polygons"
#'   , square = FALSE
#'   )
#'   
#' mygrid <- sf::st_sf(data.frame(id=1:length(mygrid), geom=mygrid))
#' 
#' 
#' #
#' # mygrid_emi <- emis_grid(data = emi_list$tp_model,
#' #                         grid = mygrid,
#' #                         time_resolution = 'day')
#'}
emis_grid <- function(data, emi, grid, time_class, time_column){
  
  
  # Rename columns ----
  data.table::setDT(data)
  emi_input <- emi
  emi <- paste0(emi,"_",1:length(emi))
  data.table::setnames(data,old = emi_input,new = emi)
  
  # Working files -----
  netdata <- data.table::copy(data.table::as.data.table(data))
  net <- sf::st_as_sf(data.table::setDF(data)) 
  
  # Check units ----
  
  emi_units <- c()
  for(i in 1:length(emi)){ # i = 1
    if(class(netdata[, .SD, .SDcols = (emi[i])][[1]]) == "units"){
      # retrieve units
      emi_units[i] <- units::deparse_unit(netdata[, .SD, .SDcols = (emi[i])][[1]])
      
      message(paste0('input data "', emi_input[i], '" is in units: ', emi_units[i]))
    }else{
      emi_units[i] <- NA
      message(paste0('input data "', emi[i], '" has no units'))
      
    }
  }
  
  # Convert to numeric ----
  
  netdata[, (emi) := lapply(.SD, as.numeric), .SDcols = emi]
  
  # Add 'id' info into grid data ----
  
  grid$id <- 1:(dim(grid)[1])
  
  # Check projections ----
  
  if(identical(sf::st_crs(grid), sf::st_crs(net)) == FALSE){
    message("Transforming input data into lat/long projection")
    grid <- sf::st_transform(grid, 4326)
    net <- sf::st_transform(net, 4326)
  }
  
  # Display emissions BEFORE  -----
  #
  # objective: user verification purposes, they can check if
  # the intersection operation into grid are displaying the total 
  # emissions correctly
  #
  message("Sum of street emissions ")
  sapply(seq_along(emi), function(j){# j = 1
    sumofstreets <- netdata[, lapply(.SD, sum, na.rm = TRUE), .SDcols = emi[j]]
    message(paste(emi_input[j], "=", round(sumofstreets, 2), emi_units[j]))
  })
  
  # Estimate emissions -------
  
  tmp_netdata <- data.table::copy(netdata)
  data.table::setDT(tmp_netdata)
  if(!missing(time_column)){
    ### 'hour' time stamp
    if(time_class == "hour"){
      tmp_netdata[, time_column := data.table::hour(get(time_column))]
    }
    ### 'hour-minute' time stamp
    if(time_class == "hour-minute"){
      
      tmp_netdata[
        , "time_column" := paste0(data.table::hour(get(time_column)), ":",
                                  data.table::minute(get(time_column)))
      ]
      
    } 
    
    tmp_netdata <- tmp_netdata[
      , lapply(.SD, sum, na.rm = TRUE)
      , .SDcols = emi
      , by = .(time_column
               , shape_id
               , stop_sequence
               , from_stop_id
               , to_stop_id)]
    
  }else{
    
    # No 'time_column' scenario 
    tmp_netdata <- tmp_netdata[
      , lapply(.SD, sum, na.rm = TRUE)
      , .SDcols = emi
      , by = .(shape_id
               , stop_sequence
               , from_stop_id
               , to_stop_id)]
    
  }
  
  # VII) Add geometry into 'netdata' ----
  
  tmp_netdata[netdata
              ,on = c("shape_id"
                      ,"stop_sequence"
                      ,"from_stop_id"
                      ,"to_stop_id")
              ,geometry := i.geometry]
  
  # 'tmp_netdata' to net
  net <- sf::st_as_sf(data.table::setDF(tmp_netdata))
  
  # Reduce grid size  -----
  # Keep only polygons that intersect with lines
  
  intersect_index <- sf::st_intersects(grid, net,sparse = FALSE)
  intersect_index <- rowSums(intersect_index)
  intersect_index <- which(intersect_index>0)
  grid <- subset(grid, id %in% intersect_index)
  net$temp_lkm <- sf::st_length(net)
  
  # Intersection -----
  netg <- suppressMessages(suppressWarnings(
    sf::st_intersection(net, grid)))
  
  netg$lkm_inter <- sf::st_length(netg)
  
  # ratio of 'lkm'
  data.table::setDT(netg)
  netg[, ratio := lkm_inter / temp_lkm]
  
  # adjust emissions by ratio
  for (col in emi) netg[, (col) := get(col) * ratio]
  
  # Aggregate by TIME-------------------
  # 
  if(!missing(time_column)){
    
    # time_class
    netg <- netg[
      , lapply(.SD, sum, na.rm = TRUE)
      ,.SDcols = emi
      ,by = .(time_column, id)]
    
  }else{
    
    # aggregation of emissions without specified time
    netg <- netg[
      , lapply(.SD, sum, na.rm = TRUE)
      , by = .(id)
      ,.SDcols = emi]
    
  }
  
  # VI) prepare output file--------
  
  temp_output <- netg[data.table::setDT(grid)
                      ,on = "id"
                      ,geometry := i.geometry] 
  
  # add units
  temp_output[, (emi) := lapply(.SD, as.numeric), .SDcols = emi]
  
  for(k in 1:length(emi_units)){ # k = 1
    
    temp_output[
      , (emi[k]) := lapply(.SD, 
                           units::as_units,
                           emi_units[k])
      ,.SDcols = emi[k]]
    
  }
  
  #
  # VII) display emissions sum AFTER intersection -----
  # print emission sum after operation 
  #
  
  message("Sum of gridded emissions ")
  sapply(seq_along(emi), function(l){ # l = 1
    
    sumofgrids <- temp_output[, lapply(.SD, sum, na.rm = TRUE), .SDcols = emi[l]]
    message(paste(emi_input[l], "=", round(sumofgrids, 2), emi_units[l]))
    
  })
  
  
  # rename columns
  data.table::setnames(temp_output
                       ,old = emi
                       ,new = emi_input)
  
  if(missing(time_column) == FALSE){
    
    data.table::setnames(temp_output,old = "time_column"
                         ,new = time_column)
    
  }
  
  
  # return output ------
  
  temp_output <- sf::st_as_sf(temp_output)
  
  return(temp_output)
}