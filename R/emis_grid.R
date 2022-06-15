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
#'# GTFS2gps filter-----
#'library(data.table)
#'library(magrittr)
#'fort <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip"
#'                                        , package = "gtfs2gps"))  %>%
#'  gtfs2gps::filter_single_trip() %>% 
#'  gtfs2gps::filter_by_shape_id(c("shape804-I", "shape806-I"))
#'
#'fort_gps <- gtfs2gps::gtfs2gps(fort, parallel = TRUE)
#'
#'fort_gpslines <- gtfs2gps::gps_as_sflinestring(fort_gps)
#'
#'fort_gpslines$dist <- units::set_units(fort_gpslines$dist, "km")
#'
#'# fleet -----------
#'total_fleet <- data.table::data.table(year = c(2005,2010,2011,2012
#'                                               ,2014,2015,2017,2018,2019),
#'                                      bus = c(1,61,50,1,45,18,62,27,31),
#'                                      veh_type_euro = "Ubus Std 15 - 18 t",
#'                                      euro_stage = c("II", "IV", "IV", "V"
#'                                                     , "V", "V", "V", "V","V"))
#'total_fleet$fleet_composition <- total_fleet$bus/sum(total_fleet$bus)
#'
#'# Emission factor
#'set.seed(1234)
#'EF_europe <- ef_europe_emep(pollutant = c("CO", "PM10"),
#'                       speed = fort_gpslines$speed,
#'                       veh_type = total_fleet$veh_type_euro,
#'                       tech = "SCR",
#'                       euro = total_fleet$euro_stage,
#'                       fcorr = rnorm(9, 0.5, 0.1))
#'# EUROPE
#'emi_europe <- emis(fleet_composition = total_fleet$fleet_composition,
#'                   dist = fort_gpslines$dist,
#'                   ef = EF_europe,
#'                   prefix = "EU")
#'
#'for_emis <- cbind(fort_gpslines, emi_europe$emi)
#'
#'# Grid
#'grid_gps <- sf::st_make_grid(for_emis, cellsize = 0.25 / 102.47, square = FALSE)
#'for_sf <- sf::st_as_sf(for_emis)
#'
#'pol_gps <- emis_grid(data = for_sf,
#'                     emi = c("EU_CO_total"),
#'                     grid = grid_gps,
#'                     time_class = 'all periods')
#'
#'pol_gps_hour <- emis_grid(data = for_sf,
#'                          emi = c("EU_CO_total"),
#'                          grid = grid_gps,
#'                          time_class = 'hour',
#'                          time_column = 'timestamp')
#'
#'pol_gps_hour_minute <- emis_grid(data = for_sf,
#'                                 emi = c("EU_CO_total"),
#'                                 grid = grid_gps,
#'                                 time_class = 'hour-minute',
#'                                 time_column = 'timestamp')
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