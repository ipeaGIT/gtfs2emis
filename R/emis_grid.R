#' @title Street emissions into grid
#'
#' @description Aggregate emissions proportionally into grid cells, by performing an intersection 
#' operation between emissions data in linestring format and grid cells. It also considers
#' aggregation per grid cells and time.
#'
#' @param data data.table; Data.table with emissions and departure_time data.
#' @param emi character; Columns names of emissions information.
#' @param grid sf polygon; Grid cell data to allocate emissions.
#' @param time_class character; Emissions can be aggregated by 'all periods' (Default),
#' 'hour' or 'hour-minute'.
#' @param time_column vector; Column name of 'departure_time' information (from data input). 
#' Only used when 'hour' or 'hour-minute' is selected. 
#' @return Spatial data (emissions into grid cells).
#' @export
emis_grid <- function(data, emi, grid, time_class = "all periods", time_column){
  # input
  # rm(list=ls())
  # data <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps")) %>%
  #   gtfs2gps::filter_by_shape_id(c("51982")) %>%
  #   gtfs2gps::gtfs2gps() %>% 
  #   gtfs2gps::gps_as_sflinestring() %>% 
  #   dplyr::select(speed, dist, departure_time)  %>% 
  #   dplyr::rename(departure_time1 = departure_time)
  # 
  # 
  # grid <- vein::make_grid(spobj = data, width =  0.25 / 102.47) # 500 meters
  # 
  # emi = c('speed','dist') 
  # time_class = "hour"
  # time_column = 'departure_time1'
  
  # working files -----
  
  net <- data
  netdata <- data.table::as.data.table(data)
  
  # check units ----
  
  emi_units <- c()
  for(i in emi){ # i = emi[1]
    if(class(netdata[, .SD, .SDcols = (i)][[1]]) == "units"){
      # retrieve units
      emi_units[i] <- units::deparse_unit(netdata[, .SD, .SDcols = (i)][[1]])
      
      message(paste0('input data "', i, '" is in units: ', emi_units[i]))
    }else{
      emi_units[i] <- NA
      message(paste0('input data "', i, '" has no units'))
      
    }
  }
  
  # convert to numeric ----
  
  netdata[, (emi) := lapply(.SD, as.numeric), .SDcols = emi]
  
  # add 'id' info into grid data ---
  
  grid$id <- 1:(dim(grid)[1])
  
  # check projections ---
  
  if(identical(sf::st_crs(grid), sf::st_crs(net)) == FALSE){
    message("Transforming input data into lat/long projection")
    grid <- sf::st_transform(grid, 4326)
    net <- sf::st_transform(net, 4326)
  }
  
  # display emissions sum BEFORE intersection -----
  #
  # objective: user verification purposes, they can check if
  # the intersection operation into grid are displaying the total 
  # emissions correctly
  #
  message("Sum of street emissions ")
  sapply(emi, function(i){
    sumofstreets <- netdata[, lapply(.SD, sum), .SDcols = i]
    message(paste(i, "=", round(sumofstreets, 2), emi_units[[i]]))
  })
  
  # intersection operation -----
  
  net$temp_lkm <- sf::st_length(net)
  
  netg <- suppressMessages(suppressWarnings(
    sf::st_intersection(net, grid)))
  
  netg$lkm_inter <- sf::st_length(netg)
  
  # ratio of 'lkm'
  setDT(netg)
  netg[, ratio := lkm_inter / temp_lkm]
  
  # adjust emissions by ratio
  
  for (col in emi) netg[, (col) := get(col) * ratio]
  
  #
  # agreggate by TIME-------------------
  #
  
  if(missing(time_column) == FALSE){
    # 'hour' time stamp
    if(time_class == "hour"){
      netg[, (time_column) := data.table::hour(get(time_column))]
      netg <- netg[, lapply(.SD, sum), .SDcols = emi, by = .('time_column' = get(time_column), id)]
      
    }
    
    # 'hour-minute' time stamp
    if(time_class == "hour-minute"){
      netg[, (time_column) := paste0(data.table::hour(get(time_column)), ":",
                                          data.table::minute(get(time_column)))]
      netg <- netg[, lapply(.SD, sum), .SDcols = emi, by = .('time_column' = get(time_column), id)]
    }
  }else{
    # aggregation of emissions without specified time
    netg <- netg[, lapply(.SD, sum), by = "id", .SDcols = emi]
  }

  # prepare output file--------
  
  temp_output <- setDT(grid)[netg,on = "id"] 
  
  #
  # display emissions sum AFTER intersection -----
  # print emission sum after operation 
  #
  message("Sum of gridded emissions ")
  sapply(emi, function(i){ # i = emi[1]
    sumofgrids <- temp_output[, lapply(.SD, sum), .SDcols = i]
    message(paste(i, "=", round(sumofgrids, 2), emi_units[[i]]))
  })
  
  #
  # return output ------
  #
  
  temp_output <- sf::st_as_sf(temp_output)
  
  return(temp_output)
}
