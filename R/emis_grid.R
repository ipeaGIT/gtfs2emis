#' @title 
#' Street emissions into grid
#'
#' @description 
#' Aggregate emissions proportionally into grid cells, by performing an 
#' intersection operation between emissions data in `sf linestring` format and 
#' grid cells. It also considers aggregation per grid cells and time.
#'
#' @param data data.table; Data.table with emissions and departure_time data.
#' @param emi character; Columns names of emissions information.
#' @param grid sf polygon; Grid cell data to allocate emissions.
#' @param time_class character; Emissions can be aggregated by 'all periods' (Default),
#' 'hour' or 'hour-minute'.
#' @param time_column vector; Column name of 'departure_time' information (from data input). 
#' Only used when 'hour' or 'hour-minute' is selected. 
#' 
#' @return An `"sf" "data.frame"` object with emissions estimates per grid cell.
#' @export
#' 
#' @examples 
#' library(magrittr)
#' gps <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps")) %>%
#' gtfs2gps::filter_by_shape_id(c("51982")) %>%
#'   gtfs2gps::gtfs2gps() %>%
#'   gtfs2gps::gps_as_sflinestring() 
#'   
#' ef <- ef_europe(speed = data$speed,
#'                 veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
#'                 euro = c("IV","V"),
#'                 pollutant = c("CO2","NOx"),
#'                 fuel = "D" ,
#'                 tech =  c("SCR","EGR"),
#'                 slope = 0.0,
#'                 load = 0.5,
#'                 fcorr = 1,
#'                 as_list = TRUE)
#'                 
#' emi <- emis(fleet_composition =  c(0.7,0.3),
#'             dist = units::set_units(data$dist,"km"),
#'             ef = ef,
#'             aggregate = FALSE,
#'             as_list = FALSE)
#'             
#' data <- cbind(emi,gps) %>% sf::st_as_sf()
#' grid <- vein::make_grid(spobj = data, width =  0.25 / 102.47) # 500 meters
#' my_grid <- emis_grid(data = emi,
#'                      grid = grid,
#'                      emi = c("CO2_Euro_IV","CO2_Euro_V","NOx_Euro_IV","NOx_Euro_V"))
#' 
emis_grid <- function(data, emi, grid, time_class, time_column){
  
  # rm(list=ls())
  # devtools::load_all()
  # 
  # gps <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps")) %>%
  #   gtfs2gps::filter_by_shape_id(c("51982")) %>%
  #   gtfs2gps::filter_single_trip() %>% 
  #   gtfs2gps::gtfs2gps() %>%
  #   gtfs2gps::gps_as_sflinestring() 
  # 
  # ef <- ef_europe(speed = gps$speed,
  #                 veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
  #                 euro = c("IV","V"),
  #                 pollutant = c("CO2","NOx"),
  #                 fuel = "D" ,
  #                 tech =  c("SCR","EGR"),
  #                 slope = 0.0,
  #                 load = 0.5,
  #                 fcorr = 1,
  #                 as_list = TRUE)
  # 
  # emi <- emis(fleet_composition =  c(0.7,0.3),
  #             dist = units::set_units(gps$dist,"km"),
  #             ef = ef,
  #             aggregate = FALSE,
  #             as_list = FALSE)
  # 
  #data <- temp_emi2
  # grid <- vein::make_grid(spobj = data, width =  0.25 / 102.47) # 500 meters
  # emi = c("CO2_Euro_IV","CO2_Euro_V","NOx_Euro_IV","NOx_Euro_V")
  # 
  
  # I) rename for working with unique columns ----
  data.table::setDT(data)
  emi_input <- emi
  emi <- paste0(emi,"_",1:length(emi))
  data.table::setnames(data,old = emi_input,new = emi)
  
  # II) working files -----
  netdata <- data.table::copy(data.table::as.data.table(data))
  net <- sf::st_as_sf(data.table::setDF(data)) 
  
  # III) check units ----
  
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
  
  # convert to numeric ----
  
  netdata[, (emi) := lapply(.SD, as.numeric), .SDcols = emi]
  
  # add 'id' info into grid data ----
  
  grid$id <- 1:(dim(grid)[1])
  
  # IV) check projections ----
  
  if(identical(sf::st_crs(grid), sf::st_crs(net)) == FALSE){
    message("Transforming input data into lat/long projection")
    grid <- sf::st_transform(grid, 4326)
    net <- sf::st_transform(net, 4326)
  }
  
  # V) display emissions sum BEFORE intersection -----
  #
  # objective: user verification purposes, they can check if
  # the intersection operation into grid are displaying the total 
  # emissions correctly
  #
  message("Sum of street emissions ")
  sapply(seq_along(emi), function(j){# j = 1
    sumofstreets <- netdata[, lapply(.SD, sum), .SDcols = emi[j]]
    message(paste(emi_input[j], "=", round(sumofstreets, 2), emi_units[j]))
  })
  
  # VI) estimate emissions over the linestring-------
  
  tmp_netdata <- data.table::copy(netdata)
  if(missing(time_column) == FALSE){
    # 'hour' time stamp------
    if(time_class == "hour"){
      tmp_netdata[, "time_column" := data.table::hour(get(time_column))]
    }
    # 'hour-minute' time stamp------
    if(time_class == "hour-minute"){
      tmp_netdata[, "time_column" := paste0(data.table::hour(get(time_column)), ":",
                                            data.table::minute(get(time_column)))]
    } 
    tmp_netdata <- tmp_netdata[, lapply(.SD, sum, na.rm = TRUE)
                               , .SDcols = emi
                               , by = .(time_column
                                        , shape_id
                                        , stop_sequence
                                        , from_stop_id
                                        , to_stop_id)]
  }else{
    # no 'time_column' scenario------
    tmp_netdata <- tmp_netdata[, lapply(.SD, sum, na.rm = TRUE)
                               , .SDcols = emi
                               , by = .(shape_id
                                        , stop_sequence
                                        , from_stop_id
                                        , to_stop_id)]
  }
  
  # VII) Add info of geometry into 'netdata' ----
  
  tmp_netdata[netdata,on = c("shape_id"
                             ,"stop_sequence"
                             ,"from_stop_id"
                             ,"to_stop_id"),geometry := i.geometry]
  # 'tmp_netdata' to net
  net <- sf::st_as_sf(data.table::setDF(netdata))
  
  # VIII) Reduce grid size  -----
  # Keep only polygons that intersect with lines
  intersect_index <- sf::st_intersects(grid, net,sparse = FALSE)
  intersect_index <- rowSums(intersect_index)
  intersect_index <- which(intersect_index>0)
  grid <- subset(grid, id %in% intersect_index)
  net$temp_lkm <- sf::st_length(net)
  
  # IV) Intersection -----
  #grid$my_id <- 1:nrow(grid)
  
  netg <- suppressMessages(suppressWarnings(
    sf::st_intersection(net, grid)))
  
  # class(netg)
  # data.table::uniqueN(netg$id)
  # data.table::uniqueN(grid$id)
  # data.table::uniqueN(grid$my_id)
  # 
  # mapview::mapview(grid$geometry) +
  #   mapview::mapview(netg$geometry)
  # 
  
  netg$lkm_inter <- sf::st_length(netg)
  
  # ratio of 'lkm'
  data.table::setDT(netg)
  netg[, ratio := lkm_inter / temp_lkm]
  
  # adjust emissions by ratio
  
  for (col in emi) netg[, (col) := get(col) * ratio]
  
  #
  # V) aggregate by TIME-------------------
  #
  
  if(missing(time_column) == FALSE){
    # time_class
    netg <- netg[, lapply(.SD, sum, na.rm = TRUE)
                 ,.SDcols = emi, 
                 by = .(time_column, id)]
  }else{
    # aggregation of emissions without specified time
    netg <- netg[, lapply(.SD, sum, na.rm = TRUE)
                 , by = .(id)
                 ,.SDcols = emi]
  }
  
  # VI) prepare output file--------
  
  temp_output <- netg[data.table::setDT(grid),on = "id",
                      geometry := i.geometry] 
  
  # add units
  temp_output[, (emi) := lapply(.SD, as.numeric), .SDcols = emi]
  
  for(k in 1:length(emi_units)){ # k = 1
    temp_output[, (emi[k]) := lapply(.SD, 
                                     units::as_units,
                                     emi_units[k]),
                .SDcols = emi[k]]
  }
  
  #
  # VII) display emissions sum AFTER intersection -----
  # print emission sum after operation 
  #
  
  message("Sum of gridded emissions ")
  sapply(seq_along(emi), function(l){ # l = 1
    sumofgrids <- temp_output[, lapply(.SD, sum), .SDcols = emi[l]]
    message(paste(emi_input[l], "=", round(sumofgrids, 2), emi_units[l]))
  })
  
  # rename columns
  data.table::setnames(temp_output,old = emi,new = emi_input)
  if(missing(time_column) == FALSE){
    data.table::setnames(temp_output,old = "time_column",new = time_column)
  }
  
  
  # return output ------
  #
  
  temp_output <- sf::st_as_sf(temp_output)
  
  return(temp_output)
}