#' @title 
#' Summarize emissions estimates
#'
#' @description 
#' Summarize emissions estimates, aggregating emissions by pollutant, time of the
#' day, vehicle.
#'
#' @param emi_list List; Emission or emission factor list.
#' @param by Character; Emissions can be aggregated by 'time', 'vehicle', 
#' or simply 'pollutant' (Default). 
#' @param veh_vars Character; data.frame names of 'emi_list' attributed to vehicle
#'        characteristics. Default is 'veh_type'.
#' @param segment_vars Character; data.frame names of 'emi_list' object attributed to the
#'        road segments. Default is NULL.
#'  
#' @return `data.table` with pollutants units ('g') aggregated by time, vehicle 
#' type, or road segment.
#' @export
#' 
#' @examples if (interactive()) {
#' 
#' library(gtfs2emis)
#' library(gtfstools)
#' 
#' # read GTFS
#' gtfs_file <- system.file("extdata/irl_dub/irl_dub_gtfs.zip", package = "gtfs2emis")
#' gtfs <- gtfstools::read_gtfs(gtfs_file)
#' 
#' # Keep a single trip
#' gtfs <- gtfstools::filter_by_trip_id(gtfs
#'                                      , trip_id = c('619.3.60-40-d12-1.224.O'
#'                                                    ,"10812.1.60-13-d12-1.26.I"))
#' # Transport model
#' tp_model <- transport_model(gtfs_data = gtfs,
#'                             spatial_resolution = 100,
#'                             parallel = TRUE)
#' 
#' # fleet data
#' fleet_df <- read.csv(system.file("extdata/irl_dub/irl_dub_fleet.txt"
#'                                  , package = "gtfs2emis"))
#' # emission model
#' emi_list <- emission_model(tp_model = tp_model
#'                            , ef_model = "ef_europe_emep"
#'                            , fleet_data = fleet_df
#'                            , pollutant = c("CO2","PM10"))
#'
#' # Aggregate total emissions by 'pollutant'
#' emis_summary(emi_list) 
#' 
#' # by vehicle type
#' emis_summary(emi_list, by = "vehicle")
#'              
#' emis_summary(emi_list
#'              , by = "vehicle"
#'              , veh_vars = c("euro"))
#'
#' emis_summary(emi_list
#'              , by = "vehicle"
#'              , veh_vars = c("fuel"))
#'
#' emis_summary(emi_list
#'              , by = "vehicle"
#'              , veh_vars = c("veh_type","euro","tech","fuel"))
#'              
#' # by time of the day
#' emis_summary(emi_list
#'              , by = "time"
#'              , segment_vars = "slope") 
#' 
#'}
emis_summary <- function(emi_list, 
                         by = "pollutant", 
                         veh_vars = "veh_type", 
                         segment_vars = NULL){
  
# gtfs_file <- system.file("extdata/bra_cur/bra_cur_gtfs.zip", package = "gtfs2emis")
# 
#  cur <- gtfstools::read_gtfs(gtfs_file)  %>%
#    gtfs2gps::filter_single_trip() %>% 
#    gtfstools::filter_by_trip_id("4439181") %>% 
#    gtfs2gps::gtfs2gps() %>% 
#    gtfs2gps::adjust_speed() %>% 
#    gtfs2gps::gps_as_sflinestring()
#  
#  cur
#  ef <- ef_europe_emep(speed = cur$speed,
#                       veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
#                       euro = c("IV","V"),
#                       pollutant = c("CO2","NOx"),
#                       fuel = "D" ,
#                       tech =  c("SCR","EGR"),
#                       as_list = TRUE)
#  
#  emi_list <- emis(fleet_composition =  c(0.7,0.3),
#              dist = units::set_units(cur$dist,"km"),
#              ef = ef,
#              aggregate = FALSE,
#              as_list = TRUE)  
#
   #emi_list$tp_model <- cur
   #by = "time"
  # emi_vars = "emi"
  # veh_vars = "veh_type"
  # pol_vars = "pollutant"
  #segment_vars = "tp_model"
  #time_column = "timestamp"
  
  
  #
  # check list condition -----
  #
  
  if(class(emi_list) != "list"){
    stop("Invalid class: 'emi_list' input needs to have a list format.")
  }
  
  # check consistencies----
  
  if(by != "time" & by != "pollutant"  & by != "vehicle"){
    stop("Invalid input: 'by' argument needs to be 'time', 'vehicle' or 'pollutant' type")
  }
  
  if(!("emi" %in% names(emi_list))){
    stop("No emissions data.frame found: 'emi_list' should have a data.frame named 'emi' with emissions information.")
  }
  
  if(!("pollutant" %in% names(emi_list))){
    stop("No 'pollutant' vector found: 'emi_list' should have a vector named 'pollutant'.")
  }
  
  if(by == "time"){
    if(missing(segment_vars)){
      stop("Missing argument: 'segment_vars' argument is needed when by = 'time' is assigned")
    }
    if(!("timestamp" %in% names(emi_list[[segment_vars]]))){
      stop(sprintf("Missing argument: 'timestamp' argument is needed in '%s' data"
                   ,segment_vars))
    }
    if(length(emi_list[[segment_vars]][["timestamp"]]) != nrow(emi_list[["emi"]])){
      stop("Incorrect dimensions: 'emi' columns needs to have the same length of 'timestamp'")
    }
  }
  
  if(by == "vehicle"){
    lapply(veh_vars,function(i){
      if(is.null(i)){
        stop("Missing argument: 'veh_vars' or 'pol_vars' are missing for 'vehicle type' post processing")
      }
    })
    segment_vars <- NULL
  }
  
  #
  # check 'emi_vars' units-----
  #
  
  lapply(seq_along(emi_list[["emi"]]),function(i){
    if(class(emi_list[["emi"]][[i]]) != "units"){
      stop("Missing units. Emissions 'emi' neeeds to have 'units' class. Please, check package 'units'")
    }
    if(units(emi_list[["emi"]][[i]])$numerator != "g" & 
       units(emi_list[["emi"]][[i]])$numerator != "kg"){
      stop("Incorrect 'units'. Emissions 'emi' needs to have 'units' in 'g' or 'kg'.")
    }
  })
  
  #
  # get units-----
  #
  
  myunits <- sapply(seq_along(emi_list[["emi"]]),function(i){
    units::deparse_unit(emi_list[["emi"]][[i]])
  }) 
  myunits <- unique(myunits)
  
  #  aggregate --------
  
  # variables to use
  if(by == "time")  {
    my_var <- "timestamp_hour"
    emi_list[[my_var]] <- data.table::hour(
      emi_list[[segment_vars]][["timestamp"]]
    ) 
  }
  if(by == "vehicle")  my_var <- veh_vars 
  
  
  # to DT
  tmp_dt <- emis_to_dt(emi_list = emi_list
                       , emi_vars = "emi"
                       , veh_vars = veh_vars
                       , pol_vars = "pollutant"
                       , segment_vars =  if(by == "time"){my_var}else{NULL}
  )
  
  # perform sum
  if(by == "pollutant"){
    tmp_dt <- tmp_dt[,lapply(.SD,sum,na.rm = TRUE)
                     ,by = c("pollutant")
                     ,.SDcols = c("emi")]
  }else{
    tmp_dt <- tmp_dt[,lapply(.SD,sum,na.rm = TRUE)
                     ,by = c(my_var,"pollutant")
                     ,.SDcols = c("emi")]
  }
  
  
  return(tmp_dt)
}
