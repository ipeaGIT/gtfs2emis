#' @title 
#' Summarize emissions estimates
#'
#' @description 
#' Summarize emissions estimates, aggregating emissions by pollutant, time of the
#' day, vehicle type or road segment (spatial).
#'
#' @param emi_list List; Emission or emission factor list.
#' @param by Character; Emissions can be aggregated by 'time', 'veh_type', 
#'        'segment_vars', or simply 'pollutant' (Default). 
#' @param emi_vars Character; Names of 'emi_list' object attributed to emissions. 
#'        Default is 'emi'.
#' @param veh_vars Character; Names of 'emi_list' object attributed to vehicle
#'        characteristics. Default is 'veh_type'.
#' @param pol_vars Character; Names of 'emi_list' object attributed to pollutants.
#'        Default is 'pollutant'
#' @param segment_vars Character; Names of 'emi_list' object attributed to the
#'        road segments. Default is NULL.
#' @param time_column vector; Required if `by = 'time'` is selected.
#'        Column name with time stamp information provided in 'segment_vars' 
#'        object of emi_list'. 
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
#' emis_summary(emi_list, by = "pollutant") 
#' 
#' # by vehicle type
#' emis_summary(emi_list, by = "veh_type")
#'              
#' emis_summary(emi_list
#'              , by = "veh_type"
#'              , veh_vars = c("euro"))
#'
#' emis_summary(emi_list
#'              , by = "veh_type"
#'              , veh_vars = c("fuel"))
#'
#' emis_summary(emi_list
#'              , by = "veh_type"
#'              , veh_vars = c("veh_type","euro","tech","fuel"))
#'              
#' # by time of the day
#' emis_summary(emi_list
#'              , by = "time"
#'              , segment_vars = "tp_model"
#'              , time_column = "timestamp") # emi_list$tp_model$timestamp
#' 
#'}
emis_summary <- function(emi_list, 
                         by = "pollutant", 
                         emi_vars = 'emi', 
                         veh_vars = "veh_type", 
                         pol_vars = "pollutant", 
                         segment_vars = NULL, 
                         time_column = NULL){
  
  # fort <- gtfs2gps::read_gtfs(system.file("extdata/fortaleza.zip"
  #                                         , package = "gtfs2gps"))  %>%
  #   gtfs2gps::filter_single_trip() %>% 
  #   gtfs2gps::filter_by_shape_id("shape804-I") %>% 
  #   gtfs2gps::gtfs2gps() %>% 
  #   gtfs2gps::adjust_speed() %>% 
  #   gtfs2gps::gps_as_sflinestring()
  # 
  # fort
  # ef <- ef_europe_emep(speed = fort$speed,
  #                      veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
  #                      euro = c("IV","V"),
  #                      pollutant = c("CO2","NOx"),
  #                      fuel = "D" ,
  #                      tech =  c("SCR","EGR"),
  #                      slope = 0.0,
  #                      load = 0.5,
  #                      fcorr = 1,
  #                      as_list = TRUE)
  # 
  # emi <- emis(fleet_composition =  c(0.7,0.3),
  #             dist = units::set_units(fort$dist,"km"),
  #             ef = ef,
  #             aggregate = FALSE,
  #             as_list = TRUE)  
  # 
  # emi$gps <- fort
  # names(emi)
  # 
  # emi_list = emi
  # by = "time"; by = "pollutant"; by = "veh_type"
  # emi_vars = "emi"
  # veh_vars = "veh_type"
  # pol_vars = "pollutant"
  #segment_vars = "gps"
  #time_column = "timestamp"
  
  
  #
  # check list condition -----
  #
  
  if(class(emi_list) != "list"){
    stop("'emi_list' input needs to have a list format.")
  }
  
  # check consistencies----
  
  if(by != "time" & by != "pollutant"  & by != "veh_type"){
    stop("'by' argument needs to be 'time', 'veh_type' or 'pollutant' type")
  }
  if(by == "time"){
    if(missing(segment_vars)){
      stop("'segment_vars' argument is needed when by = 'time' is assigned")
    }
    if(missing(time_column)){
      stop("'time_column' argument is needed when by = 'time' is assigned")
    }
    if(!(time_column %in% names(emi_list[[segment_vars]]))){
      stop(sprintf("'%s' argument is needed in '%s' data"
                   ,time_column,segment_vars))
    }
    if(length(emi_list[[segment_vars]][[time_column]]) != nrow(emi_list[[emi_vars]])){
      stop("'emi_vars' columns needs to have the same length of 'time_column'")
    }
    if(is.null(emi_vars) | is.null(pol_vars)){
      stop("'emi_vars' or 'pol_vars' are missing for 'time' post processing")
    }
  }
  if(by == "pollutant"){
    if(is.null(pol_vars)){
      stop("pol_vars are missing for 'pollutant' post processing")
    }
    segment_vars <- NULL
  }
  if(by == "veh_type"){
    lapply(veh_vars,function(i){
      if(is.null(i) | is.null(pol_vars)){
        stop("'veh_vars' or 'pol_vars' are missing for 'vehicle type' post processing")
      }
    })
    segment_vars <- NULL
  }
  
  #
  # check 'emi_vars' units-----
  #
  
  lapply(seq_along(emi_list[[emi_vars]]),function(i){
    if(class(emi_list[[emi_vars]][[i]]) != "units"){
      stop("emi neeeds to has class 'units'. Please, check package 'units'")
    }
    if(units(emi_list[[emi_vars]][[i]])$numerator != "g" & 
       units(emi_list[[emi_vars]][[i]])$numerator != "kg"){
      stop("emi needs to has 'units' in 'g' or 'kg'.")
    }
  })
  
  #
  # get units-----
  #
  
  myunits <- sapply(seq_along(emi_list[[emi_vars]]),function(i){
    units::deparse_unit(emi_list[[emi_vars]][[i]])
  }) 
  myunits <- unique(myunits)
  
  #  aggregate --------
  
  # variables to use
  if(by == "time")  {
    my_var <- sprintf("%s_hour",time_column)
    emi_list[[my_var]] <- data.table::hour(
      emi_list[[segment_vars]][[time_column]]
    ) 
  }
  if(by == "veh_type")  my_var <- veh_vars 
  
  
  # to DT
  tmp_dt <- emis_to_dt(emi_list = emi_list
                       , emi_vars = emi_vars
                       , veh_vars = veh_vars
                       , pol_vars = pol_vars
                       , segment_vars =  if(by == "time"){my_var}else{NULL}
  )
  
  # perform sum
  if(by == "pollutant"){
    tmp_dt <- tmp_dt[,lapply(.SD,sum,na.rm = TRUE)
                     ,by = c(pol_vars)
                     ,.SDcols = c(emi_vars)]
  }else{
    tmp_dt <- tmp_dt[,lapply(.SD,sum,na.rm = TRUE)
                     ,by = c(my_var,pol_vars)
                     ,.SDcols = c(emi_vars)]
  }
  
  
  return(tmp_dt)
}
