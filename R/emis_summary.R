#' @title Emissions summary by aggregating results 
#'
#' @description Aggregate emissions generated by emis function by time, 
#' vehicle type and segment link (spatial).
#'
#' @param emi data.table; Data.table with emissions and departure_time data.
#' @param emi_vars character; Columns names of emissions information.
#' @param by character; Emissions can be aggregated by 'time', 'age', 'veh_type' and 'pollutant'
#' @param time_column vector; Vector containing time_column. 
#' @param veh_vars vector; Vector containing variables of vehicle type.
#' @param pol_vars vector; Vector containing variables of pollutant. 
#'  Only used when 'hour' or 'hour-minute' is selected. 
#' @return units ('g'); emissions.
#' @export
#' @examples 
#' set.seed(1335)
#' dist = units::set_units(rnorm(100,0.250,0.03),"km")
#' ef <- ef_europe(speed = units::set_units(rnorm(100,50,5),"km/h"),
#'                 veh_type = c("Urban Buses Standard 15 - 18 t","Urban Buses Articulated >18 t"),
#'                 euro = c("IV","V"),
#'                 pollutant = c("CO2","NOx"),
#'                 fuel = "Diesel" ,
#'                 tech =  c("SCR","EGR"),
#'                 slope = 0.0,
#'                 load = 0.5,
#'                 fcorr = 1,
#'                 as_list = TRUE)
#' 
#' emi <- emis(fleet_composition =  c(0.7,0.3),
#'             dist = dist,
#'             ef = ef,
#'             aggregate = FALSE,
#'             as_list = TRUE)  
#' 
#' time_column <- data.table::as.ITime(seq(1,14400,length.out = 100))
#
#' my_emis_summary <- emis_summary(emi = emi,
#'                      emi_var = "emi", 
#'                      by = "time",  # veh_type, time, pollutant 
#'                      time_column = time_column,
#'                      veh_var = "veh_type", # veh_type
#'                      pol_var = "pollutant") # pollutant
emis_summary <- function(emi, emi_vars, by, time_column = NULL,pol_vars = NULL, veh_vars = NULL){
  
  #
  # init config
  # 
  
  # set.seed(1335)
  # dist = units::set_units(rnorm(100,0.250,0.03),"km")
  # ef <- ef_europe(speed = units::set_units(rnorm(100,50,5),"km/h"),
  #                 veh_type = c("Urban Buses Standard 15 - 18 t","Urban Buses Articulated >18 t"),
  #                 euro = c("IV","V"),
  #                 pollutant = c("CO2","NOx"),
  #                 fuel = "Diesel" ,
  #                 tech =  c("SCR","EGR"),
  #                 slope = 0.0,
  #                 load = 0.5,
  #                 fcorr = 1,
  #                 as_list = TRUE)
  # emi <- emis(fleet_composition =  c(0.7,0.3),
  #             dist = dist,
  #             ef = ef,
  #             aggregate = FALSE,
  #             as_list = TRUE)
  # rm(ef)
  # emi_vars = "emi"
  # pol_vars = "pollutant"
  # by = "time"
  # time_column <- gtfs2gps::read_gtfs(system.file("extdata/saopaulo.zip", package = "gtfs2gps")) %>%
  #   gtfs2gps::filter_by_shape_id("52736") %>%
  #   gtfs2gps::gtfs2gps() 
  # time_column <- time_column[!is.na(departure_time),]
  # time_column <- time_column[seq(1,nrow(time_column),length.out = 100),departure_time]
  # 
  # emi_vars = "emi"; by = "time";time_column = time_column; pol_vars = "pollutant"
  
  #
  # check if it's a list
  #
  
  if(class(emi) != "list"){
    stop("emi input needs to have a list format.")
  }
  
  #
  # check consistencies
  #
  
  if(by != "time" & by != "pollutant"  & by != "veh_type"){
    stop("'by' argument needs to be 'time', 'veh_type' or 'pollutant' type")
  }
  if(by == "time"){
    if(missing(time_column)){
      stop("'time_column' argument is required when by = 'time' is assigned")
    }
    if(length(time_column) != nrow(emi[[emi_vars]])){
      stop("emissions columns needs to have the same length of time_column")
    }
    if(missing(emi_vars) | missing(pol_vars)){
      stop("emi_vars or pol_vars are missing for 'time' post processing")
    }
  }
  if(by == "pollutant"){
    if(missing(pol_vars)){
      stop("pol_vars are missing for 'pollutant' post processing")
    }
  }
  if(by == "veh_type"){
    if(missing(veh_vars) | missing(pol_vars)){
      stop("pol_vars are missing for 'pollutant' post processing")
    }
  }
  
  #
  # check 'emi_vars' units
  #
  
  lapply(seq_along(emi[[emi_vars]]),function(i){
    if(class(emi[[emi_vars]][[i]]) != "units"){
      stop("emi neeeds to has class 'units'. Please, check package 'units'")
    }
    if(units(emi[[emi_vars]][[i]])$numerator != "g" & units(emi[[emi_vars]][[i]])$numerator != "kg"){
      stop("emi needs to has 'units' in 'g' or 'kg'.")
    }
  })
  
  #
  # get units
  #
  
  myunits <- sapply(seq_along(emi[[emi_vars]]),function(i){
    units::deparse_unit(emi[[emi_vars]][[i]])
  }) %>% unique()
  
  #
  # aggregate by TIME
  #
  
  if(by == "time"){
    
    # time - filter
    tmp_dt <- data.table::copy(emi[[emi_vars]])
    tmp_dt[, time := data.table::as.ITime(time_column)]
    tmp_dt <- tmp_dt[order(time), ]
    tmp_dt[, time := data.table::hour(time)]
    
    # aggregate emissions and reorder
    tmp_dt <- tmp_dt[, lapply(.SD, sum), .SDcols = !("time"),
                     by =  time]
    newCols <- names(tmp_dt)[names(tmp_dt) %nin% "time"]
    data.table::setcolorder(tmp_dt,
                            neworder = c(newCols,"time"))
    
    # aggregate by pollutant
    single_pol <- unique(emi[[pol_vars]])
    for(j in seq_along(single_pol)){ # i = 2
      colPol <- colnames(tmp_dt)[colnames(tmp_dt) %like% paste0(single_pol[j],"_")]
      tmp_dt[, (single_pol[j]) := rowSums(.SD,na.rm = TRUE), .SDcols = colPol]
    }
    # export
    tmp_dt <- tmp_dt[,.SD, .SDcols = c("time",unique(single_pol))]
    # units
    tmp_dt[, (single_pol) := lapply(.SD, units::as_units,myunits),.SDcols = single_pol]
    
  }
  
  #
  # by 'veh_type' -----------
  #
  if(by == "veh_type"){
    
    temp_data <- data.table::copy(emi[[emi_vars]])
    # aggregate by pollutant
    single_pol <- unique(emi[[pol_vars]])
    tmp_dt <- lapply(seq_along(single_pol),function(i){ # i = 1
      # temp
      tmp_id <- which(emi[[pol_vars]] %in% single_pol[i])
      tmp_dt <- data.table::copy(temp_data)[,.SD,.SDcols = tmp_id]
      #
      colType <- unique(emi[[veh_vars]])
      tmp_dt <- lapply(1:length(colType),function(j){ # j = 1
        # select pollutant and veh_type 'id'
        id_type <- which(emi[[veh_vars]] %in% colType[j]) 
        id_type <- which(id_type %in% tmp_id)
        
        tmp_dt[,(emi_vars) := rowSums(.SD), .SDcols = (id_type)]
        tmp_dt[,(veh_vars) := colType[j]]
        tmp_dt[,(pol_vars) := single_pol[i]]
        # add segment id
        tmp_dt[,segment_id := .GRP]
        # remove unecessary columns
        tmp_dt <- tmp_dt[,.SD, .SDcols = c("segment_id",veh_vars,pol_vars,emi_vars)]
        # 
        return(tmp_dt)
        
      }) %>% data.table::rbindlist()
      
      return(tmp_dt)
    }) %>% data.table::rbindlist()
    # back to units
    tmp_dt[, (emi_vars) := lapply(.SD, units::as_units,myunits),.SDcols = emi_vars]
    
  }
  
  #
  # by 'pollutant'
  #
  if(by == "pollutant"){
    
    temp_data <- data.table::copy(emi[[emi_vars]])
    # aggregate by pollutant
    single_pol <- unique(emi[[pol_vars]])
    tmp_dt <- lapply(seq_along(single_pol),function(i){ # i = 2
      # temp
      tmp_id <- which(emi[[pol_vars]] %in% single_pol[i])
      tmp_dt <- data.table::copy(temp_data)
      tmp_dt[,(emi_vars) := rowSums(.SD), .SDcols = (tmp_id)]
      tmp_dt <- tmp_dt[,.SD,.SDcols = emi_vars]
      tmp_dt[,(pol_vars) := single_pol[i]]
      # add segment id
      tmp_dt[,segment_id := .GRP]
      # remove unecessary columns
      data.table::setcolorder(tmp_dt,neworder = c("segment_id",pol_vars,emi_vars))
      # 
      return(tmp_dt)
    }) %>% data.table::rbindlist()
    # back to units
    tmp_dt[, (emi_vars) := lapply(.SD, units::as_units,myunits),.SDcols = emi_vars]
    
  }
  
  # return
  return(tmp_dt)
}