#' @title 
#' Convert emissions estimates from list to data.table format
#' 
#' @description 
#' Read emissions in list format and export to data.table format.
#' 
#' @param emi_list List; Emissions list.
#' @param emi_vars Character; Name of emissions data on 'emi_list'
#' @param veh_vars Character; Attributes of vehicle data on 'emi_list'
#' @param pol_vars Character; Attributes of pollutants data on 'emi_list'
#' @param segment_vars Character; Attributes of segment link data on 'emi_list'
#' 
#' @return data.table.
#' @export
#' 
#' @examples 
#' set.seed(1335)
#' dist = units::set_units(rnorm(100,0.250,0.03),"km")
#' ef <- ef_brazil(pollutant = c("CO2","NOx"),
#'                 veh_type = "BUS_URBAN_D", 
#'                 model_year = 2016)
#' 
#' ef <- ef_emep_europe(speed = units::set_units(rnorm(100,50,5),"km/h"),
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
#'             dist = dist,
#'             ef = ef,
#'             aggregate = FALSE,
#'             as_list = TRUE)
#'               
#' # to data.table
#' dt <- emi_to_dt(emi_list = emi,
#'                 emi_vars = "emi", # "emi" var
#'                 veh_vars = c("veh_type","euro","fuel","tech"), # "veh_type"
#'                 pol_vars = "pollutant", # pollutant
#'                 segment_vars = c("slope","load")) # NULL
#' 
emi_to_dt <- function(emi_list, emi_vars, veh_vars, pol_vars, segment_vars = NULL){
  
  #
  # init config
  #
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
  #                 as.list = TRUE)
  # emi_list <- emis(fleet_composition =  c(0.7,0.3),
  #             dist = dist,
  #             ef = ef,
  #             aggregate = FALSE,
  #             as_list = TRUE)
  # 
  # names(emi_list)
  # emi_vars = "emi"
  # veh_vars = c("veh_type","euro","fuel","tech")
  # pol_vars = "pollutant"
  # segment_vars = NULL #c("slope","load")
  # 
  all_vars = c(veh_vars, pol_vars,emi_vars)
  
  
  #
  # check units previously
  #
  
  myunits <- sapply(seq_along(emi_list[[emi_vars]]),function(i){
    units::deparse_unit(emi_list[[emi_vars]][[i]])
  })
  
  if(length(unique(myunits)) == 1){
    myunits <- myunits[1]
  }else{
    message("units are not the same for emissions columns")
    stop()
  }
  
  #
  # merge------
  #
  
  dt <- lapply(1:length(emi_list[[pol_vars]]),function(i){ # i = 1
    
    # add vars
    tmp_dt <- lapply(seq_along(all_vars),function(j){ # j = 1;i = 1
      emi_list[[all_vars[[j]]]][[i]]
    })
    if(!is.null(segment_vars)){
      tmp_dt <- do.call(c, list(tmp_dt, emi_list[segment_vars]))
      tmp_dt <- do.call(cbind,tmp_dt) %>% data.table::as.data.table()
      names(tmp_dt) <- c(all_vars,segment_vars)
    }else{
      tmp_dt <- do.call(cbind,tmp_dt) %>% data.table::as.data.table()
      names(tmp_dt) <- all_vars
    }
    
    
    # convert columns
    
    tmp_dt[,segment_id := 1:nrow(tmp_dt)]
    if(!is.null(emi_vars))      tmp_dt[,(emi_vars) := lapply(.SD, as.numeric), .SDcols = emi_vars]
    if(!is.null(veh_vars))      tmp_dt[,(veh_vars) := lapply(.SD, as.factor), .SDcols = veh_vars]
    if(!is.null(segment_vars))  tmp_dt[,(segment_vars) := lapply(.SD, as.numeric), .SDcols = segment_vars]
    if(!is.null(pol_vars))      tmp_dt[,(pol_vars) := lapply(.SD, as.character), .SDcols = pol_vars]
    
    return(tmp_dt)
  }) %>% data.table::rbindlist()
  
  # add units back ----
  
  units(dt[[emi_vars]]) <- myunits
  
  # return
  
  return(dt)
}