#' @title 
#' Estimate hot exhaust emissions 
#'
#' @description 
#' Calculate hot exhaust emissions.
#'
#' @param fleet_composition vector; Fleet composition, which is a distribution 
#'        of fleet based on frequency. If there is only one.
#' @param dist units ('km'); Length of each link in km.
#' @param ef list or data.table;	Emission factors.
#' @param aggregate logical; if TRUE (default) emissions are aggregated by pollutant.
#' @param prefix character; Add prefix into emissions names. Missing parameter (default)
#' means empty prefix.
#' @param as_list logical; if TRUE (default) emissions are returned inside 'ef' list.
#' 
#' @return units ('g'); emissions per link.
#' 
#' @export
#' @examples
#' 
#'set.seed(1335)
#'dist = units::set_units(rnorm(100,0.250,0.03),"km")
#'ef <- ef_emep_europe(speed = units::set_units(rnorm(100,50,5),"km/h"),
#'               veh_type = c("Ubus Std 15 - 18 t","Ubus Artic >18 t"),
#'               euro = c("IV","V"),
#'               pollutant = c("CO2","NOx"),
#'               fuel = "D" ,
#'               tech =  c("SCR","EGR"),
#'               slope = 0.0,
#'               load = 0.5,
#'               fcorr = 1,
#'               as_list = TRUE)
#'
#'emi <- emis(fleet_composition =  c(0.7,0.3),
#'           dist = dist,
#'           ef = ef,
#'           aggregate = FALSE,
#'           as_list = TRUE)  
#'             
emis <- function(fleet_composition, dist, ef, aggregate = TRUE, prefix = NULL, as_list = TRUE){
  
  #
  # init---
  #
  
  # set.seed(1335)
  # fleet_composition = c(0.7,0.3)
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
  
  # 
  # check units----
  #
  
  
  if(class(dist) != "units"){
    stop("dist neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  if(units(dist)$numerator != "km"){
    stop("dist need to has 'units' in 'km'.")
  }
  
  # ef----
  
  # check if its a list
  if(class(ef) == "list"){
    tmpEf <- ef$EF
  }else{
    tmpEf <- ef
  }
  # check if ef is constant
  if(length(tmpEf[[1]]) == 1){
    message("Constant emission factor along the route")
  }
  
  # units
  lapply(seq_along(tmpEf), function(i){ # i = 1
    if(class(tmpEf[[i]]) != "units"){
      stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
    }
    if(units(tmpEf[[i]])$numerator != "g" | units(tmpEf[[i]])$denominator != "km"){
      stop("ef need to has 'units' in 'g/km'.")
    }
    # check 'tmpEf' length with veh
    if(length(tmpEf[[i]]) != 1 & length(tmpEf[[i]]) != length(dist)){
      stop(paste0("ef '", names(tmpEf[i]), "' needs to has the same length of dist"))
    }
  })
  
  # extract single pollutants----
  
  if(class(ef) == "list"){
    single_pol <- unique(ef$pollutant)
  }else{
    # verify how many pollutants 'ef' data has based on names(ef)
    m <- regexpr(pattern = "\\_", text = colnames(ef), fixed = FALSE)
    # get unique pollutants
    single_pol <- sapply(regmatches(x = colnames(ef), m = m, invert = TRUE), function(i){i[[1]]}) %>% 
      unique()
  }
  
  # match 'veh' size based on different pollutant----
  
  fleet_composition <- rep(fleet_composition, length(single_pol))
  
  # emissions----
  
  emi <- lapply(seq_along(tmpEf), function(i){ # i = 1 
    temp_emis <- tmpEf[[i]] * fleet_composition[[i]] * dist
    return(temp_emis)
  })
  
  emi <- do.call(cbind, emi) %>% data.table::as.data.table()
  colnames(emi) <- colnames(tmpEf)
  
  #
  # aggregate condition----
  #
  
  if(aggregate){
    # aggregate by single pollutant
    lapply(single_pol, function(i){ # i = single_pol[1]
      colnames_temp <- colnames(emi)[colnames(emi) %like% paste0(i, "_")]
      names_newcol <- paste0(i, "_total")
      emi[, (names_newcol) := rowSums(.SD), .SDcols = colnames_temp]
    })
    # keep only total_emissions
    colnames_avg <- colnames(emi)[colnames(emi) %like% "_total"]
    emi <- emi[, .SD, .SDcols = colnames_avg]
  }
  
  # prefix condition ----
  
  if(missing(prefix) == FALSE){
    colnames(emi) <- paste0(prefix, "_", colnames(emi))
  }
  
  # units conversion ----
  
  emi <- emi[, lapply(.SD, units::set_units,'g')]
  
  # export ---
  
  if(as_list & class(ef) == "list"){
    ef$emi <- emi
    return(ef)
  }else{
    return(emi)
  }
  
}
