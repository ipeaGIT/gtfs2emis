#' @title Estimate hot exhaust emissions 
#'
#' @description Calculate hot exhaust emissions
#'
#' @param veh Vehicles data-frame or list of "Vehicles" data-frame.
#' Each data-frame as number of columns matching the age distribution of that ype of vehicle. The number of rows is equal to the number of streets link. If this is a list, the length of the list is the vehicles for each hour.
#' @param dist Units (in km); Length of each link in km
#' @param ef Units (in g/km);	List of functions of emission factors
#' @return Units (in g); emissions per link
#' @examples 
#' set.seed(12039)
#' emis(veh = data.table::data.table("veh1" = 0.75,"veh2" = 0.25),
#' dist = units::set_units(rnorm(10,mean = 8),'km'),
#' ef = list("veh1" = units::set_units(rnorm(10,0.35,0.01),'g/km'),
#' "veh2" = units::set_units(rnorm(10,0.27,0.01),'g/km')))
#' 
#' emis(veh = data.table::data.table("veh1" = 1),
#' dist = units::set_units(rnorm(10,mean = 8),'km'),
#' ef = list("veh1" = units::set_units(rnorm(10,0.35,0.01),'g/km')))      
#' @export
emis <- function(veh,dist,ef){
  # dist = units::set_units(rep(1,10),'km')
  # ef <- list("2005"= units::set_units(rep(1,10),'g/km'),
  #           "2006" = units::set_units(rep(-1,10),'g/km'),
  #           "2007" = units::set_units(rep(0,10),'g/km'))
  # veh <- data.table::data.table("veh1" = 0.2,"veh2" = 0.2,"veh3" = 0.2)
  # veh <- vehdt
  # check units
  #
  # dist
  if(class(dist) != "units"){
    stop("dist neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  if(units(dist)$numerator != "km"){
    stop("dist need to has 'units' in 'km'.")
  }
  # check ef units
  if(class(ef) != "list"){
    stop("ef should be presented in 'list' class. Please, try using 'list(ef)' instead.")
  }
  lapply(seq_along(ef),function(i){ # i = 1
    if(class(ef[[i]]) != "units"){
      stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
    }
    if(units(ef[[i]])$numerator != "g" | units(ef[[i]])$denominator != "km"){
      stop("ef need to has 'units' in 'g/km'.")
    }
    # check length with veh
    if(length(ef[[i]]) != length(dist)){
      stop(paste0("ef '",names(ef[i]),"' needs to has the same length of dist"))
    }
  })
  # check length of ef with veh
  if(length(ef) != length(veh)){
    stop("ef needs to has the same length of veh")
  }
  # emissions
  emi <- lapply(seq_along(ef),function(i){ # i = 1 
    # ef into matrix (efm)
    efm <- matrix(ef[[i]])
    # estimate emissions
    aux <- efm * as.numeric(veh[[i]])
    temp_emis <- aux * as.numeric(dist)
    return(temp_emis)
  }) 
  names(emi) <- names(ef)
  return(emi)
}
