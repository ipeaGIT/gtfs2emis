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
#' vehdt <- data.table::data.table("veh1" = 0.75,"veh2" = 0.25)
#' efdt <- list("CO"=units::set_units(c(0.25,0.17),'g/km'))
#' distdt <- units::set_units(rep(1,10),'km')
#' emis(veh = vehdt, dist = distdt, ef = efdt)
#' @export
emis <- function(veh,dist,ef){
  # dist <- units::set_units(rep(2,10),"km")
  # ef <- list("CO"=set_units(rep(1,5),'g/km'),"MP" = set_units(rep(-1,5),'g/km'))
  # veh <- data.table("veh1" = 0.2,"veh2" = 0.2,"veh3" = 0.2,"veh4" = 0.2,"veh5" = 0.2)
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
  for(i in seq_along(ef)){ # i = 1
    if(class(ef[[i]]) != "units"){
      stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
    }
    if(units(ef[[i]])$numerator != "g" | units(ef[[i]])$denominator != "km"){
      stop("ef need to has 'units' in 'g/km'.")
    }
  }

   # disagregated emissions
  emi <- lapply(seq_along(list(ef)),function(i){ # i = 1 
    # ef into matrix (efm)
    efm <- matrix(ef[[i]])
    # check length  
    if(nrow(efm) != length(veh)){
      stop("Length of ef is different than veh. \n Lengths should be the same.")
    }
    # estimate emissions
    aux <- efm * as.numeric(veh)
    temp_dist <- as.numeric(dist)
    temp_emis <- aux %*% t(matrix(temp_dist))
    row.names(temp_emis) <- names(veh)
    return(temp_emis)
  }) 
  names(emi) <- names(ef)
  return(emi)
}
