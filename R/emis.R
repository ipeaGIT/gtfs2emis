#' @title Estimate hot exhaust emissions 
#'
#' @description Calculate hot exhaust emissions
#'
#' @param veh Vehicles data-frame or list of "Vehicles" data-frame.
#' Each data-frame as number of columns matching the age distribution of that ype of vehicle. The number of rows is equal to the number of streets link. If this is a list, the length of the list is the vehicles for each hour.
#' @param dist Units (in km); Length of each link in km
#' @param ef Units (in g/km);	List of functions of emission factors
#' @return Units (in g); emissions per link
#' @export
emis <- function(veh,dist,ef){
  #
  # check dist units
  if(class(dist) != "units"){
    stop("dist neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  if(units(dist)$numerator != "km"){
    stop("dist need to has 'units' in 'km'.")
  }
  
  #
  # check ef units
  #if(class(ef) != "list"){
  #  stop("ef should be presented in 'list' class. Please, try using 'list(ef)' instead.")
  #}
  lapply(seq_along(ef),function(i){ # i = 1
    if(class(ef[[i]]) != "units"){
      stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
    }
    if(units(ef[[i]])$numerator != "g" | units(ef[[i]])$denominator != "km"){
      stop("ef need to has 'units' in 'g/km'.")
    }
    
    #
    # check 'ef' length with veh
    if(length(ef[[i]]) != length(dist)){
      stop(paste0("ef '",names(ef[i]),"' needs to has the same length of dist"))
    }
  })
  # emissions
  emi <- lapply(seq_along(ef),function(i){ # i = 1 
    # ef into matrix (efm)
    #efm <- matrix(ef[[i]])
    # estimate emissions
    aux <- ef[[i]] * veh[[i]]
    temp_emis <- aux * dist
    return(temp_emis)
  }) 
  emi1 <- do.call(cbind,emi) %>% units::set_units("g")
  names(emi1) <- names(ef)
  return(emi1)
}
