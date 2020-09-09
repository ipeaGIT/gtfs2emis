#' @title Estimate hot exhaust emissions 
#'
#' @description Calculate hot exhaust emissions.
#'
#' @param fleet_composition vector; Fleet composition.
#' @param dist units ('km'); Length of each link in km.
#' @param ef units ('g/km');	List of functions of emission factors.
#' @param aggregate logical; if TRUE (default) emissions are aggregated by pollutant.
#' @param prefix character; Add prefix into emissions names. Missing parameter (default)
#' means empty prefix.
#' @return units ('g'); emissions per link.
#' @export
emis <- function(fleet_composition, dist, ef, aggregate = TRUE, prefix){
  # fleet_composition = total_fleet$fleet_composition
  # dist = spo_gpslines$dist
  # ef = EF_brazil
  # prefix = ""
  # 
  # check units----
  
  if(class(dist) != "units"){
    stop("dist neeeds to has class 'units' in 'km'. Please, check package 'units'")
  }
  if(units(dist)$numerator != "km"){
    stop("dist need to has 'units' in 'km'.")
  }
  
  # ef----
  
  # check if ef is constant
  if(length(ef[[1]]) == 1){
    message("Constant emission factor along the route")
  }
  
  # units
  lapply(seq_along(ef), function(i){ # i = 1
    if(class(ef[[i]]) != "units"){
      stop("ef neeeds to has class 'units' in 'g/km'. Please, check package 'units'")
    }
    if(units(ef[[i]])$numerator != "g" | units(ef[[i]])$denominator != "km"){
      stop("ef need to has 'units' in 'g/km'.")
    }
    # check 'ef' length with veh
    if(length(ef[[i]]) != 1 & length(ef[[i]]) != length(dist)){
      stop(paste0("ef '", names(ef[i]), "' needs to has the same length of dist"))
    }
  })
  
  # extract single pollutants----
  
  # verify how many pollutants 'ef' data has based on names(ef)
  m <- regexpr(pattern = "\\_", text = colnames(ef), fixed = FALSE)
  # get unique pollutants
  single_pol <- sapply(regmatches(x = colnames(ef), m = m, invert = TRUE), function(i){i[[1]]}) %>% 
    unique()
  
  # match 'veh' size based on different pollutant----
  
  fleet_composition <- rep(fleet_composition, length(single_pol))
  
  # emissions----
  
  emi <- lapply(seq_along(ef), function(i){ # i = 1 
    aux <- ef[[i]] * fleet_composition[[i]]
    temp_emis <- aux * dist
    return(temp_emis)
  })

  emi <- do.call(cbind, emi) %>% data.table::as.data.table()
  colnames(emi) <- colnames(ef)
  
  # aggregate condition----
  
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
  
  # prefix condition----
  
  if(missing(prefix) == FALSE){
    colnames(emi) <- paste0(prefix, "_", colnames(emi))
  }
  
  # units convertion----
  
  to_units <- function(i){units::set_units(i, "g")} # not sure why couldn't use it within data.table
  emi <- emi[, lapply(.SD, to_units)]
  return(emi)
}
