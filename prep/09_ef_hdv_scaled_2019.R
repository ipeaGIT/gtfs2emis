ef_hdv_scaled_2019 <- function(dfcol,vel,ef,veh,fuel,segment,euro,SDC = 34.12,tech = "SCR",pol,slope = 0.0,load = 0.5,show.equation = TRUE){
  #
  # adjust
  #
  if (length(dfcol) != length(euro)) 
    stop("Length of dfcol must be the same as length of eu")
  dfcol <- as.numeric(dfcol)
  la <- lapply(1:length(dfcol), function(i) {
    funIN <- ef_hdv_speed_2019(vel = vel,ef = ef,veh = "Buses",
                               segment = segment,fuel = "Diesel",
                               euro = euro,
                               tech = tech,pol = pol,show.equation = FALSE)
    k <- dfcol[i]/funIN
    ef_scaled <- ef_hdv_speed_2019(vel = SDC,ef = ef,veh = "Buses",
                      segment = segment,fuel = "Diesel",
                      euro = euro,
                      tech = tech,pol = pol,k = k,show.equation = FALSE)
    return(ef_scaled)
  }) 
  return(la)
}
