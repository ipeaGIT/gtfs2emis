#' @title Running exhaust emissions factors from EMFAC2017 model
#' 
#' @description Emissions that come out of the vehicle tailpipe while the vehicle is traveling on the road. The function returns emissions factor in units
#' 'g/km'
#'
#' @param pol Character; Pollutants: Carbon monoxide (CO); Nitrogen oxides (NOx); 
#'   Hydrocarbons as TOG (total organic gases), ROG (reactive organic gases), 
#'   THC (total hydrocarbon), or CH4 (methane); 
#'   Particulate matter as particulate matters 10 microns or less in diameter (PM10), 
#'   and particulate matters 2.5 microns or less in diameter (PM2.5);
#'   Sulfur oxides (SOx); Carbon Dioxide (CO2); 
#'   Nitrous Oxide (N2O) and Methane (CH4).
#' @param calendar_year Numeric; Calendar Year between 2010 - 2020
#' @param fuel Character; Type of fuel: 'Diesel','Gasoline','Natural Gas'. Default is 'Diesel'.
#' @param model_year Numeric; Model year of vehicle.
#' @param speed Units; Speed in 'km/h'; Emission factor are returned in speed intervals 
#'  such as "5-10","10-15","15-20","20-25","25-30","30-35","35-40","40-45","45-50"
#'   "50-55","55-60","60-65","65-70","70-75","75-80","80-85","85-90".">90" mph (miles/h)
#' @param veh numeric; proportion of circulating fleet according to model_year. It has to be 
#' the same length as model_year.
#' @param aggregate does the emission factor should be aggregated?
#' @return Data.table; Emission factors in units 'g/km' by speed and model_year
#' @source \url{https://arb.ca.gov/emfac/}
#' @export
ef_emfac <- function(pol,calendar_year,model_year,speed,veh = NULL,aggregate = TRUE,fuel = 'Diesel'){
  #
  # Filter calendar
  temp_emfac <- emfac[`Calendar Year` %in% as.character(calendar_year) &
                        Fuel %in% fuel,]
  #
  # check veh class
  if(class(veh) != 'numeric'){stop("'veh' class must be in numeric format.")}
  #
  # Filter pollutant
  ef_pol <- lapply(pol, function(p){ 
    #
    # p = pol[1]
    temp_emfac <- temp_emfac[Pollutant %in% p,]
    
    #
    # check condition
    if(dim(temp_emfac)[1] == 0){
      stop("Emission Factor do not exist. \n Please check `data(emfac)` for valid emission factors.")
    }
    
    #
    # Filter Model Year
    temp_ef <- sapply(model_year,function(i){ 
      
      # i = model_year[1]
      temp_emfac <- temp_emfac[`Model Year` %in% as.character(i),]
      
      #
      # check condition (2)
      if(dim(temp_emfac)[1] == 0){
        stop("Emission Factor do not exist. \n Please check `data(emfac)` for valid emission factors.")
      }
      
      #
      # Filter Speed
      temp_speed <- rep(NA,length(speed))
      temp_order <- unique(emfac$upper_speed_interval)
      temp_order <- temp_order[order(temp_order,decreasing = TRUE)]
      for(t in temp_order){ # t = temp_order[1]
        temp_speed[which(as.numeric(speed) < t)] <- t
      }
      #temp_speed
      temp_model <- temp_emfac[sapply(temp_speed,
                                      function(i){which(upper_speed_interval %in% i)}),EF]
      return(temp_model)
    }) %>% data.table::as.data.table()
    
    #
    # aggregate
    if(aggregate){
      if(is.null(veh)){stop("Veh file is missing")}else{
        #
        # average ef
        temp_ef_avg <- lapply(seq_along(veh),function(k){ # k = 1
          temp_ef[,.SD,.SDcols=c(k)] * veh[k]
          }) 
        temp_ef_avg <- do.call(cbind,temp_ef_avg) %>% rowSums() %>% 
          units::set_units('g/km') %>% data.table::as.data.table()
        names(temp_ef_avg) <- paste0(p,"_avg")
        
        return(temp_ef_avg)
      }
    }else{
      #
      # do not aggregate emission factors
      names(temp_ef) <- paste0(p,"_",model_year)
      return(temp_ef)
    }
  }) 
  #
  # return in a data.table like format
  ef_final <- do.call(cbind,ef_pol)
  return(ef_final)
}
