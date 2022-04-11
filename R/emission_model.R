#' @title Emissions model 
#' 
#' @description Estimate hot-exhaust emissions from the input transport model
#' 
#' @param gps Character or sf_linestring; Filepath with the GPS files in sf_linestring format or an sf_linestring data.
#' @param ef_data_base Character; Emission factor database, such as "moves_usa",
#' "emfac_usa", "emep_europe","cetesb_brazil". 
#' @param fleet_data Data.table. Database with fleet_data. The requires columns 
#' depends on the ef_data_base source. See @examples for input.  
#' @param output_path Character; Filepath to export emissions.
#' @param pollutant Vector; Vector of characters of pollutants. character; The available 
#' pollutants depends on the ef_data_base. See `ef_usa_moves`,`ef_usa_emfac`,
#' `ef_brazil_cetesb` or `ef_emep_europe` functions for details. 
#' @param parallel Logical; Decides whether the function should run in parallel. 
#' Defaults is FALSE.When TRUE, it will use all cores available minus one 
#' using future::plan() with strategy "multisession" internally. 
#' @details In the fleet database..
#' - Column `calendar_year`: Character; Base year of the @ef_data_base input. 
#' Required only when  `usa_moves` or `usa_emfac` are selected.
#' - Column `tech`  Character; After treatment technology, classified in "SCR" 
#' (Selective Catalytic Reduction), "EGR" (Exhaust Gas Recirculation), and 
#' "DPF+SCR" (Diesel Particulate Filter + SCR, for Euro VI). Default is "SCR" 
#' for "IV" and "V". This is required only when `emep_europe` is selected.
#' - Column `euro`  character; Euro period of vehicle, classified in 
#' "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV". This is required 
#' only when `ef_emep_europe` or `ef_euro_scaled` are selected.
#' - Column `fuel` character; Fuel type, classified in "D" (Diesel)
#' ,"DHD" (Diesel Hybrid ~ Diesel), "DHE" (Diesel Hybrid ~ Electricity)
#' , "CNG" (Compressed Natural Gas), "BD" (Biodiesel). 
#' @param tech character; After treatment technology, classified in "SCR"
#'  (Selective Catalytic Reduction), 
#' @return NULL
#' 
#' @export
#' @examples      |
emission_model <- function(gps
                           ,ef_data_base
                           ,fleet_data
                           ,pollutant
                           ,output_path
                           ,parallel
                           ,calendar_year){
  # Checking inputs -----
  
  message("Checking input data")
  
  
  # Fleet 
  message("   Fleet data...")
  if(!(ef_data_base %in% c("cetesb_brazil","emfac_usa","moves_usa","emep_europe"))){
    stop("'ef_data_base' has to me one of the following 'cetesb_brazil',\n'emfac_usa','moves_usa', or 'emep_europe'")
  }
  if(ef_data_base == "cetesb_brazil"){
    if(is.null(fleet_data$veh_type) | is.null(fleet_data$model_year)
       | is.null(fleet_data$fleet_composition)){
      stop(sprintf("Arguments 'veh_type','fleet_composition', and 'model_year' input is required in the 'fleet_data' for %s database")
           ,ef_data_base)
    }
  }
  if(ef_data_base == "emfac_usa" | ef_data_base == "moves_usa"){
    if(is.null(fleet_data$veh_type) | is.null(fleet_data$model_year)
       | is.null(fleet_data$fleet_composition)){
      stop(sprintf("Arguments 'veh_type','fleet_composition', and 'model_year'  input \nis required  in the 'fleet_data' for %s database")
           ,ef_data_base)
    }
  }
  if(ef_data_base == "ef_emep_'"){
    if(is.null(fleet_data$speed) | is.null(fleet_data$euro)
       | is.null(fleet_data$fleet_composition)){
      stop(sprintf("Arguments 'speed','euro','fleet_composition', and 'speed' input \nis required  in the 'fleet_data' for %s database"
                   ,ef_data_base))
    }
  }
  
  
  message("   Transport model")
  
  if(is.character(gps) & !missing(output_path)){
    stop("User should provide 'gps' and 'output_path' if wants to save emission into a provided folder.")
  }
  
  message("Emissions estimates")
  
  # EF ----------------
  
  if(ef_data_base == "cetesb_brazil"){
    temp_ef <- ef_brazil(pollutant = pollutant,
                         veh_type = fleet_data$veh_type,
                         model_year = as.numeric(fleet_data$model_year),
                         as_list = TRUE)
  }
  # Emission function -----
  emission_estimate <- function(gps){ # i = 1
    
    # i) Read GPS linestrings or DT ----
    if(is.character(gps)){
      tmp_gps <- readr::read_rds(gps)
    }else{
      tmp_gps <- gps
    }
    # ii) Get EF  ----
    if(ef_data_base == "emep_europe"){
      temp_ef <- ef_europe(pollutant = pollutant,
                           speed = tmp_gps$speed,
                           veh_type = fleet_data$veh_type,
                           tech = fleet_data$tech,
                           euro = fleet_data$euro,
                           fcorr = 1)
    }
    if(ef_data_base == "emfac_usa"){
      temp_ef <- ef_usa_emfac(pollutant = pollutant,
                              calendar_year = calendar_year,
                              model_year = fleet_data$model_year,
                              speed = tmp_gps$speed,
                              fuel = fleet_data$fuel)
    }
    if(ef_data_base == "moves_usa"){
      temp_ef <- ef_usa_moves(pollutant = pollutant,
                              calendar_year = calendar_year,
                              model_year = fleet_data$model_year,
                              speed = tmp_gps$speed,
                              fuel = fleet_data$fuel)
    }
    # iii) Emissions -----
    temp_emis <- emis(fleet_composition = fleet_data$fleet_composition
                      , dist = units::set_units(tmp_gps$dist,"km")
                      , ef = temp_ef
                      , aggregate = FALSE
                      , as_list = TRUE)
    # iv) Add data -----
    # Add fleet info
    if(ef_data_base != "emep_europe"){
      temp_emis$model_year = rep(fleet_data$model_year
                          ,data.table::uniqueN(temp_emis$pollutant))
    }else{
      temp_emis$euro = rep(fleet_data$euro
                           ,data.table::uniqueN(temp_emis$pollutant))
    }
    
    temp_emis$fleet_composition <- rep(fleet_data$fleet_composition
                                       ,data.table::uniqueN(temp_emis$pollutant))
    
    
    # Add EF to list
    temp_emis$EF = temp_ef$EF
    
    # Add gps data into emissions
    temp_emis$gps <- tmp_gps
    
    # v) Save emissions -----
    
    return(temp_emis)
  }
  # Loop emissions ----
  
  ## No output ----
  if(missing(output_path)){
    emisLine <- emission_estimate(gps)
    return(emisLine)
  }else{
    
    input_gps_names <- list.files(gps,full.names = FALSE)
    input_gps <- list.files(gps,full.names = TRUE)
    
    ## With output----
    emisLine <- furrr::future_map(.x = 1:length(input_gps)
                                  ,.f = function(i){
                                    tmp_emis <- emission_estimate(i)
                                    readr::write_rds(x = tmp_emis
                                                     ,file = sprintf("%s/%s",output_path,files_gps_names[i])
                                                     ,compress = "gz")
                                    return(NULL)
                                  } ,.options = furrr::furrr_options(seed = 123))
    return(NULL)
  }
}
# End---------