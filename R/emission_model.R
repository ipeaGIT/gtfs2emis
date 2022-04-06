#' @title Emissions model 
#' 
#' @description Estimate hot-exhaust emissions from the input transport model
#' 
#' @param gps_line_path Character; Filepath with the GPS files as sf_linestring.
#' @param ef_data_base Character; Emission factor database, such as "moves_usa",
#' "emfac_usa", "emep_europe","cetesb_brazil". 
#' @param fleet_data Data.table. Database with fleet_data. The requires columns 
#' depends on the ef_data_base source. See @details for input examples.  
#' @param emis_path Character; Filepath to export emissions.
#' @param pollutant Vector; Vector of characters of pollutants. character; The available 
#' pollutants depends on the ef_data_base. See `ef_usa_moves`,`ef_usa_emfac`,
#' `ef_brazil_cetesb` or `ef_emep_europe` for details. 
#' @param load Numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' This is required only `ef_emep_europe` is selected.
#' @param as_list Logical; Returns emission factors as a list, instead of 
#' data.table format. Default is TRUE.
#' @param parallel Logical; Decides whether the function should run in parallel. 
#' Defaults is FALSE.When TRUE, it will use all cores available minus one 
#' using future::plan() with strategy "multisession" internally. 
#' @param workers Numeric; Number of workers available for parallel processing.    
#' @details In the fleet database..
#' - Column `calendar_year`: Character; Base year of the @ef_data_base input. 
#' Required only when  `ef_usa_moves` or `ef_usa_emfac` are selected.
#' - Column `tech`  Character; After treatment technology, classified in "SCR" 
#' (Selective Catalytic Reduction), "EGR" (Exhaust Gas Recirculation), and 
#' "DPF+SCR" (Diesel Particulate Filter + SCR, for Euro VI). Default is "SCR" 
#' for "IV" and "V". This is required only when `ef_emep_europe` or 
#' `ef_euro_scaled` are selected.
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
#' @examples 
# input parameters -----           
#                               #          Input               |    Source    | Req./ C.Req./ Def / NA |
#--------------------------------------------------------------|--------------|------------------------|#
# gps_line_path = input           #           Gps_line_path      |  "Character" |          Required      |
# ef_data_base =                  # cetesb_brazil/emfac_usa/../  |  "Character" |          Required      |
# fleet_data = data.frame input   #             Fleet            |  Fleet data  |          Required      |
# emis_path = "article/data/emis/"#          Emis path           |  "Character" |          Required      |
# pollutant =                     #         EF functions         |  "Character" |          Required      |
# calendar_year = "2019"          #      If emfac / moves        |  "Character" |         C.Required     |
# tech =                          #          ef_europe           |  Fleet data  |         C.Required     |
# euro =                          #          ef_europe           |  Fleet data  |         C.Required     |
# load = 0.5                      #          ef_europe           |  "Character" |          Default       |
# as_list = TRUE                  #          ef_brazil           |  Character   |          Default       |
# fcorr = 1                       #          ef_europe           |  User        |          Default       |
# slope = 0                       #          ef_europe           |  GPS         |          Default       |
# is_ef_euro_scaled               # condition to scale the EF    |  "Character" |          Default       |
# aggregate = FALSE               #         emissions            |  "Character" |          Default       |
# as_list = TRUE                  #         emissions            |  "Character" |          Default       |
# parallel = TRUE                 #       processing strategy    |  "Character" |          Default       |
# workers                         #       processing strategy    |  "Character" |          Default       |
# veh_type =                      #          ef_brazil           |  Fleet data  |             NA         |
# model_year =                    #          ef_brazil           |  Fleet data  |             NA         |
# speed =                         #          ef_europe           |  GPS         |             NA         |
# fuel = "D"                      #          ef_europe           |  Fleet data  |             NA         |
emission_model <- function(gps_line_path
                           ,ef_data_base
                           ,fleet_data
                           ,emis_path
                           ,pollutant
                           ,parallel
                           ,calendar_year
                           ,is_ef_scaled){
  
  
  fleet_data
  # Checking inputs -----
  
  message("Checking input data.")
  message("....")
  
  
  # Fleet 
  message("Fleet data...")
  if(ef_data_base == "cetesb_brazil"){
    if(is.null(fleet_data$veh_type) & is.null(fleet_data$model_year)){
      stop(sprintf("Arguments 'veh_type', 'model_year', and \n'calendar_year' input is required in the 'fleet_data' for %s database")
           ,ef_data_base)
    }
  }
  if(ef_data_base == "emfac_usa" | ef_data_base == "emfac_usa"){
    if(is.null(fleet_data$veh_type) & is.null(fleet_data$model_year)
       & is.null(fleet_data$calendar_year)){
      stop(sprintf("Arguments 'veh_type', and 'model_year'  input \nis required  in the 'fleet_data' for %s database")
           ,ef_data_base)
    }
  }
  if(ef_data_base == "ef_europe"){
    if(is.null(fleet_data$speed) & is.null(fleet_data$euro)
       & is.null(fleet_data$speed)){
      stop(sprintf("Arguments 'speed','euro', and 'speed' input \nis required  in the 'fleet_data' for %s database"
                   ,ef_data_base))
    }
  }
  
  # obs: this needs to read fleet first
  #
  # List GPS linestrings
  files_gps <- list.files(path = gps_line_path,full.names = TRUE)
  files_gps_names <- list.files(path = gps_line_path,full.names = FALSE)
  
  # EF ----------------
  if(ef_data_base == "cetesb_brazil"){
    temp_ef <- ef_brazil(pollutant = c("CO2","NOx","PM10","CH4"),
                         veh_type = fleet_spo$type_name_br,
                         model_year = as.numeric(fleet_spo$year),
                         as_list = TRUE)
  }
  # Emission loop function -----
  emission_estimate <- furrr::future_map(seq_along(files_gps),function(i){ # i = 1
    
    message(sprintf("Emissions estimates of file '%s'",files_gps_names[i]))
    
    # i) Read GPS linestrings ----
    tmp_gps <- readr::read_rds(files_gps[i])
    
    # ii) Get EF  ----
    if(ef_data_base == "emep_europe"){
      temp_ef <- ef_europe(pollutant = pollutant,
                           speed = tmp_gps$speed,
                           veh_type = fleet$veh_type,
                           tech = fleet$tech,
                           euro = fleet$euro,
                           fcorr = fcorr)
    }
    if(ef_data_base == "emfac_usa"){
      temp_ef <- ef_usa_emfac(pollutant = pollutant,
                              calendar_year = calendar_year,
                              model_year = total_fleet$year,
                              speed = tmp_gps$speed,
                              fuel = fuel)
    }
    if(ef_data_base == "moves_usa"){
      temp_ef <- ef_usa_moves(pollutant = pollutant,
                              calendar_year = calendar_year,
                              model_year = model_year,
                              speed = speed,
                              fuel = fuel)
    }
    if(ef_scaled){
      data(ef_europe_db)
      temp_ef_scaled <- base::suppressMessages(ef_euro_scaled(ef_local = units::set_units(temp_ef$EF,"g/km"),
                                                              speed = units::set_units(tmp_gps$speed,"km/h"),
                                                              veh_type = fleet$type_name_eu,
                                                              euro = fleet$euro,
                                                              fuel = fleet$fuel,
                                                              tech = fleet$Technology,
                                                              pollutant = pollutant) )
    }else{
      temp_ef_scaled <- temp_ef
    }
    
    # iii) Emissions -----
    temp_emis <- emis(fleet_composition = fleet$fleet_composition
                      , dist = units::set_units(tmp_gps$dist,"km")
                      , ef = temp_ef
                      , aggregate = FALSE
                      , as_list = TRUE)
    
    # iv) Add data -----
    # Add fleet info
    temp_emis$age = rep(fleet$year
                        ,data.table::uniqueN(temp_emis$pollutant))
    temp_emis$fleet_composition <- rep(fleet$fleet_composition
                                       ,data.table::uniqueN(temp_emis$pollutant))
    
    
    # Add EF to list
    temp_emis$EF = temp_ef$EF
    
    # Add gps data into emissions
    temp_emis$gps <- tmp_gps
    
    # v) Save emissions -----
    readr::write_rds(x = temp_emis
                     ,file = paste0(emis_path)
                     ,compress = "gz")
    
    return(NULL)
  })
  
  return(NULL)
}
# End---------