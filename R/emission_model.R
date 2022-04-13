#' @title Emissions model 
#' 
#' @description Estimate hot-exhaust emissions from the input transport model.
#' 
#' @param gps character or sf_linestring; Filepath with the GPS files in 
#' sf_linestring format or an sf_linestring data.
#' @param ef_data_base character; Emission factor database, such as "moves_usa",
#' "emfac_usa", "emep_europe","cetesb_brazil". 
#' @param fleet_data data.table. Database with fleet_data. The requires columns 
#' depends on the 'ef_data_base' source. See @examples for input.  
#' @param output_path character. Filepath to export emissions. The function only exports 
#' the emissions files when `gps` parameter is input as character. 
#' If `gps` argument is a sf_linestring, the function returns the data to User.
#' @param pollutant character. e.g. "CO","PM10","CO2","CH4","NOx" etc.
#'  See `ef_usa_moves`,`ef_usa_emfac`, `ef_brazil_cetesb` or `ef_emep_europe` 
#'  functions to check which pollutants are available. 
#' @param parallel logical; Decides whether the function should run in parallel. 
#' Defaults is FALSE.When TRUE, it will use all cores available minus one 
#' using future::plan() with strategy "multisession" internally. 
#' @param calendar_year numeric.Year in which the emissions inventory
#'  is estimated. Argument only required when `ef_usa_moves`,`ef_usa_emfac` are
#'  selected. 
#' @details In the fleet database..
#' - Column `calendar_year`: character; Base year of the @ef_data_base input. 
#' Required only when  `usa_moves` or `usa_emfac` are selected.
#' - Column `tech`: character; After treatment technology. This is required only 
#' when `emep_europe` is selected. Check `ef_emep_europe` for details.
#' - Column `euro`: character; Euro period of vehicle, classified in 
#' "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV". This is required only 
#' when `emep_europe` is selected. Check `ef_emep_europe` for details.
#' - Column `fuel`: character; Required when `ef_moves_usa`, `ef_emfac_usa` and
#'  `ef_emep_europe` are selected. 
#'  - Column `fleet_composition`: Numeric. Scaled composition of fleet. As several 
#'  cities does not specify which vehicle runs on each route, the composition is used
#'  to attribute a probability of a specific vehicle to circulate in the line. The 
#'  probability sums one. Required for all emission factors selection. 
#'  If the information of specific vehicle is known, user should develop the emission
#'  inventory according to the vignette <<detailed_vignette_link>>.
#'  
#' @return list of variables of emissions or NULL.
#' 
#' @export
#' @examples  
#' library(magrittr)
#' gtfs <- gtfs2gps::read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>% 
#' gtfs2gps::filter_by_shape_id(., "T2-1") %>%
#'   gtfs2gps::filter_single_trip()
#' 
#' sf_line <- transport_model(gtfs = gtfs,parallel = TRUE)
#' 
#' fleet_data_ef_cetesb <- data.table::data.table("veh_type" = "BUS_URBAN_D"
#'                                                ,"model_year" = 2010:2019
#'                                                ,"fuel" = "D"
#'                                                ,"fleet_composition" = rep(0.1,10))
#' fleet_data_ef_moves <- data.table::data.table("veh_type" = "BUS_URBAN_D"
#'                                               ,"model_year" = 2010:2019
#'                                               ,"fuel" = "D"
#'                                               ,"calendar_year" = 2019
#'                                               ,"fleet_composition" = rep(0.1,10))
#' fleet_data_ef_emfac <- data.table::data.table("veh_type" = "BUS_URBAN_D"
#'                                               ,"model_year" = 2010:2019
#'                                               ,"fuel" = "D"
#'                                               ,"calendar_year" = 2019
#'                                               ,"fleet_composition" = rep(0.1,10))
#' fleet_data_ef_europe <- data.table::data.table("veh_type" = c("Ubus Midi <=15 t"
#'                                                               ,"Ubus Std 15 - 18 t"
#'                                                               ,"Ubus Artic >18 t")
#'                                                ,"euro" = c("III","IV","V")
#'                                                ,"fuel" = rep("D",3)
#'                                                ,"tech" = c("-","SCR","SCR")
#'                                                ,"fleet_composition" = c(0.4,0.5,0.1))
#' 
#' sf_cetesb <- emission_model(gps = sf_line
#'                           ,ef_data_base = "cetesb_brazil"
#'                           ,fleet_data = fleet_data_ef_cetesb
#'                           ,pollutant = c("CO","PM10","CO2","CH4","NOx"))
#' sf_emfac <- emission_model(gps = sf_line
#'                           ,ef_data_base = "emfac_usa"
#'                           ,fleet_data = fleet_data_ef_emfac
#'                           ,pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                           ,calendar_year = 2019)
#' sf_moves <- emission_model(gps = sf_line
#'                           ,ef_data_base = "moves_usa"
#'                           ,fleet_data = fleet_data_ef_moves
#'                           ,pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                           ,calendar_year = 2019)
#' sf_emep <- emission_model(gps = sf_line
#'                           ,ef_data_base = "emep_europe"
#'                           ,fleet_data = fleet_data_ef_europe
#'                           ,pollutant = c("CO","PM10","CO2","CH4","NOx"))
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
  if(ef_data_base == "emep_europe"){
    if(is.null(fleet_data$euro)| is.null(fleet_data$fuel)|is.null(fleet_data$tech)| is.null(fleet_data$fleet_composition)){
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
      temp_ef <- ef_emep_europe(pollutant = pollutant,
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