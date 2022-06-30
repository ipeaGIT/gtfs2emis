#' @title Emission model 
#' 
#' @description Estimate hot-exhaust emissions of public transport systems. This
#'              function must be used together with \code{\link{transport_model}}.
#' 
#' @param tp_model sf_linestring object or a character path the to sf_linestring objects.
#' The `tp_model` is the output from \code{\link{transport_model}}, 
#' or the path in which the output files from the \code{\link{transport_model}} are saved.
#' @param ef_model character. A string indicating the emission factor model 
#' to be used. Options include `ef_usa_moves`, `ef_usa_emfac`,`ef_europe_emep`, 
#' or `ef_brazil_cetesb`.
#' @param fleet_data data.frame. A`data.frame` with information the fleet 
#' characteristics. The required columns depend on the 
#' `ef_model` parameted selected. See @examples for input.
#' @param pollutant character. Vector with one or more pollutants to be estimated.
#' Example: `c("CO", "CO2", "PM10", "NOx")`. See the documentation to check which 
#' pollutants are available for each emission factor model (`ef_usa_moves`, `ef_usa_emfac`,
#' `ef_europe_emep`, or `ef_brazil_cetesb`).
#' @param reference_year numeric. Year of reference considered to calculate the
#'                      emissions inventory. Defaults to `2020`. This 
#'                      argument is only required when the `ef_model` 
#'                      argument is `ef_usa_moves` or `ef_usa_emfac`.
#' @param parallel logical. Decides whether the function should run in parallel. 
#'                 When TRUE, it will use all cores available
#'                 minus one using future::plan() with strategy "multisession" 
#'                 internally. Defaults is TRUE. Note that it is possible to create 
#'                 your own plan before calling this function. In this case, do not
#'                 use this argument.                
#' @param output_path character. Filepath where the function output is saved.
#'                    666  The function only exports the emissions files when `gps` 
#'                    parameter is input as character. If `gps` argument is a 
#'                    sf_linestring, the function returns the data to user. 
#'                    666 Esse comportamento nao faz sentido neh. O funcionamento
#'                    do output_path nao deveria depender do input de gps. Isso deveria ser:
#'                    666 If `NULL` (Default), the function returns the output
#'                    to user.
#' @details The `fleet_data` must be a `data.frame` organized according to the desired
#' `ef_model`. The required columns is organized as follows (see @examples for real 
#' data usage). 
#' - `reference_year`: character; Base year of the @ef_model input. 
#' Required only when  `ef_usa_moves` or `_efusa_emfac` are selected.
#' - `tech`: character; After treatment technology. This is required only 
#' when `emep_europe` is selected. Check `?ef_emep_europe` for details.
#' - `euro`: character; Euro period of vehicle, classified in 
#' "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV". This is required only 
#' when `ef_emep_europe` is selected. Check `ef_europe_emep` for details.
#' - `fuel`: character; Required when `ef_usa_moves`, `ef_usa_emfac` and
#'  `ef_europe_emep` are selected. 
#'  - `fleet_composition`: Numeric. Scaled composition of fleet. In most 
#'  cases, the user might not know which vehicles run on each specific routes.
#'  The composition is used to attribute a probability of a specific vehicle to 
#'  circulate in the line. The probability sums one. Required for all emission 
#'  factors selection. If the information of specific vehicle is known, user 
#'  should develop the emission inventory according to the vignette 
#'  <<http://www.github.com/ipeaGIT/gtfs2emis/>>.
#'  
#' @return A `list` with emissions estimates or `NULL` with output files saved 
#'         locally at `output_path`.
#' @family Core function
#' @export
#' @examples if (interactive()) {
#' library(gtfs2emis)
#' library(gtfs2gps)
#' library(magrittr)
#' 
#' gtfs <- gtfstools::read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>% 
#' gtfstools::filter_by_shape_id(., "T2-1") %>%
#'   gtfs2gps::filter_single_trip()
#' 
#' tp_model <- transport_model(gtfs = gtfs, parallel = TRUE)
#' 
#' 
#' # Example using Brazilian emission model and fleet
#' fleet_data_ef_cetesb <- data.frame(  veh_type = "BUS_URBAN_D"
#'                                    , model_year = 2010:2019
#'                                    , fuel = "D"
#'                                    , fleet_composition = rep(0.1,10))
#'                                    
#' emi_cetesb <- emission_model(tp_model = tp_model
#'                             , ef_model = "ef_brazil_cetesb"
#'                             , fleet_data = fleet_data_ef_cetesb
#'                             , pollutant = c("CO","PM10","CO2","CH4","NOx"))
#'                             
#' # Example using European emission model and fleet
#' 
#' fleet_data_ef_europe <- data.frame(  veh_type = c("Ubus Midi <=15 t" 
#' ,"Ubus Std 15 - 18 t" ,"Ubus Artic >18 t")
#'                                    , euro = c("III","IV","V")
#'                                    , fuel = rep("D",3)
#'                                    , tech = c("-","SCR","SCR")
#'                                    , fleet_composition = c(0.4,0.5,0.1))
#'                                    
#' emi_emep <- emission_model(tp_model = tp_model
#'                           , ef_model = "ef_europe_emep"
#'                           , fleet_data = fleet_data_ef_europe
#'                           , pollutant = c("CO","PM10","CO2","CH4","NOx"))
#'                           
#'                           
#' # Example using US emission model and fleet
#' fleet_data_ef_moves <- data.frame(  veh_type = "BUS_URBAN_D"
#'                                   , model_year = 2010:2019
#'                                   , fuel = "D"
#'                                   , reference_year = 2020
#'                                   , fleet_composition = rep(0.1,10))
#'                                   
#' fleet_data_ef_emfac <- data.frame(  veh_type =  "BUS_URBAN_D"
#'                                   , model_year = 2010:2019
#'                                   , fuel = "D"
#'                                   , reference_year = 2020
#'                                   , fleet_composition = rep(0.1,10))
#'                                   
#' emi_moves <- emission_model(tp_model = tp_model
#'                           , ef_model = "ef_usa_moves"
#'                           , fleet_data = fleet_data_ef_moves
#'                           , pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                           , reference_year = 2020)
#'                           
#' emi_emfac <- emission_model(tp_model = tp_model
#'                           , ef_model = "ef_usa_emfac"
#'                           , fleet_data = fleet_data_ef_emfac
#'                           , pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                           , reference_year = 2020)
#'}
emission_model <- function(  tp_model
                             , ef_model
                             , fleet_data
                             , pollutant
                             , reference_year = 2020
                             , parallel = TRUE
                             , output_path = NULL){
  # A) Checking inputs -----
  
  checkmate::assert_data_frame(fleet_data, null.ok = FALSE)
  checkmate::assert_vector(pollutant, null.ok = FALSE)
  checkmate::assert_logical(parallel, null.ok = FALSE)
  checkmate::assert_string(output_path, null.ok = TRUE)
  checkmate::assert_numeric(reference_year, lower = 2000, finite = TRUE, any.missing = TRUE)
  checkmate::assert_class(tp_model, classes = c("sf", "data.frame"))
    
  ## i) EF model ----
  if (!(ef_model %in% c("ef_brazil_cetesb","ef_usa_emfac","ef_usa_moves","ef_europe_emep"))) {
    stop("'ef_model' has to me one of the following 'ef_brazil_cetesb',\n'ef_usa_emfac'
         ,'ef_usa_moves', or 'ef_europe_emep'")
  }
  
  ## ii) Fleet data  | EF model ----
  if (ef_model == "ef_cetesb_brazil") {
    if(is.null(fleet_data$veh_type) | is.null(fleet_data$model_year)
       | is.null(fleet_data$fleet_composition)){
      stop("fleet_data input must have the columns c('veh_type',
           'fleet_composition', 'model_year') when ef_model == 'ef_cetesb_brazil'")
    }
  }
  if (ef_model == "ef_usa_emfac" | ef_model == "ef_usa_moves") {

    if(is.null(fleet_data$veh_type) | is.null(fleet_data$model_year)
       | is.null(fleet_data$fleet_composition)){
      stop(sprintf("fleet_data input must have the columns c('veh_type', 
                   'fleet_composition', 'model_year') when ef_model == %s")
           ,ef_model)
    }
  }
  if (ef_model == "ef_europe_emep") {
    if(is.null(fleet_data$euro) | is.null(fleet_data$fuel)|
       is.null(fleet_data$tech) | is.null(fleet_data$fleet_composition)){
      stop("fleet_data input must have the columns c('speed','euro', 
           'fleet_composition', 'speed') when  input ef_model == ef_europe_emep")
    }
  }
  
  ## iii) Transport model ----
  
  ### Character input path----
  if(is.character(tp_model)){ 
    
    message(sprintf("Checking files in %s path", tp_model))
    #tp_model <- "article/data/gps_spo/"
    tp_model_files <- list.files(path = tp_model,pattern = ".txt|.rds"
                                 ,all.files = TRUE,full.names = TRUE)
    tp_name_files <- list.files(path = tp_model,pattern = ".txt|.rds"
                                ,all.files = TRUE,full.names = FALSE)
    
    # check if there is files with .txt|.rds patterns
    if(length(tp_model_files) == 0){ 
      stop(sprintf("No files with patterns '.txt' or '.rds' were found %s path "
                   ,tp_model,".\n Please provide a valid input path."))
    }
    
  }
  ### iv) Sflinestring input file----
  if (!is.character(tp_model)) {
    
    # Columns that should exist 
    getCols <- colnames(tp_model)
    if(sum(getCols %in% c("speed","dist","cumdist","cumtime")) != 4){
      stop("Columns 'speed', 'dist', 'cumdist' and 'cumtime' should exist in the tp_model file.")
    }
    # checa output (tem que ser rds)
    if (!is.null(output_path)) {
      if (!(output_path %like% ".rds")) {
        stop("User should provide a valid output_filepath in '.rds' format.")
      }
    }
    
  } 
  
  
  # B) EF function ---------------- 

  # Generate EF cetesb_brazil before the loop
  # to avoid multiple runs in the loop
  if(ef_model == "ef_brazil_cetesb"){
    temp_ef <- ef_brazil_cetesb(pollutant = pollutant,
                                veh_type = fleet_data$veh_type,
                                model_year = as.numeric(fleet_data$model_year),
                                as_list = TRUE)
  }
  # C) Emission function -----
  emission_estimate <- function(tp_model){ # i = 1  
    
    # 666 essa funcao aqui nao tinha q ter os parametros de frota e ef q sao passados adiante?
    
    # i) Read GPS linestrings or DT ----
    if (is.character(tp_model)) {
      
      # type txt
      if(tp_model %like% ".txt"){
        tp_model <- data.table::fread(tp_model)
      }
      # type rds
      if(tp_model %like% ".rds"){
        tp_model <- readRDS(tp_model)
      }
      
      # fix units
      data.table::setDT(tp_model)
      tp_model[,speed := units::set_units(speed,"km/h")]
      tp_model[, dist := units::set_units(dist,"m")]
      tp_model[, cumdist := units::set_units(cumdist,"m")]
      tp_model[, speed := units::set_units(speed,"km/h")]
      tp_model[, cumtime := units::set_units(cumtime,"s")]
    }
    # ii) Get EF  ----
    if (ef_model == "ef_europe_emep") {
      
      temp_ef <- ef_europe_emep(pollutant = pollutant,
                                speed = tp_model$speed,
                                veh_type = fleet_data$veh_type,
                                tech = fleet_data$tech,
                                euro = fleet_data$euro,
                                fcorr = 1)
    }
    if(ef_model == "ef_usa_emfac"){
      
      temp_ef <- ef_usa_emfac(pollutant = pollutant,
                              reference_year = reference_year,
                              model_year = fleet_data$model_year,
                              speed = tp_model$speed,
                              fuel = fleet_data$fuel)
    }
    if(ef_model == "ef_usa_moves"){
      
      temp_ef <- ef_usa_moves(pollutant = pollutant,
                              reference_year = reference_year,
                              model_year = fleet_data$model_year,
                              speed = tp_model$speed,
                              fuel = fleet_data$fuel)
    }
    
    
    
    # iii) Emissions -----
    temp_emis <- emis(fleet_composition = fleet_data$fleet_composition
                      , dist = units::set_units(tp_model$dist, "km") 
                      , ef = temp_ef
                      , aggregate = FALSE
                      , as_list = TRUE)
    
    # iv) Add data -----
    # Add fleet info
    if (ef_model != "ef_europe_emep") {
      temp_emis$model_year = rep(  fleet_data$model_year
                                   , data.table::uniqueN(temp_emis$pollutant))
    } else {
      temp_emis$euro = rep(  fleet_data$euro
                             , data.table::uniqueN(temp_emis$pollutant))
    }
    
    temp_emis$fleet_composition <- rep(fleet_data$fleet_composition
                                       ,data.table::uniqueN(temp_emis$pollutant))
    
    
    # Add EF to list
    temp_emis$EF = temp_ef$EF
    
    # Add gps data into emissions
    temp_emis$tp_model <- tp_model
    
    # v) Save emissions -----
    
    return(temp_emis)
  }
  
  # D) function to write individual files -----
  
  f_input_character <- function(i){
    
    tmp_emis <- emission_estimate(tp_model_files[i])
    ###  i) Output NULL ----
    if (is.null(output_path)) {
      return(emisLine)
    }
    ###  ii) Output Valid ----
    output_name <- paste0(output_path,"/",tp_name_files[i])
    saveRDS(object = tmp_emis,file = output_name)
    return(NULL)
  }
  
  ## E) Input is a character -----
  if(is.character(tp_model)){ 
    
    # Check parallel condition
    if(parallel){
      emisLine <- furrr::future_map(.x = 1:length(tp_model_files)
                                    ,.f = f_input_character
                                    ,.options = furrr::furrr_options(seed = 123))
    }else{
      emisLine <- lapply(.x = 1:length(tp_model_files)
                         ,.f = f_input_character
                         ,.options = furrr::furrr_options(seed = 123))
    }
    emisLine <- data.table::rbindlist(emisLine)
    
    # if an output_path is provided, function do not return data
    if (!is.null(output_path)) return(NULL)
    
    return(emisLine)
  }else{ 
    
    ## F) Input is a file  -----
    
    tmp_emis <- emission_estimate(tp_model)
    
    #  Output NULL 
    if (is.null(output_path)) {
      return(tmp_emis)
    }
    # Output valid
    saveRDS(object = tmp_emis,file = output_path)
    return(NULL)
  }
}