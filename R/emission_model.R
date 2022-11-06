#' @title Emission model 
#' 
#' @description Estimate hot-exhaust emissions of public transport systems. This
#'              function must be used together with \code{\link{transport_model}}.
#' 
#' @param tp_model sf_linestring object or a character path the to sf_linestring objects.
#'        The `tp_model` is the output from \code{\link{transport_model}}, 
#'        or the path in which the output files from the \code{\link{transport_model}} are saved.
#' @param ef_model character. A string indicating the emission factor model 
#'        to be used. Options include `ef_usa_moves`, `ef_usa_emfac`,`ef_europe_emep`, 
#'        ,`ef_brazil_cetesb`, and `ef_brazil_scaled_euro` (scale `ef_brazil_cetesb()` based 
#'        on `ef_scaled_euro()`).
#' @param fleet_data data.frame. A `data.frame` with information the fleet 
#'        characteristics. The required columns depend on the 
#'        `ef_model` selection. See @examples for input.
#' @param pollutant character. Vector with one or more pollutants to be estimated.
#'        Example: `c("CO", "CO2", "PM10", "NOx")`. See the documentation to check which 
#'        pollutants are available for each emission factor model (`ef_usa_moves`, `ef_usa_emfac`,
#'        `ef_europe_emep`, or `ef_brazil_cetesb`).
#' @param reference_year numeric. Year of reference considered to calculate the
#'        emissions inventory. Defaults to `2020`. This 
#'        argument is only required when the `ef_model` 
#'        argument is `ef_usa_moves` or `ef_usa_emfac`.
#' @param heightfile character or raster data. The raster file with height data,
#'        or its filepath, used to estimate emissions considering the effect of 
#'        street slope. This argument is used only when `ef_brazil_scaled_euro` or 
#'        `ef_europe_emep` are selected. Default is `NULL`. Details are provided in
#'        \code{\link{slope_class_europe_emep}}.
#' @param parallel logical. Decides whether the function should run in parallel. 
#'        Defaults is `TRUE`. 
#' @param ncores integer. Number of cores to be used in parallel execution. This 
#'        argument is ignored if parallel is `FALSE`. Default (`NULL`) selects 
#'        the total number of available cores minus one.                
#' @param output_path character. File path where the function output is exported.
#'        If `NULL` (Default), the function returns the output to user.
#' @param continue logical. Argument that can be used only with output_path When TRUE,
#'        it skips processing the shape identifiers that were already saved into 
#'        files. It is useful to continue processing a GTFS file that was stopped
#'        for some reason. Default value is FALSE.    
#' @param quiet Logical; Display messages from the emissions or emission factor functions. 
#'        Default is 'TRUE'.
#' @details The `fleet_data` must be a `data.frame` organized according to the desired
#' `ef_model`. The required columns is organized as follows (see @examples for real 
#' data usage). 
#' - `reference_year`: character; Base year of the emission factor model input. 
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
#'  factors selection. 
#'  Users can check the 
#'  [gtfs2emis fleet data vignette](https://ipeagit.github.io/gtfs2emis/articles/gtfs2emis_fleet_data.html),
#'   for more examples. 
#'  
#'  Based on the input height data, the function returns the slope class between two consecutive 
#' bus stop positions of a LineString Simple Feature (transport model object).
#'  The slope is given by the ratio between the height difference and 
#'  network distance from two consecutive public transport stops.
#'  The function classifies the slope into one of the seven categories
#' available on the European Environmental Agency (EEA) database, which is -0.06,
#'  -0.04,-0.02, 0.00, 0.02, 0.04, and 0.06. The classifications is described in
#' @return A `list` with emissions estimates or `NULL` with output files saved 
#'         locally at `output_path`.
#' @family Core function
#' @examples
#' \dontrun{
#' library(gtfstools)
#' 
#' # read GTFS
#' gtfs_file <- system.file("extdata/bra_cur_gtfs.zip", package = "gtfs2emis")
#' gtfs <- gtfstools::read_gtfs(gtfs_file) 
#' 
#' # keep a single trip_id to speed up this example
#' gtfs_small <- gtfstools::filter_by_trip_id(gtfs, trip_id ="4451136")
#'   
#' # run transport model
#' tp_model <- transport_model(gtfs_data = gtfs_small,
#'                             min_speed = 2,
#'                             max_speed = 80,
#'                             new_speed = 20,
#'                             spatial_resolution = 100,
#'                             parallel = FALSE)
#' 
#' # Example using Brazilian emission model and fleet
#' fleet_data_ef_cetesb <- data.frame(veh_type = "BUS_URBAN_D",
#'                                    model_year = 2010:2019,
#'                                    fuel = "D",
#'                                    fleet_composition = rep(0.1,10)
#'                                    )
#'                                    
#' emi_cetesb <- progressr::with_progress(emission_model(
#'                 tp_model = tp_model,
#'                 ef_model = "ef_brazil_cetesb",
#'                 fleet_data = fleet_data_ef_cetesb,
#'                 pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                 ))
#'                             
#' # Example using European emission model and fleet
#' fleet_data_ef_europe <- data.frame(  veh_type = c("Ubus Midi <=15 t",
#'                                                   "Ubus Std 15 - 18 t",
#'                                                   "Ubus Artic >18 t")
#'                                    , euro = c("III","IV","V")
#'                                    , fuel = rep("D",3)
#'                                    , tech = c("-","SCR","SCR")
#'                                    , fleet_composition = c(0.4,0.5,0.1))
#'                                    
#' emi_emep <- progressr::with_progress(emission_model(tp_model = tp_model
#'                           , ef_model = "ef_europe_emep"
#'                           , fleet_data = fleet_data_ef_europe
#'                           , pollutant = c("PM10","NOx")))
#' 
#' raster_cur <- system.file("extdata/bra_cur-srtm.tif", package = "gtfs2emis")                           
#' emi_emep_slope <- progressr::with_progress(emission_model(tp_model = tp_model
#'                           , ef_model = "ef_europe_emep"
#'                           , fleet_data = fleet_data_ef_europe
#'                           , heightfile = raster_cur
#'                           , pollutant = c("PM10","NOx")))  
#'                                                   
#' # Example using US EMFAC emission model and fleet
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
#' # Example using US MOVES emission model and fleet
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
#' }
#' @export
emission_model <- function(  tp_model
                             , ef_model
                             , fleet_data
                             , pollutant
                             , reference_year = 2020
                             , heightfile = NULL
                             , parallel = TRUE
                             , ncores = NULL
                             , output_path = NULL
                             , continue = FALSE
                             , quiet = TRUE){
  # A) Checking inputs -----
  
  checkmate::assert_data_frame(fleet_data, null.ok = FALSE)
  checkmate::assert_vector(pollutant, null.ok = FALSE)
  checkmate::assert_logical(parallel, null.ok = FALSE)
  
  if(parallel)  checkmate::assert_integerish(ncores,lower = 1,upper = future::availableCores(),null.ok = TRUE)
  
  checkmate::assert_logical(continue, null.ok = FALSE)
  checkmate::assert_logical(quiet, null.ok = FALSE)
  checkmate::assert_numeric(reference_year, lower = 2000, finite = TRUE, any.missing = TRUE)
  checkmate::assert(
    checkmate::check_class(tp_model, classes = c("sf", "data.frame"))
    , checkmate::check_class(tp_model, classes = c("character"))
    , combine = "or"
  )
  if(is.character(tp_model)) checkmate::assert_directory_exists(tp_model)
  checkmate::assert_string(output_path, null.ok = TRUE)
  if(!is.null(output_path)) checkmate::assert_directory_exists(output_path)

  ## height file ----
  checkmate::assert(
    checkmate::check_class(heightfile, classes = c("RasterLayer","Raster"),null.ok = TRUE)
    , checkmate::check_class(heightfile, classes = c("character"),null.ok = TRUE)
    , combine = "or"
  )
  if(is.character(heightfile)) checkmate::assert_file_exists(heightfile)
  
  ## i) EF model ----
  checkmate::assert_choice(ef_model
                           ,c("ef_brazil_cetesb","ef_usa_emfac"
                              ,"ef_usa_moves","ef_europe_emep"
                              ,"ef_brazil_scaled_euro")
                           ,null.ok = FALSE)
  ## ii) Fleet data  | EF model ----
  if (ef_model != "ef_europe_emep"){
    checkmate::assert(
      checkmate::check_choice('veh_type', names(fleet_data))
      , checkmate::check_choice('model_year', names(fleet_data))
      , checkmate::check_choice('fleet_composition', names(fleet_data))
      , combine = "and"
    )
  }
  if (ef_model == "ef_europe_emep" | ef_model == "ef_brazil_scaled_euro"){
    checkmate::assert(
      checkmate::check_choice('euro', names(fleet_data))
      , checkmate::check_choice('fuel', names(fleet_data))
      , checkmate::check_choice('tech', names(fleet_data))
      , checkmate::check_choice('fleet_composition', names(fleet_data))
      , combine = "and"
    )
  }
  
  ## iii) Transport model ----
  
  ### Character input path----
  if(is.character(tp_model)){ 
    
    tp_fname_files <- list.files(path = tp_model,pattern = ".rds"
                                 ,all.files = TRUE,full.names = TRUE)
    tp_name_files <- list.files(path = tp_model,pattern = ".rds"
                                ,all.files = TRUE,full.names = FALSE)
    
    # check if there is files with .txt|.rds patterns
    if(length(tp_fname_files) == 0){ 
      stop(sprintf("No files with patterns '.rds' were found in %s path "
                   ,tp_model,".\n Please provide a valid input path."))
    }
    
  }
  
  if(parallel){
    # number of cores
    if(is.null(ncores)){
      ncores <- max(1, future::availableCores() - 1)
      
      if(!quiet) message(paste('Using', ncores, 'CPU cores'))
    }
    
    oplan <- future::plan("multisession", workers = ncores)
    on.exit(future::plan(oplan), add = TRUE)
   
    
  }
  # B) EF function ---------------- 
  
  # Generate EF cetesb_brazil before the loop
  # to avoid multiple runs in the loop
  if(ef_model == "ef_brazil_scaled_euro" | ef_model == "ef_brazil_cetesb"){
    
    if(quiet){
      
      temp_ef <- suppressMessages( ef_brazil_cetesb(pollutant = pollutant,
                                                    veh_type = fleet_data$veh_type,
                                                    model_year = as.numeric(fleet_data$model_year),
                                                    as_list = TRUE) )
    }else{
      
      temp_ef <- ef_brazil_cetesb(pollutant = pollutant,
                                  veh_type = fleet_data$veh_type,
                                  model_year = as.numeric(fleet_data$model_year),
                                  as_list = TRUE) 
    }
    
    if(ef_model == "ef_brazil_scaled_euro"){
      
      temp_local_ef <- temp_ef
      
    }
  }
  
  # C) Emission function -----
  core_fun_emis <- function(temp_shape){ # temp_shape = tp_fname_files[1]
    
    # i) Read GPS linestrings or DT ----
    if (is.character(temp_shape))  temp_shape <- readRDS(temp_shape)
    
    # ii) Get EF  ----
    if (ef_model == "ef_europe_emep") {
      
      if(!is.null(heightfile)){
        temp_shape <- slope_class_europe_emep(tp_model = temp_shape
                                              ,heightfile = heightfile)
        slope_input <- temp_shape$slope_class
      }else{
        slope_input <- 0
      }
      
      temp_ef <- ef_europe_emep(pollutant = pollutant
                                , speed = temp_shape$speed
                                , veh_type = fleet_data$veh_type
                                , tech = fleet_data$tech
                                , euro = fleet_data$euro
                                , slope = slope_input
                                )
      
    }
    if(ef_model == "ef_usa_emfac"){
      
      temp_ef <- ef_usa_emfac(pollutant = pollutant,
                              , reference_year = reference_year
                              , model_year = fleet_data$model_year
                              , speed = temp_shape$speed
                              , fuel = fleet_data$fuel)
    }
    if(ef_model == "ef_usa_moves"){
      
      temp_ef <- ef_usa_moves(pollutant = pollutant
                              , reference_year = reference_year
                              , model_year = fleet_data$model_year
                              , speed = temp_shape$speed
                              , fuel = fleet_data$fuel)
    }
    if(ef_model == "ef_brazil_scaled_euro"){
      
      if(!is.null(heightfile)){
        temp_shape <- slope_class_europe_emep(tp_model = temp_shape
                                              ,heightfile = heightfile)
        slope_input <- temp_shape$slope_class
      }else{
        slope_input <- 0
      }
      
      temp_ef <- ef_scaled_euro(ef_local = temp_local_ef$EF
                                , speed = temp_shape$speed
                                , veh_type = fleet_data$type_name_eu
                                , euro = fleet_data$euro
                                , pollutant = pollutant
                                , fuel = fleet_data$fuel
                                , SDC = 19
                                , slope = slope_input
                                , load = 0.5
                                , fcorr = 1
      )
      
    }
    
    
    # iii) Emissions -----
    temp_emis <- multiply_ef(fleet_composition = fleet_data$fleet_composition
                             , dist = units::set_units(temp_shape$dist, "km") 
                             , ef = temp_ef
                             , aggregate = FALSE
                             , as_list = TRUE)
    
    # iv) Add data -----
    # Add fleet info
    if(ef_model == "ef_brazil_scaled_euro"){
      
      temp_emis$model_year = rep(  fleet_data$model_year
                                   , data.table::uniqueN(temp_emis$pollutant))
      temp_emis$euro = rep(  fleet_data$euro
                             , data.table::uniqueN(temp_emis$pollutant))
      
    }
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
    temp_emis$tp_model <- temp_shape
    
    # v) Save emissions -----
    
    return(temp_emis)
  }
  
  # D) function to write individual files -----
  #if(is.character(tp_model)){ 
  #  p <- progressr::progressor(steps = length(tp_fname_files))
  #}
  
  prepare_emis_output <- function(i){ # i = 1
    #p()
    ###  i) Output Valid ----
    output_name <- paste0(output_path,"/",tp_name_files[i])
    if(continue){ if(file.exists(output_name)) return(NULL)  }
    
    # Core fun
    if(quiet){    
      
      tmp_emis <- suppressMessages(
        core_fun_emis(temp_shape = tp_fname_files[i])
      )
      
    }else{
      
      tmp_emis <- core_fun_emis(temp_shape = tp_fname_files[i])
      
    }
    ###  i) Output NULL ----
    if (is.null(output_path)) {
      return(tmp_emis)
    }
    
    saveRDS(object = tmp_emis,file = output_name)
    return(NULL)
  }
  
  ## E) IF 'tp_model' is character -----
  if(is.character(tp_model)){ 
    
    # Check parallel condition
    if(parallel){
      
      requiredPackages = c('data.table', 'sf', 'units')
      emisLine <- furrr::future_map(.x = 1:length(tp_fname_files)
                                    ,.f = prepare_emis_output
                                    ,.options = furrr::furrr_options(
                                      packages = requiredPackages))
    }else{
      emisLine <- lapply(X = 1:length(tp_fname_files)
                         ,FUN = prepare_emis_output)
    }
    
    # if an output_path is provided, function do not return data
    if (!is.null(output_path)) return(NULL)
    
    return(emisLine)
  }else{ 
    
    ## F) If 'tp_model' is transport model -----
    
    tmp_emis <- core_fun_emis(tp_model)
    
    #  Output NULL 
    if (is.null(output_path)) {
      return(tmp_emis)
    }
    # Output valid
    export_path <- paste0(output_path,"/"
                          ,unique(tp_model$shape_id)[1]
                          ,".rds")
    
    if( data.table::uniqueN(tp_model$shape_id) > 1 & quiet == "FALSE"){
      message(sprintf("Writing output as '%s' file."
                      ,export_path))
    }
    
    # continue condition
    if(continue){ if(file.exists(export_path)) return(NULL)  }
    
    saveRDS(object = tmp_emis
            ,file = export_path)
    return(NULL)
  }
}