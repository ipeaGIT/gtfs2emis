#' @title Emission model 
#' 
#' @description Estimate hot-exhaust emissions of public transport systems. This
#'              function must be used together with \code{\link{transport_model}}.
#' 
#' @param gps A `sf_linestring` object or a character string. The `sf_linestring` 
#'            output from \code{\link{transport_model}, or the filepath with 
#'            output files from \code{\link{transport_model} are saved.
#'            [666] talvez esse parametro devesse se chamar `tp_model`? pra deixar
#'            clara a integracao com a funcao de transport model?
#'            
#' @param ef_data_base character. A  string indicating the emission factor model
#'                     to be used. Options include `c("ef_usa_moves", "ef_usa_emfac", "ef_emep_europe","ef_brazil_cetesb")`.
#'                     [666] nome do parametro: ef_model ? E daí as opcoes seriam os strings de tipo "ef_brazil_cetesb"
#'                     
#' @param fleet_data data.frame. A  `data.frame` with information the fleet
#'                   characteristics. The required columns depend on the 
#'                   `ef_data_base` parameted selected. See @examples for input.
#'                   
#' @param pollutant character vector with one or more pollutants to be estimated.
#'                  Example: `c("CO", "CO2", "PM10", "NOx")`. See the 
#'                  documentation to check which pollutants are available for 
#'                  each emission factor model.
#'                  
#' @param calendar_year numeric. Year of reference considered to calculate the
#'                      emissions inventory. Defaults to current year. This 
#'                      argument is only required when the `ef_data_base` 
#'                      parameter is one of the following: `c("ef_usa_moves", "ef_usa_emfac")`.
#'                      [66666] Defaults to current year
#'                      [66666] mudar nome do parametro pra `reference_year` ?
#'                      
#' @param parallel logical. Decides whether the function should run in parallel. 
#'                 Defaults is FALSE. When TRUE, it will use all cores available
#'                 minus one using future::plan() with strategy "multisession" 
#'                 internally. [Note that it is possible to create your own plan 
#'                 before calling gtfs2gps(). In this case, do not use this 
#'                 argument.
#'                 [66666] acho q o default deveria ser `TRUE` aqui e no gtfs2gps tmb
#'                 
#' @param output_path character. Filepath where the function output is saved.
#'                    [666]  The function only exports the emissions files when `gps` 
#'                    parameter is input as character. If `gps` argument is a 
#'                    sf_linestring, the function returns the data to user. 
#'                    [666] Esse comportamento nao faz sentido neh. O funcionamento
#'                    do output_path nao deveria depender do input de gps. Isso deveria ser:
#'                    [666] If `NULL` (Default), the function returns the output
#'                    to user.
#'          
#' @details The `fleet_data` must be a `data.frame` organized as follows: 
#'          [6666] acho q vale a apena organizar esses details melhor
#' - Column `calendar_year`: character; Base year of the @ef_data_base input. 
#' Required only when  `usa_moves` or `usa_emfac` are selected.
#' - Column `tech`: character; After treatment technology. This is required only 
#' when `emep_europe` is selected. Check `?ef_emep_europe` for details.
#' - Column `euro`: character; Euro period of vehicle, classified in 
#' "Conventional", "I", "II", "III", "IV", "V", "VI", and "EEV". This is required only 
#' when `emep_europe` is selected. Check `ef_emep_europe` for details.
#' - Column `fuel`: character; Required when `ef_moves_usa`, `ef_emfac_usa` and
#'  `ef_emep_europe` are selected. 
#'  - Column `fleet_composition`: Numeric. Scaled composition of fleet. In most 
#'  cases, the user might not know which vehicles run on each specific routes.
#'  ... the composition is used
#'  to attribute a probability of a specific vehicle to circulate in the line. The 
#'  probability sums one. Required for all emission factors selection. 
#'  If the information of specific vehicle is known, user should develop the emission
#'  inventory according to the vignette <<detailed_vignette_link>>.
#'  
#' @return A `list` with emissions estimates or `NULL` with output files saved 
#'         locally at `output_path`.
#' 
#' @export
#' @examples  
#' library(gtfs2emis)
#' library(gtfs2gps)
#' library(magrittr)
#' library(data.table)
#' 
#' gtfs <- gtfs2gps::read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>% 
#' gtfs2gps::filter_by_shape_id(., "T2-1") %>%
#'   gtfs2gps::filter_single_trip()
#' 
#' tp_model <- transport_model(gtfs = gtfs, parallel = TRUE)
#' 
#' fleet_data_ef_cetesb <- data.frame(  veh_type = "BUS_URBAN_D"
#'                                    , model_year = 2010:2019
#'                                    , fuel = "D"
#'                                    , fleet_composition = rep(0.1,10))
#'                                                
#' fleet_data_ef_moves <- data.frame(  veh_type = "BUS_URBAN_D"
#'                                   , model_year = 2010:2019
#'                                   , fuel = "D"
#'                                   , calendar_year = 2019
#'                                   , fleet_composition = rep(0.1,10))
#'                                               
#' fleet_data_ef_emfac <- data.frame(  veh_type =  "BUS_URBAN_D"
#'                                   , model_year = 2010:2019
#'                                   , fuel = "D"
#'                                   , calendar_year = 2019
#'                                   , fleet_composition = rep(0.1,10))
#'                                   
#' fleet_data_ef_europe <- data.frame(  veh_type = c("Ubus Midi <=15 t" ,"Ubus Std 15 - 18 t" ,"Ubus Artic >18 t")
#'                                    , euro = c("III","IV","V")
#'                                    , fuel = rep("D",3)
#'                                    , tech = c("-","SCR","SCR")
#'                                    , fleet_composition = c(0.4,0.5,0.1))
#' 
#' emi_cetesb <- emission_model(gps = tp_model
#'                             , ef_data_base = "cetesb_brazil"
#'                             , fleet_data = fleet_data_ef_cetesb
#'                             , pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                             )
#'                             
#' emi_emfac <- emission_model(gps = tp_model
#'                           ,ef_data_base = "emfac_usa"
#'                           ,fleet_data = fleet_data_ef_emfac
#'                           ,pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                           ,calendar_year = 2019)
#' emi_moves <- emission_model(gps = tp_model
#'                           ,ef_data_base = "moves_usa"
#'                           ,fleet_data = fleet_data_ef_moves
#'                           ,pollutant = c("CO","PM10","CO2","CH4","NOx")
#'                           ,calendar_year = 2019)
#' emi_emep <- emission_model(gps = tp_model
#'                           ,ef_data_base = "emep_europe"
#'                           ,fleet_data = fleet_data_ef_europe
#'                           ,pollutant = c("CO","PM10","CO2","CH4","NOx"))
emission_model <- function(  gps
                           , ef_data_base
                           , fleet_data
                           , pollutant
                           , calendar_year = NULL
                           , parallel = TRUE
                           , output_path = NULL){
  # Checking inputs -----
  
  # 666 Check inputs 
  # 666 Check inputs 
  # 666 Check inputs 
  
  if (is.null(calendar_year)) { calendar_year <- format(Sys.Date(), "%Y") }
  
  # Check emission factor model input 
  if (!(ef_data_base %in% c("cetesb_brazil","emfac_usa","moves_usa","emep_europe"))) {
    stop("'ef_data_base' has to me one of the following 'cetesb_brazil',\n'emfac_usa','moves_usa', or 'emep_europe'")
  }
  
  # Check fleet data input 
  if (ef_data_base == "cetesb_brazil") {
    if(is.null(fleet_data$veh_type) | is.null(fleet_data$model_year)
       | is.null(fleet_data$fleet_composition)){
      stop(sprintf("Arguments 'veh_type','fleet_composition', and 'model_year' input is required in the 'fleet_data' for %s database")
           ,ef_data_base)
    }
  }
  if (ef_data_base == "emfac_usa" | ef_data_base == "moves_usa") {
    if(is.null(fleet_data$veh_type) | is.null(fleet_data$model_year)
       | is.null(fleet_data$fleet_composition)){
      stop(sprintf("Arguments 'veh_type','fleet_composition', and 'model_year'  input \nis required  in the 'fleet_data' for %s database")
           ,ef_data_base)
    }
  }
  if (ef_data_base == "emep_europe") {
    if(is.null(fleet_data$euro)| is.null(fleet_data$fuel)|is.null(fleet_data$tech)| is.null(fleet_data$fleet_composition)){
      stop(sprintf("Arguments 'speed','euro','fleet_composition', and 'speed' input \nis required  in the 'fleet_data' for %s database"
                   ,ef_data_base))
    }
  }
  
  # Check transport model input
  message("   Transport model")
  
  if (is.character(gps) & !missing(output_path) ) {
    stop("User should provide 'gps' and 'output_path' if wants to save emission into a provided folder.")
  } # [666] checar condicao / comportamento
  
  message("Emissions estimates")
  
  # EF ----------------
  
  
  # [666] Pra que gerar esse temp_ef aqui???
  if(ef_data_base == "cetesb_brazil"){
    temp_ef <- ef_cetesb_brazil(pollutant = pollutant,
                         veh_type = fleet_data$veh_type,
                         model_year = as.numeric(fleet_data$model_year),
                         as_list = TRUE)
  }
  # [666] Pra que gerar esse temp_ef aqui???
  
  
  
  # Emission function -----
  emission_estimate <- function(gps){ # i = 1  
    
    # [666] essa funcao aqui nao tinha q ter os parametros de frota e ef q sao passados adiante?
    
    # i) Read GPS linestrings or DT ----
    if (is.character(gps)) {
      tmp_gps <- readr::read_rds(gps)
    }else{
      tmp_gps <- gps #[666] duvida, isso nao duplica o objeto e sobrecarrega a memoria?
    }
    # ii) Get EF  ----
    if (ef_data_base == "emep_europe") {
      
      # [666] tenho a impressao que te q fazer esse call `data()` pra carregar o dado
      # mas nao sei se o melhor lugar pra charm é aqui ou dentro da funcao ef_emep_europe()
      data('ef_europe_db') 
      
      
      # calculate emission
      temp_ef <- ef_emep_europe(pollutant = pollutant,
                           speed = tmp_gps$speed,
                           veh_type = fleet_data$veh_type,
                           tech = fleet_data$tech,
                           euro = fleet_data$euro,
                           fcorr = 1)
    }
    if(ef_data_base == "emfac_usa"){
      
      # [666] alias, o nome dos arquivos dos dados de emis gtfs2emis/data/ poderiam 
      # seguir um mesmo padrao. Tipo ef_pais_fonte_db.rda
      data('usa_emfac_db') 
      
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
                      , dist = units::set_units(tmp_gps$dist, "km") # [666] acho q nao precisar setar units de novo
                      , ef = temp_ef
                      , aggregate = FALSE
                      , as_list = TRUE)
    
    # iv) Add data -----
    # Add fleet info
    if (ef_data_base != "emep_europe") {
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
    temp_emis$gps <- tmp_gps
    
    # v) Save emissions -----
    
    return(temp_emis)
  }
  # Loop emissions ----
  
  ## No output ----
  if (is.null(output_path)) {
    emisLine <- emission_estimate(gps)
    return(emisLine)
    
  }else{
    
    #[666] acho q precisa rever aqui o fluxo do codigo
    
    input_gps_names <- list.files(gps, full.names = FALSE)
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
