#' @title 
#' Running exhaust emissions factors for buses from Brazil (CETESB)
#' 
#' @description 
#' Returns a vector or `data.table` of emission factors for buses based on 
#' estimates from the  [Environment Company of Sao Paulo, Brazil (CETESB) 2019](https://cetesb.sp.gov.br/veicular/).
#' Emission factor estimates are expressed in units 'g/km'.
#'
#' @param pollutant character. Pollutants "CH4", "CO2", "PM10", "N2O", "NOx",
#'        "NO2", "NO", "RCHO", "ETOH" "KML" (Vehicle Kilometers Traveled), "FC" (Fuel Consumption), 
#'        "gD/KWH" (grams of Diesel per kWh), "gCO2/KWH" (grams of CO2 per per kWh), 
#'        "CO", "HC" (Total Hydrocarbon), "NMHC" (Non-Methane Hydrocarbon), 
#'        "FS"(Fuel Sales) and "NH3".
#' @param veh_type character. Vehicle categories by fuel: "BUS_URBAN_D", 
#'        "BUS_MICRO_D", "BUS_COACH_D" and "BUS_ARTIC_D".
#' @param model_year numeric. Vehicle model year. Supports `model_year` from 
#'        1960 to 2020.
#' @template as_list
#' 
#' @return data.table. Emission factors in units 'g/km' by model_year.
#' 
#' @details 
#' 
#' The new convention for vehicles names are translated from CETESB report:
#' 
#' | vehicle       | description                                                    |
#' | ------------- | --------------------------------------------------------------------- |
#' | BUS_URBAN_D   | Urban Bus Diesel (5perc bio-diesel) |
#' | BUS_MICRO_D   | Micro Urban Bus Diesel (5perc bio-diesel) |
#' | BUS_COACH_D   | Coach (inter-state) Bus Diesel (5perc bio-diesel) |
#' | BUS_ARTIC_D   | Articulated Urban Bus Diesel (5perc bio-diesel) |
#' 
#' The percentage varies of biofuels varies by law.
#' 
#' These emission factors are not exactly the same as the report of CETESB.
#' 
#' 1) In this emission factors, there is also NO and NO2 based on split by published in the EMEP/EEA
#' air pollutant emission inventory guidebook.
#' 2) Also, the emission factors were extended till 50 years of use, repeating the oldest value.
#' @md
#' @source \url{https://cetesb.sp.gov.br/veicular/relatorios-e-publicacoes/}
#' 
#' @family Emission factor model
#' 
#' @examples
#' df <- ef_brazil_cetesb(
#'           pollutant = c("CO","PM10","CO2","CH4","NOx"),
#'           veh_type = "BUS_URBAN_D",
#'           model_year = 2015,
#'           as_list = TRUE)
#' 
#' @export
ef_brazil_cetesb <- function(pollutant, veh_type, model_year, as_list = TRUE){
  
  # check inputs----
  
  checkmate::assert_vector(pollutant,min.len = 1,any.missing = FALSE,null.ok = FALSE)
  checkmate::assert_vector(veh_type,min.len = 1,any.missing = FALSE,null.ok = FALSE)
  checkmate::assert_vector(model_year,min.len = 1,any.missing = FALSE,null.ok = FALSE)
  checkmate::assert_numeric(model_year,lower = 1960,upper = 2020,any.missing = FALSE,min.len = 1)
  checkmate::assert_logical(as_list,len = 1)
  
  # pollutant
  names_pollutant <- unique(ef_brazil_cetesb_db$pollutant)
  for(i in pollutant) checkmate::assert_choice(i,names_pollutant ,null.ok = FALSE)
  
  # veh_type
  names_veh <- c('BUS_URBAN_D', 'BUS_MICRO_D', 'BUS_COACH_D', 'BUS_ARTIC_D')
  for(i in veh_type) checkmate::assert_choice(i, names_veh,null.ok = FALSE)
  
  # model_year and veh_type
  if(length(model_year) != length(veh_type) && length(veh_type) == 1){
    veh_type <- rep(veh_type, length(model_year))
  }
  if(length(model_year) != length(veh_type) && length(model_year) == 1){
    model_year <- rep(model_year, length(veh_type))
  }
  if(length(model_year) != length(veh_type)){
    stop("Arguments 'model_year' and 'veh_type' should have the same length.")
  }
  
  # loop through database
  tmp_model_year <- model_year
  ef_temp1 <- lapply(pollutant, function(p){ # p = pollutant[1]
    ef_temp <- lapply(seq_along(model_year), function(i){# i = 1
      ef_brazil_cetesb_db[pollutant == p &
                            model_year == tmp_model_year[i]
                          ,.SD
                          ,.SDcols = veh_type[i]][[1]]
    })
    ef_temp <- do.call(cbind, ef_temp)
    return(ef_temp)
  })
  
  ef_final_dt <- do.call(cbind, ef_temp1)
  ef_final_dt <- units::set_units(ef_final_dt,"g/km")  
  # rename colnames
  colnames(ef_final_dt) <- paste0(rep(pollutant
                                      , each = length(model_year)), "_"
                                  , model_year)
  
  # return in a data.table/list like format----
  
  # export list
  ef_final <- list("pollutant"   = rep(pollutant,each = length(veh_type))
                   ,"veh_type"   = rep(veh_type,length(pollutant))
                   ,"model_year" = rep(model_year,length(pollutant))
                   ,"EF"         = ef_final_dt)
  
  # export DT
  if(!as_list){
    #ef_final1 <- emis_to_dt(emi_list = ef_final
    #                       ,emi_vars = "EF"
    #                       ,veh_vars = "veh_type"
    #                       ,pol_vars = "pollutant")
    return(ef_final_dt)
  }else{
    
    # return function
    return(ef_final)
  }
}
