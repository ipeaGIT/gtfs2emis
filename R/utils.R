#' @importFrom magrittr %>%
#' @importFrom data.table := .SD .I .GRP %like% setDT
NULL

# Globals
utils::globalVariables(c('id', '.'))
utils::globalVariables(c('Euro', 'Fuel', 'Load', 'Pol','Pollutant','time', 'Slope', 'Segment','segment_id',
                         'Technology'))
utils::globalVariables(c( 'from_stop_id', 'geometry', 'i.geometry', 'shape_id', 'stop_sequence', 'to_stop_id'))
utils::globalVariables(c('ef_europe_db'))
utils::globalVariables(c('calendar_year', 'model_year', 'ef','pollutant'))
utils::globalVariables(c('lkm_inter', 'ratio', 'temp_lkm'))
utils::globalVariables(c('year','i.den','i.s','i.pah','i.cn','i.t95','den_base',
                         'pah_base','cn_base','t95_base','s_base','den_imp','pah_imp',
                         'cn_imp','t95_imp','s_imp',
                         'from_stop_id', 'geometry', 'i.geometry',
                         'shape_id', 'stop_sequence', 'to_stop_id'))
# ef_cetesb
utils::globalVariables(c("ef_brazil_db","tmp_model_year"))
# ef_moves
utils::globalVariables(c("fuel_type","usa_moves_db"))
# ef_emfac
utils::globalVariables(c("usa_emfac_db"))
# emission_model / transport_model
utils::globalVariables(c("arrival_time", "cumdist", "cumtime", "departure_time",
                         "dist", "files_gps_names","speed"))