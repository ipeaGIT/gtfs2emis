#' @importFrom magrittr %>%
#' @importFrom data.table := .N .SD .I .GRP
#' @importFrom utils data
NULL

`%nin%` = Negate(`%in%`)

#`%nlike%` = Negate(`%like%`)

# Globals
utils::globalVariables(c("hora_liberacao",
                         'id','range_id','i.range','.','trip_id','shape_pt_lon','shape_pt_lat','SHP',
                         'hora_liberacao','departure_time','Placa','Ano_fabricacao','tipo_de_veiculo',
                         'categoria','modelo_chassi','shape_id'))
utils::globalVariables(c('gps_df','gps_line_names'))
utils::globalVariables(c('Euro.Standard','Fuel','Load','Pollutant','Road.Slope','Segment','Technology','g','km'))
utils::globalVariables(c('temp_ef','temp_ef1','ef'))
utils::globalVariables(c('ano','dist','emission_factor','euro'))
utils::globalVariables(c('geometry','f_linhas','interval_id','i.interval','grp'))
utils::globalVariables(c('emfac','temp_emfac','Calendar Year','Model Year','EF','EF Model','Year','lower_speed_interval','upper_speed_interval'))
# check valid shapeid

check_valid_shapeid <- function(gtfs_data){
  gtfs_data$shapes <- gtfs_data$shapes[shape_id %in% unique(gtfs_data$trips$shape_id),]
  return(gtfs_data)
}
