#' @importFrom magrittr %>%
#' @importFrom data.table := .SD .I .GRP %like% setDT
# @importFrom utils data
NULL

`%nin%` = Negate(`%in%`)

`%nlike%` = Negate(`%like%`)

# Globals
utils::globalVariables(c('id', 'range_id', 'i.range', '.', 'trip_id', 'shape_pt_lon',
                         'shape_pt_lat', 'SHP', 'hora_liberacao', 'departure_time', 'Placa',
                         'Ano_fabricacao', 'tipo_de_veiculo', 'categoria', 'modelo_chassi',
                         'shape_id'))
utils::globalVariables(c('gps_df', 'gps_line_names'))
utils::globalVariables(c('Euro.Standard', 'Fuel', 'Load', 'Pollutant', 'Road.Slope', 'Segment',
                         'Technology','g','km'))
utils::globalVariables(c('temp_ef', 'temp_ef1', 'ef'))
utils::globalVariables(c('ano', 'dist', 'emission_factor', 'euro'))
utils::globalVariables(c('geometry', 'f_linhas', 'interval_id', 'i.interval', 'grp'))
utils::globalVariables(c('emis_post', 'time_dt', 'temp_pol'))
utils::globalVariables(c('europe'))
utils::globalVariables(c('usa', 'temp_emfac', 'Calendar Year', 'Model Year', 'EF', 'EF Model',
                         'Year', 'lower_speed_interval', 'upper_speed_interval'))
utils::globalVariables(c('lkm_inter', 'ratio', 'temp_lkm'))
utils::globalVariables(c('Fcorr','fuel_cor','cn','t95'))
