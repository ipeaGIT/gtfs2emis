#' @importFrom magrittr %>%
#' @importFrom data.table := .N .SD
NULL

`%nin%` = Negate(`%in%`)

#`%nlike%` = Negate(`%like%`)

# Globals
utils::globalVariables(c("input_folder", "output_folder", "hora_liberacao",
                         'id','range_id','i.range','.','trip_id','shape_pt_lon','shape_pt_lat','SHP',
                         'hora_liberacao','departure_time','Placa','Ano_fabricacao','tipo_de_veiculo',
                         'categoria','modelo_chassi','shape_id'))
# check valid shapeid

check_valid_shapeid <- function(gtfs_data){
  gtfs_data$shapes <- gtfs_data$shapes[shape_id %in% unique(gtfs_data$trips$shape_id),]
  return(gtfs_data)
}
