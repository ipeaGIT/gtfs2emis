check_valid_shapeid <- function(gtfs_data){
  gtfs_data$shapes <- gtfs_data$shapes[shape_id %in% unique(gtfs_data$trips$shape_id),]
  return(gtfs_data)
}
