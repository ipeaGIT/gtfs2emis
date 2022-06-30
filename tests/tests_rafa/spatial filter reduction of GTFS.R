# spatial filter / reduction of GTFS

library(sf)
library(gtfstools)
library(ggplot2)
library(sfheaders)
library(mapview)

# curitiba
gtfs_file <- 'C:/Users/user/Downloads/curitiba.zip' # system.file("extdata/bra_cur/bra_cur_gtfs.zip", package = "gtfs2emis")
coords <- c(-25.431272, -49.270266)


# Detroit
gtfs_file <- 'C:/Users/user/Downloads/detroit.zip' # system.file("extdata/usa_det/usa_det_gtfs.zip", package = "gtfs2emis")
coords <- c(42.3366565526915, -83.05067360162002)

# Dublin
gtfs_file <- 'C:/Users/user/Downloads/dublin.zip' # system.file("extdata/irl_dub/irl_dub_gtfs.zip", package = "gtfs2emis")
coords <- c(53.3465939, -6.27217629)



buffer_size = 2000
# cut_gtfs <- function(gtfs_file, buffer_size, coords){

# read gtfs
gtfs <- gtfstools::read_gtfs(gtfs_file)

###### create buffer ----------------------------------------------------------

center <- st_as_sf(data.frame(id='center', lon=coords[2],lat=coords[1]),
                   coords = c("lon","lat"))
sf::st_crs(center) <- 4326

buff <- center |>
  st_transform(crs = '+proj=utm +zone23 +datum=WGS84 +units=m +ellps=WGS84 +towgs84=0,0,0') |>
  st_buffer(dist = buffer_size) |>
  st_transform(crs = 4326)


###### filter stops ----------------------------------------------------------

stops_sf <- gtfstools::convert_stops_to_sf(gtfs)
new_stops_sf <- st_intersection(stops_sf, buff)

ggplot() +
  geom_sf(data = buff) +
  geom_sf(data=stops_sf) +
  geom_sf(data=new_stops_sf, color='red')


# gtfs <- gtfstools::filter_by_stop_id(gtfs, stop_id = new_stops_sf$stop_id)
gtfs$stop_times <- subset(gtfs$stop_times, stop_id %in% new_stops_sf$stop_id)
gtfs$stops <- subset(gtfs$stops, stop_id %in% new_stops_sf$stop_id)

new_stops_sf$stop_id |> unique() |> length()
gtfs$stop_times$stop_id |> unique() |> length()
gtfs$stops$stop_id |> unique() |> length()


# filter trips and routes
gtfs$trips <- subset(gtfs$trips, trip_id %in% unique(gtfs$stop_times$trip_id))
gtfs$routes <- subset(gtfs$routes, route_id %in% unique(gtfs$trips$route_id))


###### filter shapes ----------------------------------------------------------

head(gtfs$shapes)
# shape_sf <- gtfstools::convert_shapes_to_sf(gtfs)
shapes_in_poits <- sfheaders::sf_point(gtfs$shapes, x = 'shape_pt_lon', y='shape_pt_lat',keep = TRUE)
st_crs(shapes_in_poits) <- 4326

new_shapes_sf <- st_intersection(shapes_in_poits, buff)

# ggplot() +
#   geom_sf(data = buff) +
#   geom_sf(data=shapes_in_poits) +
#   geom_sf(data=new_shapes_sf, color='red', size=.5)


new_shapes_df <- sfheaders::sf_to_df(new_shapes_sf, fill = TRUE)
setDT(new_shapes_df)[, c('shape_dist_traveled', 'id', 'sfg_id', 'point_id') := NULL]

setnames(new_shapes_df, 'x' , 'shape_pt_lon')
setnames(new_shapes_df, 'y' , 'shape_pt_lat')
setcolorder(new_shapes_df, c('shape_id', 'shape_pt_lat', 'shape_pt_lon', 'shape_pt_sequence'))


# filter trips and routes
gtfs$trips <- subset(gtfs$trips, shape_id %in% unique(new_shapes_df$shape_id))
gtfs$routes <- subset(gtfs$routes, route_id %in% unique(gtfs$trips$route_id))


nrow(gtfs$shapes)
nrow(new_shapes_df)
gtfs$shapes <- new_shapes_df

object.size(gtfs)
gtfs <- gtfstools::filter_by_weekday(gtfs, weekday = 'monday')
gtfs <- gtfstools::filter_by_time_of_day(gtfs, from = '08:00:00', to = '20:00:00')
object.size(gtfs)

# drop transfers table
if(!is.null(gtfs$transfers)){
  gtfs$transfers <- NULL
}

# reduce trips by 50%
object.size(gtfs)
all_trip_ids <- unique(gtfs$trips$trip_id) |> unique()
new_trip_ids <- sample(all_trip_ids, size = length(all_trip_ids) * 0.5, replace = FALSE)

gtfs$trips <- subset(gtfs$trips, trip_id %in% new_trip_ids)
gtfs$routes <- subset(gtfs$routes, route_id %in% gtfs$trips$route_id)
gtfs$stop_times <- subset(gtfs$stop_times, trip_id %in% new_trip_ids)
object.size(gtfs)


###### save ----------------------------------------------------------
gtfstools::write_gtfs(gtfs, path = basename(gtfs_file) )

# check output
t <- read_gtfs(basename(gtfs_file) )
gtfstools::convert_stops_to_sf(t) |> plot()
gtfstools::convert_shapes_to_sf(t) |> plot()


#   }



# validate
gtfstools::download_validator(path = '.')
gtfstools::validate_gtfs(gtfs = basename(gtfs_file), output_path = 'R:/Dropbox/git/gtfs2emis', validator_path = 'R:/Dropbox/git/gtfs2emis/gtfs-validator-v3.1.0.jar')




