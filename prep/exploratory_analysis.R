rm(list=ls())
library(magrittr)
library(gtfs2gps)
gtfss <- list.files(path = "../../../proj_emis_routes/data-raw/gtfs/all",full.names = TRUE,recursive = TRUE,
                    pattern = "week.gtfs")
routess <- list.files(path = "../../../proj_emis_routes/data-raw/gtfs/all",full.names = TRUE,recursive = TRUE,
                     pattern = "routes.geojson")
stopss <- list.files(path = "../../../proj_emis_routes/data-raw/gtfs/all",full.names = TRUE,recursive = TRUE,
                     pattern = "stops.geojson")
gtfss_names <- gsub("../../../proj_emis_routes/data-raw/gtfs/all/","",gtfss) 
gtfss_names <- gsub("/week.gtfs.zip","",gtfss_names)
gtfss_names <- gsub("/","",gtfss_names)

df <-lapply(c(1:27),function(i){ #  i = 1
  # ---
  # read gtfs
  # ---
  # i = 9 
  oldfile <- gtfs2gps::read_gtfs(gtfszip = gtfss[i], remove_invalid = FALSE)
  #newfile <- oldfile %>% gtfs2gps::remove_invalid()
  # --
  # read GEOJSON
  # --
  routes <- jsonlite::read_json(path = routess[i],simplifyVector = TRUE)
  geom <- lapply(seq_along(routes$features$properties$route_type), function(i){ # i = 1
    return( routes$features$geometry$coordinates[[i]] %>% sfheaders::sf_multilinestring() )
  }) %>% data.table::rbindlist() %>% sf::st_as_sf() %>% sf::st_set_crs(4326)
  mapview::mapview(geom$geometry)
  df_routes <- data.table::data.table(route_I = routes$features$properties$route_I,
                                      route_type = routes$features$properties$route_type,
                                      route_name = routes$features$properties$route_name)
  #df_routes$geometry <- sf::st_sf(geometry = geom$geometry,crs = 4326)
  # association
  route_name <- oldfile$routes[route_short_name %in% unique(df_routes$route_name),route_id]
  #link_route_id <- oldfile$trips[route_id %in% route_name,route_id] %>% unique()
  # --
  # stops GEOJSON
  # --
  df_stops <- jsonlite::read_json(path = stopss[i],simplifyVector = TRUE)
  # --
  # resume
  # --
  df <- data.table::data.table("city" = gtfss_names[i],
                               "GTFS_shape_id" = length(unique(oldfile$shapes$shape_id)),
                               "GTFS_route_id" = length(unique(oldfile$routes$route_id)),
                               "GJSON_route_id" = length(unique(df_routes$route_name)),
                               "GJSON_matchs_GTFS_route_id" = length(unique(route_name)),
                               "GTFS_stops" = length(unique(oldfile$stops$stop_id)),
                               "GJSON_stops" = length(unique(df_stops$features$properties$stop_I))
                               )
  print(df)
  }) %>% data.table::rbindlist()
