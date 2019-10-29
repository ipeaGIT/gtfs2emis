#
# prep data
# 
library(data.table)
library(dplyr)
library(sf)
gps <- read.table("data/gps_for_etufor_2019/shape004-I.txt",header = T); gps <- head(gps,-1)
# useful data
gps <- gps[,c("trip_id","shape_pt_lon","shape_pt_lat","departure_time","speed","dist")]
# time
gps$departure_time <- gps$departure_time %>% as.character() %>% as.ITime() %>% hour()
# bind
a <- sf::st_linestring(x = matrix(c(gps$shape_pt_lon[1:10000], gps$shape_pt_lat[1:10000]), ncol = 2)) %>% st_sfc() %>% st_sf()
plot(a) 
sf::st_linestring(x = matrix(c(c(0,1,1,0), c(0,0,1,1)), ncol = 2)) %>% st_sfc() %>% st_sf() %>% plot()
b
geometry <- sf::st_sfc(geometry)
geometry <- sf::st_sf(geometry = geometry)



a <- gps$shapes[,
                           {
                             geometry <- sf::st_linestring(x = matrix(c(shape_pt_lon, shape_pt_lat), ncol = 2))
                             geometry <- sf::st_sfc(geometry)
                             geometry <- sf::st_sf(geometry = geometry)
                           }
                           , by = shape_id
                           ]

sf::st_as_sf(temp_shapes, crs = crs) %>%
  sf::as_Spatial() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(length = sf::st_length(geometry) %>% units::set_units("km"))
}

a <- gps[1:5,c("shape_pt_lon","shape_pt_lat")]  %>% Line() %>% list()
a1 <- Lines(a,ID="b")
a <- lapply(1:5,function(i){Lines(list(a[i,],ID=i))})

l1 = cbind(c(1,2,3),c(3,2,2))
l1a = cbind(l1[,1]+.05,l1[,2]+.05)
l2 = cbind(c(1,2,3),c(1,1.5,1))
Sl1 = Line(l1)
Sl1a = Line(l1a)
Sl2 = Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
