#
# emission grid
#
rm(list=ls())
library(vein)
library(stringr)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
library(h3jsr)
library(plyr)
library(geobr)
library(tibble)
# reading
hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()
dt <- sf::read_sf("data/emi/3550308.shp")%>% st_transform(31983) 
# defs
#net <- dt;g <- hex_grid;g$id <- 1:dim(g)[1]
# intersection
its <- sf::st_crosses(dt$geometry,hex_grid$geometry) %>% as.data.table()
colnames(its) <- c("emi_id","hex_id")
# rebuild vector based on its df
unico <-  unique(its$emi_id) 
its <- lapply(1:length(unico),function(i){
  aux <- unico[i]
  its[emi_id == its$emi_id[i],"total_dist"] <- dt$dist[aux]
  its[emi_id == its$emi_id[i],"total_emi"] <-  dt$emissns[aux]
  return(its[emi_id == its$emi_id[i],])
}) %>% data.table::rbindlist()
#
a <- sf::st_crop(net$geometry[2],g$geometry[884]) %>% sf::st_length()

sf::st_length(a)
sf::st_length(net$geometry[2])
sf::st_area(g$geometry[884])

sync(mapview(net$geometry[2]),mapview(g$geometry[884]))


netids <- its["row.id"==hexagons[1],"col.id"]


# visualization
sync(mapview(g$geometry[unique(its$row.id)]), mapview(net$geometry))


# 
netdf <- sf::st_set_geometry(net, NULL)
snetdf <- sum(netdf$emissns, na.rm = TRUE)
cat(paste0("Sum of street emissions ", round(snetdf, 
                                             2), "\n"))
ncolnet <- ncol(sf::st_set_geometry(net, NULL))
net <- net[, grep(pattern = TRUE, x = sapply(net, is.numeric))]
namesnet <- names(sf::st_set_geometry(net, NULL))
net$LKM <- sf::st_length(sf::st_cast(net[sf::st_dimension(net) == 
                                           1, ]))
netg <- suppressMessages(suppressWarnings(sf::st_intersection(net, 
                                                              g)))
netg1 <- suppressMessages(suppressWarnings(sf::st_intersection(g, 
                                                              net)))
netg$LKM2 <- sf::st_length(netg)
xgg <- data.table::data.table(netg)
xgg[, 1:ncolnet] <- xgg[, 1:ncolnet] * as.numeric(xgg$LKM2/xgg$LKM)
#xgg[is.na(xgg)] <- 0
#dfm <- xgg[, lapply(.SD, eval(parse(text = FN)), na.rm = TRUE), 
#           by = "id", .SDcols = namesnet]
id <- dfm$id
dfm$id <- NULL
dfm <- dfm * snetdf/sum(dfm, na.rm = TRUE)
cat(paste0("Sum of gridded emissions ", round(sum(dfm, 
                                                  na.rm = T), 2), "\n"))
dfm$id <- id
gx <- data.frame(id = g$id)
gx <- merge(gx, dfm, by = "id", all = TRUE)
gx[is.na(gx)] <- 0
gx <- sf::st_sf(gx, geometry = g$geometry) %>% which(is.)



#its <- sf::st_intersection(hex_grid$geometry,dt$geometry)
its <- sf::st_crosses(g$geometry,net$geometry) %>% as.data.table()
head(its)
its1 <- its[-which(its=="integer(0)")]
its <- its[which(length(its)>0)]

plot(hex_grid$geometry[unique(its$row.id)])
plot(its$geometry,add=T)
leafsync::sync(mapview(g$geometry[unique(its$row.id)]), mapview(net$geometry))
head(dt,1)
