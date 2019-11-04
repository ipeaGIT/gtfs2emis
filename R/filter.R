#
# data analysis sptrans
#
rm(list=ls())
library(vein)
library(stringr)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# data import
filepath <- "L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/"
dt <- fread("data/gps_spo_sptrans_2019/70912.txt")
# stop id
id0 <- which(!is.na(dt$stop_sequence))
id1 <- shift(id0-1,-1,fill=nrow(dt))
list_ids <- lapply(1:length(id0), function(i){id0[i]:id1[i]})
#  
dtnew <- dt[0,c("trip_id","id","departure_time","stop_id","stop_sequence","dist","speed")]
dtf <- list()
for(i in 1:length(id0)){
  dtf[[i]] <- dt[list_ids[[i]],
            c("trip_id","id","departure_time","stop_id","stop_sequence","dist","speed")][1]
  dtf[[i]] <- st_sf(dtf[[i]],geometry=st_sfc(st_linestring(x=as.matrix(dt[list_ids[[i]],c("shape_pt_lon","shape_pt_lat")]))),crs=4326)
}
plot(dtf$geometry[[5]])
dtf <- dtf %>%  rbindlist() %>% st_sf()
# length
dtf$dist <- st_length(dtf$geom)
#
# fator de emissao
#
ub_co <- ef_cetesb(p = "CO", veh = "UB", year = 2019)[1:7]
ub_co_2019 <- ub_co[1]
#
# emissions
#

dtf$veh <- 1
dtf$dist_m <- set_units(dtf$dist, "m")
dtf$lkm <- set_units(dtf$dist_m, "km")
dtf$emissions <- dtf$veh * ub_co_2019 * dtf$lkm
g_005 <- make_grid(dtf, 0.005)
break()
#
#
#
#
#
spobj <- dtf
net <- sf::st_as_sf(spobj)
net$id <- NULL
netdata <- as.data.frame(sf::st_set_geometry(net, NULL))
for (i in 1:length(netdata)) {
  netdata[, i] <- as.numeric(netdata[, i])
}
net <- sf::st_sf(netdata, geometry = net$geometry)
g <- sf::st_as_sf(g)
g$id <- 1:nrow(g)
if (!missing(sr)) {
  if (class(sr)[1] == "character") {
    sr <- as.numeric(substr(sp::CRS(sr), 12, nchar(sr)))
  }
  message("Transforming spatial objects to 'sr' ")
  net <- sf::st_transform(net, sr)
  g <- sf::st_transform(g, sr)
}
if (type == "lines") {
  netdf <- sf::st_set_geometry(net, NULL)
  snetdf <- sum(netdf, na.rm = TRUE)
  cat(paste0("Sum of street emissions ", round(snetdf, 
                                               2), "\n"))
  ncolnet <- ncol(sf::st_set_geometry(net, NULL))
  net <- net[, grep(pattern = TRUE, x = sapply(net, is.numeric))]
  namesnet <- names(sf::st_set_geometry(net, NULL))
  net$LKM <- sf::st_length(sf::st_cast(net[sf::st_dimension(net) == 
                                             1, ]))
  netg <- suppressMessages(suppressWarnings(sf::st_intersection(net, 
                                                                g)))
  netg$LKM2 <- sf::st_length(netg)
  xgg <- data.table::data.table(netg)
  xgg[, 1:ncolnet] <- xgg[, 1:ncolnet] * as.numeric(xgg$LKM2/xgg$LKM)
  xgg[is.na(xgg)] <- 0
  dfm <- xgg[, lapply(.SD, eval(parse(text = FN)), na.rm = TRUE), 
             by = "id", .SDcols = namesnet]
  id <- dfm$id
  dfm$id <- NULL
  dfm <- dfm * snetdf/sum(dfm, na.rm = TRUE)
  cat(paste0("Sum of gridded emissions ", round(sum(dfm, 
                                                    na.rm = T), 2), "\n"))
  dfm$id <- id
  gx <- data.frame(id = g$id)
  gx <- merge(gx, dfm, by = "id", all = TRUE)
  gx[is.na(gx)] <- 0
  gx <- sf::st_sf(gx, geometry = g$geometry)
  return(gx)
}
else if (type == "points") {
  netdf <- sf::st_set_geometry(net, NULL)
  snetdf <- sum(netdf, na.rm = TRUE)
  cat(paste0("Sum of point emissions ", round(snetdf, 
                                              2), "\n"))
  ncolnet <- ncol(sf::st_set_geometry(net, NULL))
  namesnet <- names(sf::st_set_geometry(net, NULL))
  xgg <- data.table::data.table(sf::st_set_geometry(suppressMessages(suppressWarnings(sf::st_intersection(net, 
                                                                                                          g))), NULL))
  xgg[is.na(xgg)] <- 0
  dfm <- xgg[, lapply(.SD, eval(parse(text = FN)), na.rm = TRUE), 
             by = "id", .SDcols = namesnet]
  cat(paste0("Sum of gridded emissions ", round(sum(dfm[, 
                                                        -"id"], na.rm = T), 2), "\n"))
  gx <- data.frame(id = g$id)
  gx <- merge(gx, dfm, by = "id", all.x = TRUE)
  gx[is.na(gx)] <- 0
  gx <- sf::st_sf(gx, geometry = g$geometry)
  return(gx)
}






g_005 <- make_grid(dtf$geom,width = .005)

plot(dtf["emissions"], axes = T, pal = cpt(colorRampPalette = T))
gx <- emis_grid(spobj = dtf[, "emissions"], g = g_005)
gx <- gx[gx$emissions > 0, ]  

#

aa <- readRDS("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo/simulacoes/estendida/SIM5/emi/CO2/E25_PC_1400_2000_age_20.rds")
aa <- emis_post(aa,by="streets_wide")