#
# vein test on gps data
#
rm(list=ls())
library(vein)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# data import

dt <- fread(input = "data/gps_spo_sptrans_2019/70912_first1000.txt")
last_stopid <- last(which(is.na(dt$stop_id)%in%FALSE))
dt <- dt[1:last_stopid,]; dt[last_stopid,"dist"] <- 0
# only stop_id's
dtnew <- dt[-which(is.na(dt$stop_id)),] 
dtnew$dist <- 0
dtnew$dist[1] <- dtnew$cumdist[2]-dtnew$cumdist[1]
for(i in 2:(nrow(dtnew)-1)){
  dtnew$dist[i] <- dtnew$cumdist[i+1]-dtnew$cumdist[i]
}

head(dtnew)
sum(dtnew[,"dist"])
sum(dt[,"dist"])

#
# fator de emissao
#
ub_co <- ef_cetesb(p = "CO", veh = "UB", year = 2019)[1:7]
ub_co_2019 <- ub_co[1]
#
# emissoes -- dt
#
dt$veh <- 1
dt$dist_m <- set_units(dt$dist, "m")
dt$lkm <- set_units(dt$dist_m, "km")
dt$emissions <- dt$veh * ub_co_2019 * dt$lkm
dft <- st_as_sf(dt, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
plot(dft["emissions"], axes = T, pal = cpt(colorRampPalette = T))
g_001 <- make_grid(dft, 0.001)
gx <- emis_grid(spobj = dft[, "emissions"], g = g_001, type = "points")
gx <- gx[gx$emissions > 0, ]
plot(gx["emissions"], axes = T, pal = cpt(colorRampPalette = T), lty = 0,
     main = "Emmissoes [g]")
#
# emissoes -- dtnew
# 
dtnew$veh <- 1
dtnew$dist_m <- set_units(dtnew$dist, "m")
dtnew$lkm <- set_units(dtnew$dist_m, "km")
dtnew$emissions <- dtnew$veh * ub_co_2019 * dtnew$lkm
dftnew <- st_as_sf(dtnew, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
plot(dftnew["emissions"], axes = T, pal = cpt(colorRampPalette = T))
g_001 <- make_grid(dftnew, 0.001)
gxnew <- emis_grid(spobj = dftnew[, "emissions"], g = g_001, type = "points")
gxnew <- gxnew[gxnew$emissions > 0, ]
plot(gxnew["emissions"], axes = T, pal = cpt(colorRampPalette = T), lty = 0,
     main = "Emmissoes [g]")
#
# comparison
# 
m1 <- mapview(gxnew,zcol="emissions")
m2 <- mapview(gx,zcol="emissions")
sync(m1,m2)
par(mfrow=c(2,1))
p1 <- plot(gx["emissions"], axes = T, pal = cpt(colorRampPalette = T), lty = 0,
     main = "Emmissoes [g]")
p2 <- plot(gxnew["emissions"], axes = T, pal = cpt(colorRampPalette = T), lty = 0,
     main = "Emmissoes [g]")
