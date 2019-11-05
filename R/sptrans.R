#
#
#
#
# SPTRANS
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
ids <- list.files(path=filepath);ids_saida <- str_remove(ids,".txt")
#
# fator de emissao
#
ub_co <- ef_cetesb(p = "CO", veh = "UB", year = 2019)[1:7]
ub_co_2019 <- ub_co[1]
#
# loop

# limits
#a <- seq(1,length(ids),by=1);a1 <- a[-1]-1; a1 <- c(a1,length(ids))
#limits <- lapply(1:length(a), function(i){a[i]:a1[i]})
#break()
source("R/filter.R")
system.time({
lapply(1:length(ids),function(i){ # length(a)
  # data preparation
  #limits <- a[i]:a1[i]
  filepath <- paste0("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/",
                     ids[i])
  dt <- read_gps(filepath) %>% st_as_sf()
  # emissions
  dt$veh <- 1
  dt$emissions <- dt$veh * ub_co_2019 * dt$dist
  #dft <- st_as_sf(dt, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
  # grid generation
  g_005 <- make_grid(dt, 0.00001)
  gx <- emis_grid(spobj = dt[, "emissions"], g = g_005,type="lines")
  gx <- gx[gx$emissions > 0, ]  
  # write
  write_sf(gx,paste0("output/sptrans/emis_CO_batch",ids[i],".shp"))
})
})
m1 <- mapview(dt$geometry)
m2 <- mapview(gx,zcol="emissions")
sync(m1,m2)

#
# reading files
#
rm(gx)
ids <- list.files(path="output/sptrans/")
gx <- lapply(ids,read_sf) %>% data.table::rbindlist() %>% st_as_sf()
gx <- read_sf("output/sptrans/emis_CO_batch1.shp")
plot(gx["emissions"], axes = T, pal = cpt(colorRampPalette = T,rev = T), lty = 0,
     main = "Emmissoes [g]")  
# gxs
plot(gx["emissions"], axes = T, pal = cpt(colorRampPalette = T,rev = T), lty = 0,
     main = "Emmissoes [g]")
break()
mapview(gx,zcol="emissions")
mapview(gxf,zcol="emission")
