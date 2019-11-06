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
library(h3jsr)
library(plyr)
library(geobr)
library(tibble)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# data import
filepath <- "L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/"
ids <- list.files(path=filepath);ids_saida <- str_remove(ids,".txt")

# script imports
source("R/read_gps.R")
source("R/hex_prep.R")

# emission factor
ub_co <- ef_cetesb(p = "CO", veh = "UB", year = 2019)[1:7]
ub_co_2019 <- ub_co[1]

# hex generation
hex_grid <- make_hex(resolution = 09,cod_muni=3550308,filepath="data/")

#
# loop

# limits
#a <- seq(1,length(ids),by=1);a1 <- a[-1]-1; a1 <- c(a1,length(ids))
#limits <- lapply(1:length(a), function(i){a[i]:a1[i]})
#break()

system.time({
lapply(1:length(ids),function(i){ # length(a)
  # data preparation
  #limits <- a[i]:a1[i]
  i=2
  filepath <- paste0("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/gtfs_spo_sptrans_2019-10/",
                     ids[i])
  dt <- fread(filepath)
  dim(dt)
  dt <- read_gps(filepath) %>% st_as_sf()
  # emissions
  dt$veh <- 1
  dt$emissions <- dt$veh * ub_co_2019 * dt$dist
  sf::write_sf(dt,"data/emi/3550308.shp")
  st_crs(dt);st_crs(hex_grid)
  
  
  # grid generation
  g_005 <- make_grid(spobj = dt$geometry,0.01)
  #g_005j <- make_gridj(spobj = dt,width =  0.0001,height = 0.0001)
  ggplot()+geom_sf(data=gg)+geom_sf(data=dt,aes(fill="emissions"))
  #gx <- emis_grid(spobj = dt[, "emissions"], g = g_005,type="lines")
  #ggplot(gx) + geom_sf(aes(fill="emissions"))
  #ggplot() + geom_sf(data=dt,aes(color="emissions"))
  #gx <- gx[gx$emissions > 0, ]  
  # write
  write_sf(gx,paste0("output/sptrans/emis_CO_batch",ids[i],".shp"))
})
})



class(dt)[1]

ggplot() + 
  geom_sf(data=dt)+
  geom_sf(data=g_005,aes(fill="id"))

mapview(dt$geometry)
mapview(dt["emissions"])
sync(m1,m2)

plot(dt$geometry,xlim=as.vector(st_bbox(dt))[c(1,3)],ylim=as.vector(st_bbox(dt))[c(2,4)])
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
