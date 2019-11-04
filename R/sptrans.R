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
a <- seq(1,length(ids),by=1);a1 <- a[-1]-1; a1 <- c(a1,length(ids))
limits <- lapply(1:length(a), function(i){a[i]:a1[i]})
break()
i=1
  dt <- lapply(limits[[i]],function(j){fread(input = paste0(filepath,ids[j]))}) %>% rbindlist() 
  dt$veh <- 1
  dt$dist_m <- set_units(dt$dist, "m")
  dt$lkm <- set_units(dt$dist_m, "km")
  dt$emissions <- dt$veh * ub_co_2019 * dt$lkm
  dft <- st_as_sf(dt, coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326)
  g_005 <- make_grid(dft, 0.005)
  gx <- emis_grid(spobj = dft[, "emissions"], g = g_005, type = "points")
  gx <- gx[gx$emissions > 0, ]  
  
  #write_sf(gx,paste0("output/sptrans/emis_CO_batch",i,".shp"))
}
  
# gxs
plot(gx["emissions"], axes = T, pal = cpt(colorRampPalette = T,rev = T), lty = 0,
     main = "Emmissoes [g]")
break()
mapview(gx,zcol="emissions")
mapview(gxf,zcol="emission")
