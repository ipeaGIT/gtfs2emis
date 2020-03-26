#
# Emissions
#
rm(list = ls())
source('./R/fun/setup.R')


# setwd

# Intro -------------------------------------------------------------------
proj_cities <- data.table::data.table( abrev_city = c('cur'),
                                       name_city = c('Curitiba'),
                                       name_country = c('Brazil'),
                                       abrev_country = c('BRA'))
# ---------------------
# 0) Prep data Curitiba
#
# 0.1) download gps data from Curitiba
source('prep/01.01_download_gps_data_cur.R')
# 0.2) allocate vehicles with specific shape_id
source('prep/01.1_prep_cur_fleet.R')
# ----------------------
#
# 1) gtfs2gps
#
# read GTFS and export to GPS-like format
#

source('prep/01.0_prep_gtfs2gps.R')

create_gps_outputs(city_abrev = proj_cities$abrev_city,period_start = "00:00:01", period_end = "23:59:59")
# -----------------------
# 2) gps to linestring
#
# - read GPS and export to 'linestring'
#
# - add specific fleet of curitiba into a column for each 'shape_id'
#
source('prep/01.3_read_gps.R')
# fleet data
fleet <- readr::read_rds(paste0("../../data/fleet/",
                                proj_cities$abrev_city,"/",
                                proj_cities$abrev_city,".rds"))

# data output_gps
gps_output_fo <- paste0("../../data/gps/",proj_cities$abrev_city)

read_gps(input_folder = gps_output_fo,fleet_data = fleet)
# ---------------------
# 3) emission factor
#
source("prep/09_emep-eea_emission-factor.R")
source("prep/09_ef_hdv_scaled_2019.R")

ef <- openxlsx::read.xlsx("test_joao/references/copert/1.A.3.b.i-iv Road transport hot EFs Annex 2018_Dic.xlsx") %>% 
  data.table::as.data.table()
ef <- ef[Category %in% "Buses",]


# ----------------------
# 4) emissions
#
source("prep/02_emi-estimation.R")
input_folder1 = paste0("../../data/gps_linestring/",proj_cities$abrev_city)
output_folder1 = paste0("../../data/gps_linestring_emis/",proj_cities$abrev_city)
emis(pol_list = c("CO","NOx"),
     input_folder = input_folder1, output_folder = output_folder1,
     emission_factor = ef,overwrite = TRUE)

# ----------------------
# 5) post processing
#












# ----
# old stuff
# ----
# ----
  # data import
gtfs <- "gtfs_spo_sptrans_2019-10/"
filepath <- paste0("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/",gtfs)
ids <- list.files(path=filepath);ids_saida <- str_remove(ids,".txt")
# hex reading
hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()
# script imports
source("R/01_read_gps.R")


# Emission factor ---------------------------------------------------------

ub_co <- ef_cetesb(p = "CO", veh = "UB", year = 2009)[1]
ub_nox <- ef_cetesb(p = "NOx", veh = "UB", year = 2009)[1]
ub_pm <- ef_cetesb(p = "PM", veh = "UB", year = 2009)[1]
ub_nmhc <- ef_cetesb(p = "NMHC", veh = "UB", year = 2009)[1]
ub_co2 <- ef_cetesb(p = "CO2", veh = "UB", year = 2009)[1]
ub_ch4 <- ef_cetesb(p = "CH4", veh = "UB", year = 2009)[1]

# scaled emission factor
ef_ub_co <- vein::ef_hdv_scaled(dfcol = ub_co, 
                                v = "Ubus", 
                                t = "Std",
                                g = ">15 & <=18", eu = "V", 
                                gr = 0, l = 0.5, p = "CO")
ef_ub_nox <- vein::ef_hdv_scaled(dfcol = ub_nox, 
                                 v = "Ubus", 
                                 t = "Std",
                                 g = ">15 & <=18", eu = "V", 
                                 gr = 0, l = 0.5, p = "NOx")

ef_ub_nmhc <- vein::ef_hdv_scaled(dfcol = ub_nmhc, 
                                  v = "Ubus", 
                                  t = "Std",
                                  g = ">15 & <=18", eu = "V", 
                                  gr = 0, l = 0.5, p = "NMHC")
ef_ub_co2 <- vein::ef_hdv_scaled(dfcol = ub_co2, 
                                 v = "Ubus", 
                                 t = "Std",
                                 g = ">15 & <=18", eu = "V", 
                                 gr = 0, l = 0.5, p = "CO2")
ef_ub_ch4 <- vein::ef_hdv_scaled(dfcol = ub_ch4, 
                                 v = "Ubus", 
                                 t = "Std",
                                 g = ">15 & <=18", eu = "V", 
                                 gr = 0, l = 0.5, p = "CH4")

# break()
# i=1

# Emis estimation ---------------------------------------------------------

system.time({
  future.apply::future_lapply(seq_along(ids),function(i){ # seq_along(ids)
    # --
    # data preparation
    # --
    filepath1 <- paste0(filepath,
                        ids[i])
    dt <- data.table::fread(filepath1)
    dt <- read_gps(filepath1) %>% st_as_sf() %>% st_transform(31983) 
    
    
    
    # emissions
    dt$veh <- 1
    # emissions speed
    dt$emi_co <- dt$veh * ub_co * dt$dist * ef_ub_co[[1]](dt$speed)
    dt$emi_nox <- dt$veh * ub_nox * dt$dist * ef_ub_nox[[1]](dt$speed)
    dt$emi_pm <- dt$veh * ub_pm * dt$dist * ef_ub_pm[[1]](dt$speed)
    dt$emi_nmhc <- dt$veh * ub_nmhc * dt$dist * ef_ub_nmhc[[1]](dt$speed)
    dt$emi_co2 <- dt$veh * ub_co2 * dt$dist * ef_ub_co2[[1]](dt$speed)
    dt$emi_ch4 <- dt$veh * ub_ch4 * dt$dist * ef_ub_ch4[[1]](dt$speed)
    # --
    # gridded
    # --
    # intersect
    its <- sf::st_intersects(dt$geometry,hex_grid$geometry) %>% as.data.table()
    colnames(its) <- c("emi_id","hex_id")
    its$hex_id <- hex_grid$h3_address[its$hex_id]
    hex_city <- hex_grid[hex_grid$h3_address %in% unique(its$hex_id),]
    # emis grid
    hex_city$emi_co <- vein::emis_grid(spobj = dt["emi_co"],g = hex_city)$emi_co
    hex_city$emi_nox <- vein::emis_grid(spobj = dt["emi_nox"],g = hex_city)$emi_nox
    hex_city$emi_pm <- vein::emis_grid(spobj = dt["emi_pm"],g = hex_city)$emi_pm
    hex_city$emi_nmhc <- vein::emis_grid(spobj = dt["emi_nmhc"],g = hex_city)$emi_nmhc
    hex_city$emi_co2 <- vein::emis_grid(spobj = dt["emi_co2"],g = hex_city)$emi_co2
    hex_city$emi_ch4 <- vein::emis_grid(spobj = dt["emi_ch4"],g = hex_city)$emi_ch4
    # salve
    sf::write_sf(hex_city,paste0("data/emi_speed_grid/",gtfs,ids_saida[i],".shp"))
    sf::write_sf(dt,paste0("data/emi_speed_line/",gtfs,ids_saida[i],".shp"))
  })
})

# break()
mapview(hex_grid$geometry,alpha.regions = 0.0)+mapview(hex_city,zcol="emi_co")+
  mapview(dt$geometry)

