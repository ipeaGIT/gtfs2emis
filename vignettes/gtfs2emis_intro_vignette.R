## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ---- eval = FALSE, message = FALSE-------------------------------------------
#  # From CRAN
#  install.packages("gtfs2emis")
#  
#  # Dev. version with latest features
#  utils::remove.packages('gtfs2emis')
#  devtools::install_github("ipeaGIT/gtfs2emis")
#  

## ---- message = FALSE---------------------------------------------------------
library(gtfs2emis)
library(gtfstools)
library(progressr)
library(data.table)
library(ggplot2)
library(units)
library(sf)


## ---- message = FALSE---------------------------------------------------------
# path to GTFS.zip file
gtfs_file <- system.file("extdata/irl_dub/irl_dub_gtfs.zip", package = "gtfs2emis")

# read GTFS
gtfs <- gtfstools::read_gtfs(gtfs_file)

# Keep Monday services GTFS
gtfs <- gtfstools::filter_by_weekday(gtfs, 
                                         weekday = c('saturday', 'sunday'), 
                                         keep = FALSE)

# 66666666666 REMOVER
gtfs <- gtfstools::filter_by_trip_id(gtfs, trip_id = '619.3.60-40-d12-1.224.O')


## ---- message = FALSE---------------------------------------------------------
# generate transport model
progressr::with_progress( 
  
  tp_model <- transport_model(gtfs_data = gtfs,
                              spatial_resolution = 100,
                              parallel = TRUE) 
  )

head(tp_model)

## ---- message = FALSE, fig.width=5, fig.height=5------------------------------
ggplot(data = tp_model) +
  geom_sf(aes(color= as.numeric(speed))) +
  scale_color_continuous(type = "viridis")+
  labs(color = "Speed (km/h)")+
  theme_void()


## ---- message = FALSE---------------------------------------------------------
fleet_file <- system.file("extdata/irl_dub/irl_dub_fleet.txt", package = "gtfs2emis")

fleet_df <- read.csv(fleet_file)
head(fleet_df)


## ---- message = FALSE---------------------------------------------------------
emi_list <- emission_model(tp_model = tp_model,
                           ef_model = "ef_europe_emep",
                           fleet_data = fleet_df,
                           pollutant = c("CO2","PM10"),
                           reference_year = 2020
                           )

names(emi_list)

## ----eval = TRUE--------------------------------------------------------------
emi_dt <- emis_to_dt(emi_list = emi_list
                    ,emi_vars = "emi"
                    ,veh_vars = c("veh_type", "euro", "fuel")
                    ,pol_vars = "pollutant")

head(emi_dt) 


