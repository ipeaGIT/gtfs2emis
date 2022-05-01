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
library(gtfs2gps)
library(data.table)
library(magrittr)
library(ggplot2)
library(units)
library(sf)

## ---- message = FALSE---------------------------------------------------------
gtfs <- gtfs2gps::read_gtfs(system.file("extdata/poa.zip", package = "gtfs2gps")) %>%
        # gtfstools::filter_by_weekday(weekday ='monday') 
        gtfs2gps::filter_week_days() 
        # %>% gtfstools::filter_by_shape_id(shape_id = '176-1') 


## ---- message = FALSE---------------------------------------------------------
tp_model <- transport_model(gtfs_data = gtfs, parallel = TRUE)


## ---- message = FALSE, fig.width=3, fig.height=3------------------------------
head(tp_model)

tp_model <- sf::st_as_sf(tp_model) #[666] Essa transfomacao pra sf nao devia ser feita dentro do transport_model() ?
plot(tp_model["speed"])

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_europe <- data.frame(  veh_type = c("Ubus Midi <=15 t","Ubus Std 15 - 18 t" ,"Ubus Artic >18 t")
                                   , euro = c("III","IV","V")
                                   , fuel = rep("D",3)
                                   , tech = c("-","SCR","SCR") #[666] o q significa o tracinho? nao tem info de tech daquele veiculo? nesse caso, o q o modelo faz?
                                   , fleet_composition = c(0.4,0.5,0.1)) #
fleet_data_ef_europe

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_emfac <- data.frame(  veh_type = "BUS_URBAN_D"
                                  , model_year = 2011:2015
                                  , fuel = "D"
                                  , calendar_year = 2019
                                  , fleet_composition = rep(0.2,5))
fleet_data_ef_emfac

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_moves <- data.frame(  veh_type = "BUS_URBAN_D"
                                  , model_year = 2011:2015
                                  , fuel = "D"
                                  , calendar_year = 2016
                                  , fleet_composition = rep(0.2,5))
fleet_data_ef_moves

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_cetesb <- data.frame( veh_type = c("BUS_MICRO_D", "BUS_URBAN_D", "BUS_ARTIC_D")
                                  , model_year = c(2010, 2012, 2018)
                                  , fuel = rep("D", 3)
                                  , fleet_composition = c(0.4, 0.4, 0.2))
fleet_data_ef_cetesb

## ---- message = FALSE---------------------------------------------------------
sf_emis <- emission_model( gps = tp_model
                          , ef_data_base = "emep_europe"
                          , fleet_data = fleet_data_ef_europe
                          , pollutant = c("CO","PM10","CO2","CH4","NOx")
                          )
class(sf_emis)
names(sf_emis)

## ---- message = FALSE---------------------------------------------------------
sf_emis$time_column <- sf_emis$gps$timestamp

names(sf_emis)
my_emis_summary <- emis_summary(emi_list = sf_emis,
                    emi_var = "emi", 
                    by = "time", 
                    time_column = "time_column",
                    veh_var = "veh_type", 
                    pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                 emi_var = "emi", 
                                 by = "veh_type", 
                                 time_column = "time_column",
                                 veh_var = "veh_type", 
                                 pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                emi_var = "emi", 
                                by = "veh_type", 
                                time_column = "time_column",
                                veh_var = c("veh_type", "euro", "fuel"),
                                pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
my_emis_summary <- emis_summary(emi_list = sf_emis,
                                 emi_var = "emi", 
                                 by = "pollutant", 
                                 time_column = "time_column",
                                 veh_var = "veh_type", 
                                 pol_var = "pollutant") 
my_emis_summary

## ---- message = FALSE---------------------------------------------------------
sf_emis$time_column <- sf_emis$gps$timestamp
my_emis_dt <- emi_to_dt(emi_list = sf_emis
                    ,emi_vars = "emi"
                    ,veh_vars = c("veh_type", "euro", "fuel")
                    ,pol_vars = "pollutant"
                    ,segment_vars = "time_column")
head(my_emis_dt, 5)

## ---- message = FALSE---------------------------------------------------------
grid_gps <- vein::make_grid(spobj = sf_emis$gps, width =  0.25 / 102.47) # 500 meters
grid_gps <- sf::st_as_sf(grid_gps)

## ---- message = FALSE, fig.width=4,fig.height=4-------------------------------
names(sf_emis$emi)
# we add info of emissions into gps file
sf_emis$gps <- cbind(sf_emis$gps,sf_emis$emi)

pol_grid <- emis_grid(data = sf_emis$gps,
                     emi = "CO_Euro_IV",
                     grid = grid_gps,
                     time_class = 'all periods')
plot(pol_grid["CO_Euro_IV"])

