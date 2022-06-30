## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ---- message = FALSE---------------------------------------------------------
simple_fleet_file <- system.file("extdata/irl_dub/irl_dub_fleet.txt", package = "gtfs2emis")
simple_fleet_df <- read.csv(simple_fleet_file)
head(simple_fleet_df)

## ---- message = FALSE---------------------------------------------------------
detailed_fleet_file <- system.file("extdata/bra_cur/bra_cur_fleet.txt", package = "gtfs2emis")
detailed_fleet_df <- read.csv(detailed_fleet_file)
head(detailed_fleet_df)

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_cetesb <- data.frame( veh_type = c("BUS_MICRO_D", "BUS_URBAN_D", "BUS_ARTIC_D")
                                  , model_year = c(2010, 2012, 2018)
                                  , fuel = rep("D", 3)
                                  , fleet_composition = c(0.4, 0.4, 0.2))
fleet_data_ef_cetesb

## ---- message = FALSE---------------------------------------------------------
fleet_data_ef_europe <- data.frame(  veh_type = c("Ubus Midi <=15 t","Ubus Std 15 - 18 t" ,"Ubus Artic >18 t")
                                   , euro = c("III","IV","V")
                                   , fuel = rep("D",3)
                                   , tech = c("-","SCR","SCR")
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

