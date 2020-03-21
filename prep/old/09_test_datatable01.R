rm(list=ls())
library(vein)
library(stringr)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
library(plyr)
library(geobr)
library(profvis)
library(tibble)
# setwd

# Intro -------------------------------------------------------------------


setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
source("R/01_read_gps.R")
# data import
gtfs <- "gtfs_spo_sptrans_2019-10/"
filepath <- paste0("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gtfs2gps/tests_joao/data/output/",gtfs)
ids <- list.files(path=filepath,full.names = TRUE)#;ids_saida <- str_remove(ids,".txt")
# hex reading
hex_grid <- readRDS("data/hex/3550308_09.rds") %>% st_transform(31983) %>% st_sf()
# script import
profvis({
  dt1 <- read_gps(ids[2])
})
# system time 02
profvis({
  dt2 <- read_gps01(filepath=ids[2])
})
# system time 03
a <- profvis({
  dt3 <- read_gps02(filepath=ids[2])
})
