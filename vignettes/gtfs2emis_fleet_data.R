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

