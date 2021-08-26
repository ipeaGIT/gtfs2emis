

# 1) load libraries------------
# install.packages("dbplyr")
# install.packages("RMariaDB")
# install.packages("Rcpp")

rm(list=ls())
library("Rcpp")
library("dbplyr")
library("dplyr")
library("RMariaDB")
library("data.table")
library("ggplot2")


# 2) connect MariaDB----
con <- DBI::dbConnect(RMariaDB::MariaDB(),
                      host = "localhost",
                      db = "usa_cities_out", # your database output
                      user = "moves",
                      password = "moves")


# List Tables

dbListTables(con)

# RateperDistance

# rate_per_distance_db <- dplyr::tbl(con, "rateperdistance")

# SqL run

dbGetQuery(con,'
  SELECT processID
FROM `rateperdistance`
WHERE processID = 1
LIMIT 5
')

dt_copy <- DBI::dbGetQuery(con,'
SELECT pollutantID, sourceTypeID, regClassID, fuelTypeID,
modelYearID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
FROM rateperdistance
WHERE ratePerDistance > 0
GROUP BY MOVESRunID, pollutantID, modelYearID,
fuelTypeID, avgSpeedBinID
')

# 3) copy of database-----

dt <- data.table::copy(dt_copy)
data.table::setDT(dt)

## * pollutantID-----
# ref:
# https://github.com/USEPA/EPA_MOVES_Model/blob/master/docs/MOVES3CheatsheetOnroad.pdf


dt[pollutantID == 1, pollutant := "TGH"]
dt[pollutantID == 2, pollutant := "CO"]
dt[pollutantID == 3, pollutant := "NOx"]
dt[pollutantID == 5, pollutant := "CH4"]
dt[pollutantID == 90, pollutant := "CO2"]
dt[pollutantID == 91, pollutant := "EC"] # Total Energy Consumption
dt[pollutantID == 100, pollutant := "PM10"]
dt[pollutantID == 110, pollutant := "OC"] # Organic Carbon
dt[pollutantID == 112, pollutant := "Elemental_carbon"] # Elemental Carbon
dt[pollutantID == 115, pollutant := "Sulfate_Particulate"] # Sulfate Particulate
dt[pollutantID == 118, pollutant := "NonECPM"] # Composite - NonECPM

## * sourceTypeID -----

dt[sourceTypeID == 42,  source_type := "Transit Bus"]

# * fuelTypeID -----

unique(dt$fuelTypeID)
dt[fuelTypeID == 1, fuel_type := "G"]
dt[fuelTypeID == 2, fuel_type := "D"]
dt[fuelTypeID == 3, fuel_type := "CNG"] # Compressed Natural Gas

## * modelYearID -----
unique(dt$modelYearID)

data.table::setnames(dt,"modelYearID","model_year")

## * avgSpeedBinID -----
unique(dt$avgSpeedBinID)

dt <- dt[avgSpeedBinID != 0,]
dt[avgSpeedBinID == 1,  `:=`(lower_speed_interval = 0.0 ,upper_speed_interval = 2.5)]
dt[avgSpeedBinID == 2,  `:=`(lower_speed_interval = 2.5 ,upper_speed_interval = 7.5)]
dt[avgSpeedBinID == 3,  `:=`(lower_speed_interval = 7.5 ,upper_speed_interval = 12.5)]
dt[avgSpeedBinID == 4,  `:=`(lower_speed_interval = 12.5,upper_speed_interval = 17.5)]
dt[avgSpeedBinID == 5,  `:=`(lower_speed_interval = 17.5,upper_speed_interval = 22.5)]
dt[avgSpeedBinID == 6,  `:=`(lower_speed_interval = 22.5,upper_speed_interval = 27.5)]
dt[avgSpeedBinID == 7,  `:=`(lower_speed_interval = 27.5,upper_speed_interval = 32.5)]
dt[avgSpeedBinID == 8,  `:=`(lower_speed_interval = 32.5,upper_speed_interval = 37.5)]
dt[avgSpeedBinID == 9,  `:=`(lower_speed_interval = 37.5,upper_speed_interval = 42.5)]
dt[avgSpeedBinID == 10, `:=`(lower_speed_interval = 42.5,upper_speed_interval = 47.5)]
dt[avgSpeedBinID == 11, `:=`(lower_speed_interval = 47.5,upper_speed_interval = 52.5)]
dt[avgSpeedBinID == 12, `:=`(lower_speed_interval = 52.5,upper_speed_interval = 57.5)]
dt[avgSpeedBinID == 13, `:=`(lower_speed_interval = 57.5,upper_speed_interval = 62.5)]
dt[avgSpeedBinID == 14, `:=`(lower_speed_interval = 62.5,upper_speed_interval = 67.5)]
dt[avgSpeedBinID == 15, `:=`(lower_speed_interval = 67.5,upper_speed_interval = 72.5)]
dt[avgSpeedBinID == 16, `:=`(lower_speed_interval = 72.5,upper_speed_interval = 77.5)]

dt[,
   c("lower_speed_interval","upper_speed_interval") := 
     lapply(.SD,units::set_units,"miles/h"),
   .SDcols = c("lower_speed_interval","upper_speed_interval")]
dt[,
   c("lower_speed_interval","upper_speed_interval") := 
     lapply(.SD,units::set_units,"km/h"),
   .SDcols = c("lower_speed_interval","upper_speed_interval")]
dt[,
   c("lower_speed_interval","upper_speed_interval") := 
     lapply(.SD,round,1),
   .SDcols = c("lower_speed_interval","upper_speed_interval")]

## * emission factor -----

data.table::setnames(dt,"AVG(ratePerDistance)","ef")
dt[,ef := units::set_units(ef,"g/km")]
dt[pollutant == "EC",ef := units::set_units(as.numeric(ef),"J/km")]

## * adjust columns -----
dt[,`:=`(pollutantID = NULL,
         sourceTypeID = NULL,
         regClassID = NULL,
         fuelTypeID = NULL,
         avgSpeedBinID = NULL,
         ratePerDistance = NULL)]

# change order of columns
data.table::setcolorder(dt,
                        c("source_type","fuel_type","model_year",
                          "pollutant","lower_speed_interval",
                          "upper_speed_interval","ef"))


# 4) plot-----

# * according to speed bin----
dt %>%
  filter(model_year == 2015) %>%
  filter(pollutant %in% c("CH4","NOx","CO2","CO","PM10")) %>%
  ggplot()+
  geom_line(aes(x= avgSpeedBinID,y=`AVG(ratePerDistance)`,color = fuelType ))+
  facet_wrap(~pollutant,scales = "free")

# * according to EF
dt %>%
  filter(model_year == 2015) %>%
  filter(pollutant %in% c("CH4","NOx","CO2","CO","PM10")) %>%
  filter(as.numeric(lower_speed_interval) == 28.2) %>% 
  filter(fuel_type == "D") %>% 
  ggplot()+
  geom_bar(aes(x= source_type ,y= as.numeric(ef),
               fill = fuel_type),
           stat="identity",
           width=.5,
           position = "dodge")+
  facet_wrap(~pollutant,scales = "free")


# 5) export-----
usa_moves_db <- data.table::copy(dt)
usethis::use_data(usa_moves_db,overwrite = TRUE)





