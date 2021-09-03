

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


# 1.2) read dictionary-----
#setwd("L:/Proj_emission_routes/pacote/joao_gtfs2emis/gtfs2emis/")
abas <- openxlsx::getSheetNames("data-raw/moves_cheat_sheet.xlsx")

dic <- lapply(abas,function(i){
  openxlsx::read.xlsx("data-raw/moves_cheat_sheet.xlsx"
                      ,sheet = i) %>% data.table::setDT()
})
names(dic) <- abas


# 2) connect MariaDB----
# * usa cities out ----
# con1 <- DBI::dbConnect(RMariaDB::MariaDB(),
#                        host = "localhost",
#                        db = "usa_cities_out", # your database output
#                        user = "moves",
#                        password = "moves")

# temp_dt <- DBI::dbGetQuery(con1,'
# SELECT *
# FROM rateperdistance
# LIMIT 10
# ')
# 
# BBI::dbGetQuery(con1,'
# SELECT pollutantID, sourceTypeID, regClassID, fuelTypeID,
# modelYearID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
# FROM rateperdistance
# WHERE regClassID = 1
# WHERE sourceTypeID = 11
# WHERE ratePerDistance > 0
# GROUP BY MOVESRunID, pollutantID, modelYearID,
# fuelTypeID, avgSpeedBinID, hourID
# ')

# usa_cities_out <- DBI::dbGetQuery(con1,'
# SELECT pollutantID, sourceTypeID, regClassID, fuelTypeID,
# modelYearID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
# FROM rateperdistance
# WHERE ratePerDistance > 0
# GROUP BY MOVESRunID, pollutantID, modelYearID,
# fuelTypeID, avgSpeedBinID
# ')

# * wayne-----
con2 <- DBI::dbConnect(RMariaDB::MariaDB(),
                       host = "localhost",
                       db = "wayne_november_out_all_calendar", # your database output
                       user = "moves",
                       password = "moves")

wayne_out <- DBI::dbGetQuery(con2,'
SELECT yearID, pollutantID, sourceTypeID, regClassID, fuelTypeID,
modelYearID, avgSpeedBinID, ratePerDistance, AVG(ratePerDistance)
FROM rateperdistance
GROUP BY yearID, MOVESRunID, pollutantID, modelYearID,
fuelTypeID, avgSpeedBinID
')


# 3) copy of database-----

dt <- data.table::copy(wayne_out)
data.table::setDT(dt)

## * pollutantID-----
# ref:
# https://github.com/USEPA/EPA_MOVES_Model/blob/master/docs/MOVES3CheatsheetOnroad.pdf

dt$pollutantID %>% unique()
dt[dic$pollutants,on = c("pollutantID" = "ID"),pollutant := i.pollutantname]
dt[dic$pollutants,on = c("pollutantID" = "ID"),tmp_pollutant := i.short_name]
dt <- dt[!is.na(tmp_pollutant),]
dt[,pollutant  := NULL]
data.table::setnames(dt,"tmp_pollutant","pollutant")
# remove space
unique(dt$pollutant)
dt[,pollutant := gsub(" ","",pollutant)]


## * regClassID -----
unique(dt$regClassID)
dic$regulatory_class

#dt <- dt[regClassID == 48,]

## * sourceTypeID -----

dt$sourceTypeID %>% unique()
dt[sourceTypeID == 42,  source_type := "Transit Bus"]

# * fuelTypeID -----

unique(dt$fuelTypeID)
dt[dic$fuel_type,on = c("fuelTypeID" = "ID"),fuel_type := i.fuelType]
dt[fuel_type == "Gasoline",fuel_type := "G"]
dt[fuel_type == "Diesel",fuel_type := "D"]

## * modelYearID -----
unique(dt$modelYearID)

data.table::setnames(dt,"modelYearID","model_year")

## * avgSpeedBinID -----
unique(dt$avgSpeedBinID)

dt <- dt[avgSpeedBinID != 0,]
dic$speed_bin
dt[dic$speed_bin,on = c("avgSpeedBinID" = "ID"),
   `:=`(lower_speed_interval = i.lower_speed_limit,
        upper_speed_interval = i.upper_speed_limit) ]

dt[,
   c("lower_speed_interval","upper_speed_interval") := 
     lapply(.SD,as.numeric),
   .SDcols = c("lower_speed_interval","upper_speed_interval")]
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

## * calendar year -----
unique(dt$yearID)
data.table::setnames(dt,"yearID","calendar_year")
dt <- dt[calendar_year %in% c(2015:2022),]

## * adjust columns -----
dt[,`:=`(pollutantID = NULL,
         sourceTypeID = NULL,
         regClassID = NULL,
         fuelTypeID = NULL,
         avgSpeedBinID = NULL,
         ratePerDistance = NULL)]

# change order of columns
data.table::setcolorder(dt,
                        c("fuel_type","calendar_year","model_year",
                          "pollutant","lower_speed_interval",
                          "upper_speed_interval","ef"))

## * fix NA EF ----
dt[,id_speed := .GRP,by = .(lower_speed_interval,upper_speed_interval)]
cols <- c("fuel_type",
          "calendar_year",
          "pollutant",
          "source_type",
          "id_speed")
dt_invalid <- data.table::copy(dt)[as.numeric(ef) == 0]
dt_add <-  data.table::copy(dt)[as.numeric(ef) != 0][,.SD[1],by = cols]
dt_valid <-  data.table::copy(dt)[as.numeric(ef) != 0]

# update dt_invalid
dt_invalid <- dt_invalid[dt_add,
                         on = c("fuel_type" = "fuel_type",
                                "calendar_year" = "calendar_year",
                                "pollutant" = "pollutant",
                                "source_type" = "source_type",
                                "id_speed" = "id_speed"),
                         ef := i.ef]
# check nrow dt_invalid
dt_invalid[as.numeric(ef) == 0]

# remove zero EF
dt_invalid <- dt_invalid[as.numeric(ef) != 0]

# rbind dt_valid with dt_invalid
dt_bind <- data.table::rbindlist(list(dt_valid,dt_invalid),
                                 use.names = TRUE)
data.table::setkeyv(x = dt_bind,
                    cols = c("fuel_type",
                             "calendar_year",
                             "model_year",
                             "pollutant",
                             "source_type",
                             "id_speed"))

## * fix CH4 ID----
# dt[,.N,by =.(pollutant)]
# dt[pollutant == "CH4" & 
#      model_year < 2007 & 
#      fuel_type == "D",]
# dt[pollutant == "CH4" & 
#      model_year < 2007 & 
#      fuel_type == "D",.N,by = model_year]
# dt[pollutant == "CH4" & model_year == 1989,] # CNG
# dt[pollutant == "CH4" & model_year == 1992,] # CNG, G
# dt[pollutant == "CH4" & model_year == 2007,] # CNG, G, D

# 4) plot-----

# * according to speed bin----

dt_bind %>%
  filter(model_year == 2015) %>%
  filter(pollutant %in% c("CH4","NOx","CO2","CO","PM10")) %>%
  filter(calendar_year == 2019) %>% 
  ggplot()+
  geom_line(aes(x= id_speed,y= as.numeric(ef),color = fuel_type  ))+
  geom_point(aes(x= id_speed,y= as.numeric(ef),color = fuel_type  ))+
  facet_wrap(~pollutant,scales = "free")

# * according to EF
dt_bind %>%
  filter(model_year == 2015) %>%
  filter(pollutant %in% c("CH4","NOx","CO2","CO","PM10")) %>%
  filter(as.numeric(lower_speed_interval) == 28.2) %>% 
  filter(fuel_type == "D") %>% 
  filter(calendar_year == 2019) %>% 
  ggplot()+
  geom_bar(aes(x= source_type ,y= as.numeric(ef),
               fill = fuel_type),
           stat="identity",
           width=.5,
           position = "dodge")+
  facet_wrap(~pollutant,scales = "free")

# * according to year
dt_bind %>%
  #filter(pollutant %in% c("CH4")) %>%
  filter(as.numeric(lower_speed_interval) == 28.2) %>%
  filter(fuel_type != "CNG") %>% 
  filter(calendar_year == 2019) %>% 
  ggplot()+
  geom_point(aes(x= model_year,y= as.numeric(ef),
                 color = fuel_type))+
  geom_line(aes(x= model_year,y= as.numeric(ef),
                color = fuel_type))+
  facet_wrap(~pollutant,scales = "free_y")

# 5) export-----
usa_moves_db <- data.table::copy(dt_bind)
usethis::use_data(usa_moves_db,overwrite = TRUE)



