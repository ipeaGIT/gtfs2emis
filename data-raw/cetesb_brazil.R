
# 1) load libraries-------

rm(list=ls())
library(data.table)
library(magrittr)



# download sysdata in vein rep

link_vein <- "https://github.com/atmoschem/vein/blob/master/R/sysdata.rda?raw=true"
download.file(url = link_vein,destfile = "sysdata.rda",mode = "wb")
load("sysdata.rda")

tmp_cetesb <- sysdata$cetesb 
data.table::setDT(tmp_cetesb)

# * select columns----

tmp_cetesb <- tmp_cetesb[, c("Age",
                             "Year",
                             "Pollutant",
                             "Proconve_HDV",
                             "Euro_EqHDV", 
                             "BUS_URBAN_D", 
                             "BUS_MICRO_D",
                             "BUS_COACH_D",
                             "BUS_ARTIC_D")]

## * setnames----
data.table::setnames(tmp_cetesb
                     , old = c("Age","Year","Pollutant")
                     , new = c("calendar_year","model_year","pollutant"))

## * pollutant----
# rename
tmp_cetesb[pollutant == "PM",pollutant := "PM10"]

# exclude
pol_to_exclude <- c("D_20_35","S_20_35","R_20_35","D_10_25",
                    "S_10_25","R_10_25","D_0_15","S_0_15","R_0_15")
tmp_cetesb <- tmp_cetesb[!(pollutant %in% pol_to_exclude),]


## * calendar year----
tmp_cetesb[,calendar_year := 2020]

## export-----
ef_brazil_db <- data.table::copy(tmp_cetesb)
usethis::use_data(ef_brazil_db,overwrite = TRUE)
