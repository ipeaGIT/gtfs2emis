rm(list=ls())
library(openxlsx)
library(magrittr)
library(data.table)
`%nin%` = Negate(`%in%`)
`%nlike%` = Negate(`%like%`)
# REPORT 
# 
# 1) EMEP/EEA air pollutant emission inventory guidebook 2019
#    1.A.3.b.i, 1.A.3.b.ii, 1.A.3.b.iii, 1.A.3.b.iv
#    Passenger cars, light commercial trucks, heavy-duty vehicles including
#    buses and motor cycles
# 2) Check purpose of 2016 data
# 3) EMEP/EEA for 2013 and 2009 were also used to add Fuel Consumption 
# and CO2 emissions factor
#
# 
body <- "https://www.eea.europa.eu/publications/"
link2019 <- paste0(body,"emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/at_download/file")
link2016 <- paste0(body,"emep-eea-guidebook-2016/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-3-b-i-1/at_download/file")
#link2013 <- paste0(body,"emep-eea-guidebook-2013/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-3-b-road-transport-annex-hdv-files.zip/at_download/file")
#link2009 <- paste0(body,"emep-eea-emission-inventory-guidebook-2009/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1.a.3.b-road-transport-annex-hdv-files.zip/at_download/file")

# 
# Emep 2019 ----
# 

emep2019_raw <- openxlsx::read.xlsx(link2019,sheet = 2) %>% data.table::setDT()

emep2019 <- data.table::copy(emep2019_raw)
emep2019[,.N,by = .(Category,Fuel,Segment,Euro.Standard,Technology,Pollutant)][N>1,]#[,.(Category,Fuel,Segment,Euro.Standard,Technology,Pollutant)]

# simply 'Category' names
emep2019$Category %>% unique()
emep2019 <- emep2019[Category %in% "Buses",] 

# simplify 'Fuel' names
#
# hydrid vehicle (HV)
# plug-in hybrid vehicle (PHEV)
# electric vehicle with range-extender (EREV)
# batery electric vehicle (BEV)
# 
emep2019$Fuel %>% unique()
emep2019[Fuel %in% "Diesel", Fuel := "D"]
emep2019[Fuel %in% "Diesel Hybrid ~ Diesel", Fuel := "DHD"]
emep2019[Fuel %in% "Diesel Hybrid ~ Electricity", Fuel := "DHE"]
emep2019[Fuel %in% "CNG", Fuel := "CNG"]
emep2019[Fuel %in% "Biodiesel", Fuel := "BD"]

# simplify 'Segment'
unique(emep2019$Segment)

emep2019[,Segment := stringr::str_replace_all(Segment,"Articulated","Artic")]
emep2019[,Segment := stringr::str_replace_all(Segment,"Urban Buses","Ubus")]
emep2019[,Segment := stringr::str_replace_all(Segment,"Standard","Std")]
emep2019[,Segment := stringr::str_replace_all(Segment,"Urban Biodiesel Buses","Ubus Std 15 - 18 t")]
emep2019[,Segment := stringr::str_replace_all(Segment,"Urban CNG Buses","Ubus Std 15 - 18 t")]
emep2019[,Segment := stringr::str_replace_all(Segment,"Ubus Diesel Hybrid","Ubus Std 15 - 18 t")]

names(emep2019)

# simplify "Euro.Standard"

# Petrol Passanger Cars
# 
# pre ECE vehicles - up to 1971
# ECE-15.00 and ECE 15.01 - 1972 to 1977
# ECE-15.02 - 1978 to 1980
# ECE-15.03 - 1981 to 1985
# ECE-15.04 - 1985 to 1992

data.table::setnames(emep2019,"Euro.Standard","Euro")
unique(emep2019$Euro)

emep2019[Euro %like% "Euro 6" | Euro %like% "Euro VI", Euro := "VI"]
emep2019[Euro %in% "Euro 5" | Euro %in% "Euro V", Euro := "V"]
emep2019[Euro %in% "Euro 4" | Euro %in% "Euro IV", Euro := "IV"]
emep2019[Euro %in% "Euro 3" | Euro %in% "Euro III", Euro := "III"]
emep2019[Euro %in% "Euro 2" | Euro %in% "Euro II", Euro := "II"]
emep2019[Euro %in% "Euro 1" | Euro %in% "Euro I", Euro := "I"]

# simplify 'Pollutant' names
unique(emep2019$Pollutant)
data.table::setnames(emep2019,"Pollutant","Pol")
emep2019[Pol %in% "PM Exhaust",Pol := "PM10"]

# simplify 'mode'
unique(emep2019$Mode)
emep2019[Mode %in% "", Mode := NA]

# simplify Technology
unique(emep2019$Technology)
emep2019[Technology %in% "", Technology := NA]

# exclude extra columns
emep2019[1]
emep2019[,`:=`(`15` = NULL,`Bio.Reduction.Factor.[%]` = NULL,
               `EF.[g/km].or.ECF.[MJ/km]` = NULL)]
# simplify Load
unique(emep2019$Load)
emep2019[is.na(Load), Load := 0.5]

# simplify Road.Slope
unique(emep2019$Road.Slope)
emep2019[is.na(Road.Slope), Road.Slope := 0.0]

# add Function
emep2019[,Function := "(Alpha*Speed^2+Beta*Speed+Gamma+Delta/Speed)/(Epsilon*Speed^2+Zita*Speed+Hta)*(1-rf)*k"]
emep2019[,Function_ID := "1"]
emep2019[,k := 1]
emep2019[,Thita := 0]

# rename colmuns
emep2019[1]
data.table::setnames(emep2019,old = c("Reduction.Factor.[%]","Max.Speed.[km/h]","Min.Speed.[km/h]","Road.Slope"), 
                     new = c("RF","Vmax","Vmin","Slope"))


# avoid duplicated EF
ColNames <- c("Category","Fuel","Segment","Euro","Technology","Pol","Slope","Load","Vmin","Vmax")
emep2019 <- emep2019[,.SD[1],by = ColNames]

# avoid duplicated EF, with only change on 'Mode' character
ColNames <- c("Category","Fuel","Segment","Euro","Technology","Pol","Slope","Load","Vmin","Vmax",
              "Alpha","Beta","Gamma","Delta","Epsilon","Zita","Hta","RF","Function","Function_ID","k","Thita")
emep2019 <- emep2019[,.SD[1],by = ColNames]


emep2019[,Mode := NULL]

# 
# Emep 2016 ---------
#


a <- tempdir()
download.file(link2016,destfile = paste0(a,"2016ef.xlsx"))
emep2016 <- openxlsx::read.xlsx(xlsxFile = paste0(a,"2016ef.xlsx"), sheet = 1) %>% data.table::setDT() 

# simplify by 'Pollutant' and 'Category"
unique(emep2016$Category)
emep2016 <- emep2016[Pollutant %in% "FC" & Category %in% "Buses",]

names(emep2016)

# simplify 'Subsector' <-> 'Segment'
unique(emep2016$Segment)

emep2016[,Segment := stringr::str_replace_all(Segment,"Articulated","Artic")]
emep2016[,Segment := stringr::str_replace_all(Segment,"Urban Buses","Ubus")]
emep2016[,Segment := stringr::str_replace_all(Segment,"Standard","Std")]
emep2016[,Segment := stringr::str_replace_all(Segment,"Urban Biodiesel Buses","Ubus Std 15 - 18 t")]
emep2016[,Segment := stringr::str_replace_all(Segment,"Urban CNG Buses","Ubus Std 15 - 18 t")]
emep2016[,Segment := stringr::str_replace_all(Segment,"Ubus Diesel Hybrid","Ubus Std 15 - 18 t")]

# fuel
emep2016$Fuel %>% unique()
emep2016[Fuel %in% "Diesel", Fuel := "D"]
emep2016[Fuel %in% "Biodiesel", Fuel := "BD"]

# simplify Euro
data.table::setnames(emep2016,"Euro.Standard","Euro")
unique(emep2016$Euro)
emep2016[Euro %like% "Euro 6" | Euro %like% "Euro VI", Euro := "VI"]
emep2016[Euro %in% "Euro 5" | Euro %in% "Euro V", Euro := "V"]
emep2016[Euro %in% "Euro 4" | Euro %in% "Euro IV", Euro := "IV"]
emep2016[Euro %in% "Euro 3" | Euro %in% "Euro III", Euro := "III"]
emep2016[Euro %in% "Euro 2" | Euro %in% "Euro II", Euro := "II"]
emep2016[Euro %in% "Euro 1" | Euro %in% "Euro I", Euro := "I"]

# Simplify names
names(emep2016)
data.table::setnames(emep2016,c("Min.Speed.[km/h]",
                                "Max.Speed.[km/h]",
                                "Reduction.Factor.[%]",
                                "EF.[g/km].or.ECF.[MJ/km]",
                                "Pollutant","Road.Slope"),
                     c("Vmin","Vmax","RF","EF","Pol","Slope"))

# add function column
emep2016[,Function := "(Alpha*Speed^2+Beta*Speed+Gamma+Delta/Speed)/(Epsilon*Speed^2+Zita*Speed+Hta)*(1-rf)*k"]
emep2016[,Function_ID := "1"]
# simplify others
names(emep2016)
emep2016[,`:=`(`120` = NULL,k = 1,`Bio.Reduction.Factor.[%]` = NULL,EF = NULL)]

# avoid duplicated EF, with only change on 'Mode' character
ColNames <- c("Category","Fuel","Segment","Euro","Technology","Pol","Slope","Load","Vmin","Vmax",
              "Alpha","Beta","Gamma","Delta","Epsilon","Zita","Hta","RF","Function","Function_ID","k","Thita")
emep2016 <- emep2016[,.SD[1],by = ColNames]
emep2016[,Mode := NULL]

# add CO2
co2_2016 <- data.table::copy(emep2016)[,Pol := "CO2"]
co2_2016[,k := 44.011 / (12.001 * 1.86 + 16*0 ) ]


emep2016_final <- data.table::rbindlist(l = list(emep2016,co2_2016))


#
# rbindlist all emission factors-----------------
#

names(emep2016_final)
names(emep2019)

break()
europe <- data.table::rbindlist(list(emep2019,emep2016_final),use.names = TRUE)

#
# NA to "-"
#
NumericCols <- names(europe)[names(europe) %nin% c('Slope','Load')]
for(i in NumericCols) europe[is.na(get(i)),(i) := "-"]
europe[is.na(Slope),Slope := 0]
europe[is.na(Load), Load := 0.5]

#
# export
#
usethis::use_data(europe,overwrite = TRUE)
rm(list=ls())
