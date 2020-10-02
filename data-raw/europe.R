rm(list=ls())
library(openxlsx)
library(magrittr)
library(data.table)
`%nin%` = Negate(`%in%`)
`%nlike%` = Negate(`%like%`)
# REPORT -----------------------
# 
# 1) EMEP/EEA air pollutant emission inventory guidebook 2019
#    1.A.3.b.i, 1.A.3.b.ii, 1.A.3.b.iii, 1.A.3.b.iv
#    Passenger cars, light commercial trucks, heavy-duty vehicles including
#    buses and motor cycles
# 2) EMEP/EEA for 2016 and 2013 were also used to add Fuel Consumption 
# and CO2 emissions factor
#
# download in --------------------

# dir.create("test_joao/ef")
# emep2019 <- openxlsx::read.xlsx("test_joao/ef/2019.xlsx",sheet = 2) %>% data.table::setDT()# 
# tempdest <- "test_joao/ef/1.A.3.b Road transport annex HDV files.zip"
# download.file(url = link2009,destfile = tempdest)
# download.file(url = link2019,destfile = "test_joao/ef/2019.xlsx")
# unzip(zipfile = tempdest, exdir = "test_joao/ef/")
# link2016 <- "https://www.eea.europa.eu/publications/emep-eea-guidebook-2016/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-3-b-i-1/at_download/file"
keep_digits <- function(i){
  exp <- regmatches(i, gregexpr("[[:digit:]]+", i)) %>% 
    unlist() %>% as.numeric()
  return(exp)
}
# read 2019 ------------------
# --
link2019 <- "https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/road-transport-appendix-4-emission/at_download/file"


emep2019 <- openxlsx::read.xlsx(link2019,sheet = 2) %>% data.table::setDT()
emep2019 <- emep2019[Category %in% "Buses" & Segment %like% "Buses",]
emep2019[Pollutant %in% "PM Exhaust",Pollutant := "PM10"]
emep2019[Mode %in% "", Mode := NA]
emep2019[Technology %in% "", Technology := NA]
emep2019[,`:=`(`50` = NULL,`Bio.Reduction.Factor.[%]` = NULL,
               `EF.[g/km].or.ECF.[MJ/km]` = NULL)]
emep2019[is.na(Load), Load := 0.5]
emep2019[is.na(Road.Slope), Road.Slope := 0.0]
emep2019[,Function := "(Alpha*Speed^2+Beta*Speed+Gamma+Delta/Speed)/(Epsilon*Speed^2+Zita*Speed+Hta)*(1-rf)*k"]
emep2019[,Function_ID := "2019"]
emep2019[,k := 1]
emep2019[,Thita := 0]
data.table::setnames(emep2019,old = "Reduction.Factor.[%]", new = "RF")
# ---
# read 2016 ------------------------
# ---

link2016 <- "https://www.eea.europa.eu/publications/emep-eea-guidebook-2013/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-3-b-road-transport-annex-hdv-files.zip/at_download/file"
a <- tempdir()
zipfile2016 <- paste0(a,"/1.A.3.b.i-iv Exhaust emissions from road transport annex HDV files.zip")
download.file(url = link2016,destfile = zipfile2016)
unzip(zipfile = zipfile2016, exdir = a)
filepath2016 <- paste0(a,"/1.A.3.b.i-iv Exhaust emissions from road transport data annex Sept2014 update.xlsx")
openxlsx::getSheetNames(filepath2016)
#eq2016 <- openxlsx::read.xlsx(xlsxFile = filepath2016,sheet = "HDV - BUS Equations") %>% 
#  data.table::setDT()
emep2016 <- openxlsx::read.xlsx(xlsxFile = filepath2016,sheet = "HDVs - BUS") %>% 
  data.table::setDT()

# organize 
# > head(emep2019,1)
# Category   Fuel                 Segment Euro.Standard Technology Pollutant Mode Road.Slope Load
# Buses Diesel Urban Buses Midi <=15 t  Conventional       <NA>        CO <NA>      -0.06    0
#Min.Speed.[km/h] Max.Speed.[km/h]         Alpha       Beta    Gamma     Delta    Epsilon       Zita
# 11               86 -9.713108e-05 0.02241283 2.552201 -1.785515 0.00121752 0.01119629
# Hta Reduction.Factor.[%]                                   Function      Equation
# 0.1641829                    0 ((a*x^2+b*x+c+d/x)/(e*x^2+f*x+g)*(1-rf))*k Equation 2019
emep2016 <- emep2016[Sector %in% "Buses" & 
                       `Sub-Sector` %like% "Bus" &
                       Pollutant %in% "FC",]
emep2016[Technology %like% "SCR", Technology := "SCR"]
emep2016[Technology %like% "EGR", Technology := "EGR"]
emep2016[Technology %nlike% "EGR" & Technology %nlike% "SCR", Technology := NA]
emep2016[,`:=`(`50` = NULL,
               R2 = NULL,
               EF = NULL,
               RF = 0,
               Fuel = "Diesel", 
               Mode = NA,
               k = 1)]
emep2016[Euro.No. == 5, Euro.Standard := "Euro V"]
emep2016[Euro.No. == 6, Euro.Standard := "Euro VI"]
emep2016[,Euro.No. := NULL]
emep2016[,Load := Load / 100]
emep2016[is.na(Load), Load := 0.5]
data.table::setnames(emep2016,"Slope","Road.Slope")
emep2016[is.na(Road.Slope), Road.Slope := 0.0]
# fix formula names
# manual edit using formula below
# emep2016[,.SD[1],by = .(Formula)][,.(Equation,Formula)] %>% dput()
formulas_dt <- data.table::data.table(
  Equation = c("Equation 3", "Equation 12", "Equation 9", 
               "Equation 11", "Equation 4", "Equation 16", "Equation 10", "Equation 1"), 
  Formula = c("(Alpha*(Beta^Speed)*(Speed^Gamma))",
              "(Alpha/(1+(Beta*Exp(((-1)*Gamma)*Speed))))", 
              "(1/(Alpha+(Beta*(Speed^Gamma))))", 
              "(Alpha-(Beta*Exp(((-1)*Gamma)*(Speed^ Delta))))", 
              "(Alpha*(Speed^Beta))+(Gamma*(Speed^Delta))", 
              "(exp((Alpha+(Beta/Speed))+(Gamma*log(Speed))))", 
              "(1/(Alpha+(Beta*Speed)))", 
              "(Alpha*(Speed^3)+(Beta*(Speed^2))+(Gamma*Speed)+Delta)"))
  emep2016[formulas_dt,on = "Equation", Formula := i.Formula]
  # rename files, so it matches with 2019
  head(emep2016,1)
  dim(emep2016)
  head(emep2019,1)
  dim(emep2019)
  data.table::setnames(emep2016,old = c("Sector","Sub-Sector",
                                        "Vmin","Vmax","Equation","Formula","Ita"),
                       new = c("Category","Segment",
                               "Min.Speed.[km/h]","Max.Speed.[km/h]","Function_ID","Function","Hta"))
  
  # reorder columns of 'emep2016' based on 'emep2019'
  data.table::setcolorder(emep2016,neworder = names(emep2019))
  head(emep2016,1)
  head(emep2019,1)
  
  # add co2
  # make a copy from fuel consumption from 2009 EMEP/EEA data.base
  # table_32.9 
  # Ratios of hydrogen to carbon and oxygen to carbon atoms for different reference
  # blend fuels (REF) used in vehicle testing and estimated values for non-reference
  # fuels and blends
  # r_hc = 1.86, r_oc = 0.0
  # Eco2 = 44.011 * FC / (12.001 * r_hc + 16 * r_oc)
  #
  fileco2 <- data.table::copy(emep2016)[,Pollutant := "CO2"]
  fileco2[,k := 44.011 / (12.001 * 1.86 + 16*0 ) ]
  # rbind2016
  
  emep2016 <- data.table::rbindlist(list(emep2016,fileco2))
  
  # ---
  # EMEP 2009
  # ---
  link2009 <- "https://www.eea.europa.eu/publications/emep-eea-emission-inventory-guidebook-2009/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1.a.3.b-road-transport-annex-hdv-files.zip/at_download/file"
  a <- tempdir()
  zipfile2009 <- paste0(a,"/1.A.3.b Road transport annex HDV files_2009.zip")
  download.file(url = link2009,destfile = zipfile2009)
  unzip(zipfile = zipfile2009, exdir = a)
  # list.files(a)
  filepath2009 <- paste0(a,"/1.A.3.b Road transport annex HDV files.xlsx")
  openxlsx::getSheetNames(filepath2009)
  emep2009 <- openxlsx::read.xlsx(xlsxFile = filepath2009,sheet = "ARTEMIS") %>% 
    data.table::setDT()
  
  # organize
  # head(emep2019,1)
  # Category   Fuel                 Segment Euro.Standard Technology Pollutant Mode Road.Slope Load
  # Buses Diesel Urban Buses Midi <=15 t  Conventional       <NA>        CO <NA>      -0.06    0
  # Min.Speed.[km/h] Max.Speed.[km/h]         Alpha       Beta    Gamma     Delta    Epsilon       Zita
  # 11               86 -9.713108e-05 0.02241283 2.552201 -1.785515 0.00121752 0.01119629
  # Hta Reduction.Factor.[%]                                   Function Function_ID k Thita
  # 0.1641829                    0 ((a*x^2+b*x+c+d/x)/(e*x^2+f*x+g)*(1-rf))*k        2019 1     0
  emep2009 <- emep2009[Type %in% "BUS" & 
                         SSC_NAME %like% "Buses" & 
                         Pollutant %in% "FC",]
  emep2009[Type %in% "BUS", Type := "Buses"]
  emep2009[Subsegment %like% "SCR",Technology := "SCR"]
  emep2009[Subsegment %like% "EGR",Technology := "SCR"]
  # fix variable of functions
  emep2009[,.SD[1],by = Function_ID][,.(Function,Function_ID)]
  formulas_dt <- data.table::data.table(Function = c(
    "(Alpha*(Beta^Speed))*(Speed^Gamma)",
    "(Alpha*(Speed^Beta))+(Gamma*(Speed^Delta))",
    "(exp((Alpha+(Beta/Speed))+(Gamma*log(Speed))))",
    "(Alpha*(Speed^3)+Beta*(Speed^2)+Gamma*Speed+Delta)",
    "(Epsilon+Alpha*exp(((-1)*Beta)*Speed)+(Gamma*exp(((-1)*Delta)*Speed)))"),
    Function_ID = c(0,1, 13, 14, 4))
  emep2009[formulas_dt,on = "Function_ID", Function := i.Function]
  # rename euro.standards
  list_euro <- paste("Euro",c("I","II","III","IV","V"))
  emep2009[TEC_NAME %like% "Euro VI", Euro.Standard := "Euro VI"]
  for(i in list_euro){ emep2009[TEC_NAME %like% paste0(i," "), Euro.Standard := i]}
  emep2009[TEC_NAME %in% "Conventional", Euro.Standard := "Conventional"]
  # remove euro standard of phase "V" and "VI"
  # since it has already been used in newer EF database (2016)
  emep2009 <- emep2009[Euro.Standard %nin% c("Euro V","Euro VI"),]
  emep2009[,`:=`(TEC_NAME = NULL, Mode = NA,
                 R2 = NULL, Subsegment = NULL, k =1,
                 Thita = 0,
                 RF = 0,
                 Calc_func = NULL, Fuel = "Diesel",
                 `100` = NULL, Speed = NULL)]
  emep2009[,Load := Load / 100]
  # rename
  data.table::setnames(emep2009,old = c("Type","SSC_NAME",
                                        "Vmin","Vmax",
                                        "Slope",
                                        "a","b","c","d","e","f","g"),
                       new = c("Category","Segment",
                               "Min.Speed.[km/h]","Max.Speed.[km/h]",
                               "Road.Slope",
                               "Alpha","Beta","Gamma","Delta","Epsilon","Zita","Hta"))
  # reorder columns of 'emep2009' based on 'emep2019'
  data.table::setcolorder(emep2009,neworder = names(emep2019))
  head(emep2009,1)
  head(emep2019,1)
  # add co2
  # make a copy from fuel consumption from 2009 EMEP/EEA data.base
  # table_32.9 
  # Ratios of hydrogen to carbon and oxygen to carbon atoms for different reference
  # blend fuels (REF) used in vehicle testing and estimated values for non-reference
  # fuels and blends
  # r_hc = 1.86, r_oc = 0.0
  # Eco2 = 44.011 * FC / (12.001 * r_hc + 16 * r_oc)
  #
  fileco2 <- data.table::copy(emep2009)[,Pollutant := "CO2"]
  fileco2[,k := 44.011 / (12.001 * 1.86 + 16*0 ) ]
  # rbind2016
  
  emep2009 <- data.table::rbindlist(list(emep2009,fileco2))
  
  #
  # rbindlist all emission factors
  #
  europe <- data.table::rbindlist(list(emep2009,emep2016,emep2019))
  #
  # rename Function_ID
  europe[,Function_ID := .GRP, by = Function]
  #
  # export
  #
  
  # break()
  # for(i in colnames(europe)[1:9]){ # i = colnames(europe)[1]
  #   print(europe[,get(i)] %>% unique())
  # }
  usethis::use_data(europe,overwrite = TRUE)
  