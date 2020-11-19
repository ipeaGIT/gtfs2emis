# ------------------------------------------------------------
# based on EMFAC2017 "Volume III – Technical Documentation V1.0.2 July 20, 2018"
# ------------------------------------------------------------
# Table 4.3-55 Speed Correction Factors for Diesel Buses
# ------------------------------------------------------------
# Diesel Bus | HD Diesel Truck | Speed Correction Curve
# Pre-2003   | Pre-2003        | SCF for Pre-2003 HDDT
# 2003-2006  | 2006-2006       | SCF for 2003-2006 HDDT
# 2007-2009  | 2007-2009       | SCF for 2007-2009 HDDT
# 2020+      | 2013+           | SCF for 2013+ HDDT
rm(list=ls())
library(magrittr)
library(ggplot2)
# ---------------------------------------------------------
# Table 4.3-49: Coefficients for EMFAC2017 HHDDT Speed Corrections Factors
# ---------------------------------------------------------
`%nin%` = Negate(`%in%`)
table4349 <- data.table::data.table(
  pollutant = c("HC","CO","NOx","NOx","PM","CO2"),
  from_year_group = c(2010,2010,2010,2013,2010,2010),
  to_year_group   = c(2020,2020,2012,2020,2020,2020),
  a = c(0.553, 3.64, 21.7, 21.3, 0,7450),
  b = c(-1.15, -1.2,-0.527,-0.702 ,0,-0.469),
  c = c(3.77*10^(-2), .35, 10.2, 6.14, 7.34, 3610),
  d = c(-1.33*10^(-3), -1.24*10^(-2), -3.85, -0.225, -0.297, 99.8),
  e = c(1.48*10^(-5), 1.19*10^(-4), 4.28*10^(-3), 2.25*10^(-3), 7.34*10^(-3), 1.07))
table4349_above <- data.table::copy(table4349)[pollutant %nin% "PM",]
table4349_above[ , `:=`(speed = "18.8-65mph",
                        a = 0, b = 0)]
table4349[pollutant %in% "PM",`:=`(speed = "all",
                                   a = 0,
                                   b = 0)]
table4349[pollutant %nin% "PM",`:=`(speed = "<18.8mph",
                                    c = 0,
                                    d = 0,
                                    e = 0)]
table4349 <- data.table::rbindlist(list(table4349,table4349_above))
# ---------------------------------------------------------
# Eq.4.3-10 SCF for all pollutants and speed below 18.8 mph
# for PM, only Eq. 4.3.11 is applied
# Eq.4.3.11 SCF for all pollutants and speed between 18.8 and 65 mph
# ---------------------------------------------------------
function_scf_diesel <- function(a,b,c,d,e,speed,temp_speed){
  switch(temp_speed,
         "<18.8mph" = (a * speed ^ b) / (a * 18.8 ^ b),
         "18.8-65mph" = (c + d * speed + e * speed^2)/(c + d * 18.8 + e * 18.8^2),
         "all" = (c + d * speed + e * speed^2)/(c + d * 18.8 + e * 18.8^2))
}
# ---------------------------------------------------------
# CNG Buses SCF
# ---------------------------------------------------------
# Table 4.3-57: Speed Correction Factors for 2008+ Model Year CNG Buses
# ---------------------------------------------------------
function_scf_cng <- function(speed,pol){
  switch(pol,
         "HC" = (-1.031 * log(speed) + 5.906),
         "CO" =  (-2.076 * log(speed) + 16.22),
         "NOx" = (-0.130 * log(speed) + 0.727),
         "PM" = (6.34*10^(-6) * speed^2 - 6.16*10^(-4) * speed + 1.74*10^(-2)),
         "CO2" = (-549 * log(speed) + 3597))
}
# --------------------------
# analysis
# --------------------------
year <- 1970:2020
speed <- 1:104 %>% units::set_units("km/h") %>% units::set_units("mile/h") %>% as.numeric()
vector_pol <- c("HC","CO","NOx","PM","CO2")
fuel <- c("Diesel","CNG")

# --------------------------
# SCF loop
# --------------------------
dt <- lapply(1:length(vector_pol),function(j){ 
  dt <- lapply(1:length(speed),function(i){
    dt <- lapply(1:length(year),function(k){ 
      dt <- lapply(1:length(fuel),function(l){# j = 1 | k = 39 | i = 1
        
        # message
        # message(paste0("pol = ",vector_pol[j]," | year = ",year[k]," | speed = ",round(speed[i],3)))
        #message(paste0("j = ",j," ; k = ",k," ; i = ",i,"; l = ",l))
        # speed
        temp_speed <- ifelse(speed[i] < 18.8 ,"<18.8mph","18.8-65mph")
        temp_table <- data.table::copy(table4349)[pollutant %in% vector_pol[j] & 
                                                    speed %in% temp_speed & 
                                                    from_year_group <= year[k] &
                                                    to_year_group >= year[k], ]
        
        if(vector_pol[j] == "PM"){
          temp_speed <- "all"
          temp_table <- data.table::copy(table4349)[pollutant %in% vector_pol[j] &
                                                      from_year_group <= year[k], ]
        }
        
        if(nrow(temp_table) == 0){
          scf <- 1
        }else{
          scf <- function_scf_diesel(a = temp_table$a,
                                     b = temp_table$b,
                                     c = temp_table$c,
                                     d = temp_table$d,
                                     e = temp_table$e,
                                     temp_speed = temp_table$speed,
                                     speed = speed[i])
        }
        
        #
        # cng buses 2008+
        #
        
        if(year[k] >= 2008 & fuel[l] == "CNG"){
          scf <- function_scf_cng(speed = speed[i],pol = vector_pol[j])
        }
        
        # export dt SCF correction
        
        export_dt <- data.table::data.table("speed" = speed[i],
                                            "scf" = scf,
                                            "pollutant" = vector_pol[j],
                                            "year" = year[k],
                                            "fuel" =  fuel[l])
        return(export_dt)
      }) %>% data.table::rbindlist()
      return(dt)
    }) %>% data.table::rbindlist()
    return(dt)
  }) %>% data.table::rbindlist()
  return(dt)
}) %>% data.table::rbindlist()


# ------------------------------------------------------------
# Table 4.3-56 Speed Correction Factors for CNG Buses
# ------------------------------------------------------------
# CNG Bus | HD Diesel Truck | Speed Correction Curve
# Pre-2003   | 2003-2006       | SCF for 2003-2006 HDDT
# 2003-2007  | 2007-2009       | SCF for 2007-2009 HDDT
# 2008+      |  ----           | SCF based on CNG HDT data
# ------------------------------------------------------------
ggplot() + 
  geom_point(data = dt[year %in% 2008 & fuel %in% "CNG",],
             aes(x = speed,y = scf,color = pollutant),stat = "identity") + 
  facet_grid(rows = vars(pollutant),scales = "free")

#
#
#
pol = "CO"
year_group = 2012
speed = 20
scf(pol = "CO2",speed = 15,year_group = 2010)
