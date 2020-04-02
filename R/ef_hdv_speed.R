#' @title Emission factor dependent on speed by EMEP/EEA
#' 
#' @description Estimates emission factor for buses based on EMEP/EEA 
#' 
#' @param vel Units; Speed in units [km/h]
#' @param veh Character; Bus type, classified in "Urban Buses Midi <=15 t", "Urban Buses Standard 15 - 18 t", 
#' "Urban Buses Articulated >18 t", "Coaches Standard <=18 t"       
#' "Coaches Articulated >18 t", "Urban CNG Buses", "Urban Biodiesel Buses"  
#' @param fuel Character; Fuel type, classified in "Diesel", "CNG"  and "Biodiesel"
#' @param euro Character; Euro period of vehicle, classified in "Conventional", "Euro I", "Euro II", "Euro III", 
#' "Euro IV", "Euro V", "Euro VI" and "EEV"  
#' @param tech Character; Technology, classified in "SCR", "EGR" and "DPF+SCR"
#' @param pol Character; Pollutant, classified in "CO", "NOx", "VOC", "PM", "FC", "CH4", "NH3" and "N2O"
#' @param slope Numeric; Slope gradient, classified in -0.06, -0.04, -0.02, 0.00, 0.02, 0.04 and 0.06.
#'  Negative gradients means downhills and positive uphills. Default is 0.0.
#' @param load Numeric; Load ratio, classified in 0.0, 0.5 and 1.0. Default is 0.5.
#' @param show.equation Logical; show.equation from EMEP/EEEA used? Default parameter is TRUE.
#' @param k Numeric; constant value to adjust emission factors. Default is 1.0.
#' @return Emission factors in units g/km
#' @export
ef_hdv_speed <- function(vel,veh,fuel,euro,tech,pol,slope = 0.0,load = 0.5,k = 1,show.equation = TRUE){
  #
  # pre-conditions
  #
  if(euro == "Euro III" | euro == "Euro II"){
    tech = NA
    message("no technology associated with Euro III")
  }
  #
  # veh 
  #
  # [1] "Passenger Cars"            "Light Commercial Vehicles" "Heavy Duty Trucks"        
  # [4] "Buses"                     "L-Category" 
  #
  # fuel
  #
  # [1] "Petrol"              "Diesel"              "Petrol Hybrid"       "LPG Bifuel ~ LPG"   
  # [5] "LPG Bifuel ~ Petrol" "CNG Bifuel ~ CNG"    "CNG Bifuel ~ Petrol" "CNG"                
  # [9] "Biodiesel"
  #
  # segment
  #
  # [1] "Mini"                               "Small"                             
  # [3] "Medium"                             "Large-SUV-Executive"               
  # [5] "2-Stroke"                           "N1-I"                              
  # [7] "N1-II"                              "N1-III"                            
  # [9] ">3,5 t"                             "Rigid <=7,5 t"                     
  # [11] "Rigid 7,5 - 12 t"                   "Rigid 12 - 14 t"                   
  # [13] "Rigid 14 - 20 t"                    "Rigid 20 - 26 t"                   
  # [15] "Rigid 26 - 28 t"                    "Rigid 28 - 32 t"                   
  # [17] "Rigid >32 t"                        "Articulated 14 - 20 t"             
  # [19] "Articulated 20 - 28 t"              "Articulated 28 - 34 t"             
  # [21] "Articulated 34 - 40 t"              "Articulated 40 - 50 t"             
  # [23] "Articulated 50 - 60 t"              "Urban Buses Midi <=15 t"           
  # [25] "Urban Buses Standard 15 - 18 t"     "Urban Buses Articulated >18 t"     
  # [27] "Coaches Standard <=18 t"            "Coaches Articulated >18 t"         
  # [29] "Urban CNG Buses"                    "Urban Biodiesel Buses"             
  # [31] "Mopeds 2-stroke <50 cm³"            "Mopeds 4-stroke <50 cm³"           
  # [33] "Motorcycles 2-stroke >50 cm³"       "Motorcycles 4-stroke <250 cm³"     
  # [35] "Motorcycles 4-stroke 250 - 750 cm³" "Motorcycles 4-stroke >750 cm³"     
  # [37] "Quad & ATVs"                        "Micro-car"   
  #
  #
  # euro
  #
  # [1] "Euro 4"                "Euro 5"                "Euro 6 up to 2016"    
  # [4] "Euro 6 2017-2019"      "Euro 6 2020+"          "PRE ECE"              
  # [7] "ECE 15/00-01"          "ECE 15/02"             "ECE 15/03"            
  # [10] "ECE 15/04"             "Improved Conventional" "Open Loop"            
  # [13] "Euro 1"                "Euro 2"                "Euro 3"               
  # [16] "Conventional"          "Euro 6"                "Euro 6 up to 2017"    
  # [19] "Euro 6 2018-2020"      "Euro 6 2021+"          "Euro I"               
  # [22] "Euro II"               "Euro III"              "Euro IV"              
  # [25] "Euro V"                "Euro VI"               "EEV" 
  #
  # tech
  #
  # [1] "GDI"     "PFI"     "GDI+GPF" NA        "DPF"     "DPF+SCR"
  # [7] "LNT+DPF" "SCR"     "EGR" 
  #
  # pol
  #
  # [1] "CO"         "NOx"        "VOC"       
  # [4] "PM Exhaust" "FC"         "CH4"       
  # [7] "NH3"        "N2O"   
  #
  # mode
  #
  # [1] NA               "Urban Peak"    
  # [3] "Urban Off Peak" "Rural"         
  # [5] "Highway" 
  #
  # road slope
  # 
  # [1]    NA -0.06 -0.04 -0.02  0.00  0.02  0.04  0.06
  #
  # load
  #
  # [1]  NA 0.0 0.5 1.0
  #
  # expression
  #
  eq_num <- function(a,b,g,d,e,z,h,rf,v,k){
    eq <- (a * v^2 + b * v + g + d/v) / (e * v^2 + z * v + h) * (1 - rf) * k
    return(eq)
  }
  #
  # ef
  #
  ef1 <- ef[Fuel %in% fuel & 
              Segment %in% veh & 
              Euro.Standard %in% euro & 
              Technology %in% tech &
              Pollutant %in% pol &
              #Mode %in% mode &
              Road.Slope %in% slope &
              Load %in% load,]
  
  if(nrow(ef1) == 0){return(message("No ef"))}
  if(nrow(ef1) > 1){ef2 <- ef1[1,];message("More than one")}
  if(nrow(ef1) == 1){ef2 <- ef1}
  #
  # show.equation
  #
  if (show.equation == TRUE) {
    cat(paste0("a = ", ef2$Alpha, ", b = ", ef2$Beta, ", g = ", 
               ef2$Gamma, ", d = ", ef2$Delta, ", e = ", ef2$Epsilon, ", rf = ", 
               ef2$`Reduction.Factor.[%]`, ", z = ", ef2$Zita, ", h = ", ef2$Hta, "\n"))
  }
  #
  # fix speed
  #
  if(length(which(vel < ef2$`Min.Speed.[km/h]`)) > 0) vel[vel < ef2$`Min.Speed.[km/h]`] <- ef2$`Min.Speed.[km/h]`
  if(length(which(vel > ef2$`Max.Speed.[km/h]`)) > 0) vel[vel > ef2$`Max.Speed.[km/h]`] <- ef2$`Max.Speed.[km/h]`
  #
  # return
  #
  eq_output <- eq_num(a = ef2$Alpha,b = ef2$Beta,g = ef2$Gamma,d = ef2$Delta,
                      e = ef2$Epsilon,z = ef2$Zita,h = ef2$Hta,
                      rf = ef2$`Reduction.Factor.[%]`, v = vel, k = k)
  eq_output <- units::set_units(eq_output,g/km)
  
  return(eq_output)
}
