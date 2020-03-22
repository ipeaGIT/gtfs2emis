# ---
#
# emission factor analysis of HDV
#
# comparison between EMEP/EEA 2019 and EMEP/EEA 2016 (incorpored into VEIN)
#
# ---
rm(list=ls())
library(vein)
library(dplyr)
library(data.table)
library(openxlsx)
source("prep/09_emep-eea_emission-factor.R")
ef <- openxlsx::read.xlsx("../../emission_routes_joao/references/copert/1.A.3.b.i-iv Road transport hot EFs Annex 2018_Dic.xlsx") %>% 
  data.table::as.data.table()
ef <- ef[Category %in% "Buses",]


buses_type_aut <- unique(ef$Segment)[1:3]
buses_type_vein <- c("Midi","Std","RT")
g_vein <- c( "<=15",">15 & <=18",">18")
euro_vein <- c("PRE", "I", "II", "III", "IV", "V")
#euro_aut <- c("Conventional","Euro I","Euro II","Euro III","Euro IV","Euro V")
# tech_aut <- c(rep(NA,4),"SCR","SCR")
euro_aut <- c("Euro I","Euro II","Euro III","Euro IV","Euro V")
tech_aut <- c(rep(NA,3),"SCR","SCR")
pol_aut <- c("CO","NOx","VOC","PM Exhaust")
slope_aut <- c(0.06,0,-0.06)
speed <- 1:100  

ef[Pollutant %in% "CO",][,unique(Technology),by = .(Segment, Euro.Standard)] #[,.SD[1],by = .(Segment,Euro.Standard)]

lapply(1:length(buses_type_aut),function(b){ # b = 1
  df2 <- lapply(1:length(euro_aut),function(e){ # e = 1
    df1 <- lapply(1:length(pol_aut), function(p){ # p = 1
      df0 <- lapply(1:length(slope_aut), function(s){ # s = 1
        df <- data.frame("speed" = as.numeric(speed),
                         "ef" = ef_hdv_speed_2019(vel = speed,ef = ef,
                                                  veh = "Buses",fuel = "Diesel",
                                                  segment = buses_type_aut[b], 
                                                  euro = euro_aut[e],
                                                  tech = tech_aut[e],
                                                  pol = pol_aut[p],
                                                  slope = slope_aut[s],
                                                  load = 0.5),
                         "slope" = as.character(slope_aut[s]),
                         "euro" = euro_aut[e], 
                         "pol" = pol_aut[p])
        return(df)
      }) %>% data.table::rbindlist()
      return(df0)
    }) %>% data.table::rbindlist()
    return(df1)
  }) %>% data.table::rbindlist()
  pp <- ggplot(data = df2,aes(x = speed,y = as.numeric(ef), group = slope)) + 
    geom_line(aes(color = euro,linetype = slope)) +
    scale_linetype_manual(values = c("twodash", "solid","dotted")) +
    facet_grid(cols = vars(euro),rows = vars(pol), scales = "free_y") +
    ylab("FE(v) (g/km)") + xlab("Velocidade (km/h)") + xlim(10,100) +
    scale_fill_discrete(guide = FALSE) + labs(color = buses_type_aut[b], linetype = "Slope (%)")

  # ----
  nmfile <- paste0("figures/ef_",buses_type_vein[b],".jpg")
  ggsave(filename = nmfile,plot = pp,width = 18,height = 8,units = "cm",scale = 1.2,dpi = 300)
  
})
#sapply(1:nrow(buses_type), function(i){
message(buses_type[i])
vein::ef_hdv_speed(v = "Ubus",
                   speed = vein::Speed(86,"km/h"),
                   t = buses_type_vein[i],
                   g = g_vein[i],
                   eu = euro_vein[e],
                   gr = 0,
                   l = 1.0,
                   p = "CO",
                   show.equation = FALSE) 

ef_hdv_speed_2019(vel = 86,
                  ef = ef,
                  veh = "Buses",
                  fuel = "Diesel",
                  segment = buses_type[i],
                  euro = euro_aut[e],
                  tech = "EGR",
                  pol = "CO",
                  slope = 0,
                  load = 0.5)
ef[Category %in% veh & 
     Fuel %in% "Diesel" & 
     Segment %in% buses_type[i] & 
     Euro.Standard %in% euro_aut[e] & 
     Technology %in% "EGR" &
     Pollutant %in% "CO" &
     Road.Slope %in% 0 &
     #Mode %in% mode &
     Load %in% 1.0,`EF.[g/km].or.ECF.[MJ/km]`]

})



lapply(c(1,2,3,4,5,6,7,8,9), function(i){table(ef[,..i],useNA = "always")})
vein::ef_ldv_speed()


