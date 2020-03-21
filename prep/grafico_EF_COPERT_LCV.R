# ++++
#
# emission factor with speed function
# 
# ++++
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/master-thesis-repo1")
library(vein)
library(ggplot2)
rm(list=ls())
# +++
pol <- c("CO2","CO", "HC", "NMHC", "NOx", "NO2" ,"NO")
for(p in (1:length(pol))){
  # p =2
  message(pol[p])
  speed1 <- Speed(1:100,"km/h")
  eu1 <- c("PRE","I","II","III","IV","V")
  dfnew <- data.frame()
  for(e in (1:length(eu1))){
    # e = eu1[1]
    ef <- ef_ldv_speed(v = "LCV",eu =  eu1[e],cc = "<3.5",f = "G",p =  pol[p],show.equation = T,speed = speed1)
    df <- data.frame("speed"=as.numeric(speed1),"ef"=ef,"eu"=eu1[e])
    dfnew <- rbind(dfnew,df)
  }
  
  ggplot(data = dfnew,aes(x=speed,y=as.numeric(ef))) + 
    geom_line(aes(color = eu))+
    facet_grid(eu ~ ., scales = "free_y")+
    ylab("FE (g/km)")+xlab("Velocidade (km/h)")+xlim(10,100)+
    scale_fill_discrete(guide=FALSE)+labs(color="Padrão Euro")
  # ----
  nmfile <- paste0("graficos/mapa/ef/",pol[p],"_LCV.jpg")
  ggsave(nmfile,
         width = 12,height = 15,units = "cm",scale = 1.2,dpi = 300)
}

