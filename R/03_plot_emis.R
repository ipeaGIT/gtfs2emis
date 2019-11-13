#
# plot analysis 
#
rm(list=ls())
library(vein)
library(stringr)
library(data.table)
library(cptcity)
library(sf)
library(units)
library(mapview)
library(ggplot2)
library(plyr)
library(geobr)
library(tibble)
library(future.apply)
library(ggalt)
library(hrbrthemes)
library(ggnewscale)
# setwd
setwd("L:/# DIRUR #/ASMEQ/bosistas/joaobazzo/gps2emission/")
# --
# data import
# --
filepath <- "data/emi_speed_grid/"
ids <- list.files(path=filepath,pattern = ".shp");ids_saida <- str_remove(ids,".shp")
break()
# --
# rbinding
# --
system.time({
dd <- future.apply::future_lapply(1:length(ids),function(i){
  sf::read_sf(paste0("data/emi_speed_grid/",ids[i]))%>% st_transform(31983)
}) %>% data.table::rbindlist() 
})
# sum of emission =s
dd <- dd[,emi:=sum(emi),by=h3_ddrs][,.SD[1],by=h3_ddrs] %>% st_as_sf()
# --
# plot
# --
ggplot(dd)+
  geom_sf(aes(fill=emi/1000), color = NA, alpha=.7)+
  viridis::scale_fill_viridis( direction = -1)+
  labs(fill = "CO (kg/dia)",
       title = "Emissões de CO da frota \n de ônibus de São Paulo")

ggsave(file= sprintf("./maps/sptrans/hex_emi.jpg"), 
       dpi = 300, width = 14, height = 10, units = "cm")
# ---
# plot 1
# --
ggplot(dd,aes(x="",y=emi))+
  geom_boxplot()+ylab("Emissões por hexágono (g/dia)")+
  xlab("")#+
  geom_jitter(shape=16, position=position_jitter(0.1))
  

plot1 <- ggplot() + 
  geom_raster(data = map_tiles, aes(x, y, fill = hex), alpha = 0.5) +
  coord_equal() +
  scale_fill_identity()+
  # nova escala
  new_scale_fill() +
  geom_sf(dat = st_transform(acess_pt_pico, 3857), aes(fill = valor), color = NA, alpha=.7)  +
  viridis::scale_fill_viridis( direction = -1
                               # , breaks = c(0, 10, 20, 30, 40)
                               # , labels = c(0, 10, 20, 30, "+40 min")
  ) +
  labs(fill = "Tempo até a oportunidade\n mais próxima",
       title = munis_df[abrev_muni == sigla_muni]$name_muni)+
  facet_wrap(~ind, ncol = cols)+
  theme_for_TMI()+
  theme(plot.title = element_text(hjust = 0.5))


# save map
ggsave(plot1, 
       file= sprintf("../figures/td_todas/fig1-%s_TMI_SM_TP.png", sigla_muni), 
       dpi = 300, width = 14, height = 10, units = "cm")
#
sum(dd$emi)
sync(mapview(dd["emi"]),mapview(dd$geometry,alpha.regions = 0))
#d1 <- dd[dd$h3_ddrs %in% unique(dd$h3_ddrs),]
dd$emi1 %>% sum()
mapview(dd$geometry[1],color="red",alpha.regions = 0)
mapview(dd["h3_rslt"])
plot(dd["geometry"])
# intersect

lapply(1:10, function(i){sqrt(i)})
