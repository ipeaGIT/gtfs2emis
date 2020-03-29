#
# temporal plot
#
rm(list=ls())
library(ggplot2); library(patchwork)
line <- readr::read_rds("../../data/linestring_emi/cur/cur.rds")
line$departure_time1 <- stringr::str_sub(line$departure_time,1,2)
#
# plot - basic
#
df <- data.table::as.data.table(line)[,EM_CO := sum(EM_CO)/1000,by = departure_time1][,.SD[1],by = departure_time1]
pf <- ggplot(data = df,aes(x = departure_time1, y = as.numeric(EM_CO), fill = as.numeric(EM_CO))) + 
  geom_bar(stat = "identity",size=.3, alpha=1) + 
  viridis::scale_fill_viridis(discrete = F,option = "A",
                              direction = -1,
                              guide = guide_colourbar(barheight = 10,
                                                      frame.colour = "black")) + 
  labs(title = "Total emissions") + 
  xlab("Hour") + ylab("CO emissions [kg]") + 
  theme(legend.position = "none")
# ggsave(plot = pf,filename = paste0("figures/CO_total_hour.jpg"),
#        width = 9.5,height = 5,units = "cm",scale = 1.5,dpi = 300)
#
# plot - tipo veiculo
#
df1 <- data.table::as.data.table(line)[,EM_CO := sum(EM_CO)/1000,
                          by = .(departure_time1,tipo_de_veiculo)][,.SD[1],
                                                                by = .(departure_time1,tipo_de_veiculo)]
pf1 <- ggplot(data = df1,aes(x = departure_time1, y = as.numeric(EM_CO), fill = tipo_de_veiculo)) + 
  geom_bar(stat = "identity",size=.3, alpha=1) + 
  labs(fill = "Bus type") + 
  xlab("Hour") + ylab("CO emissions [kg]") + 
  guides(fill=guide_legend(ncol=2)) 
# pf1
#  ggsave(plot = pf1,filename = paste0("figures/CO_total_veic.jpg"),
#        width = 10.5,height = 5,units = "cm",scale = 2.2,dpi = 300)
 #
 # plot - tipo category
 #
 df2 <- data.table::as.data.table(line)[,EM_CO := sum(EM_CO)/1000,
                            by = .(departure_time1,categoria)][,.SD[1],
                                                                     by = .(departure_time1,categoria)]
 pf2 <- ggplot(data = df2,aes(x = departure_time1, y = as.numeric(EM_CO), fill = categoria)) + 
   geom_bar(stat = "identity",size=.3, alpha=1) + 
   labs(fill = "Line category") + 
   xlab("Hour") + ylab("CO emissions [kg]") 
# pf2
 # ggsave(plot = pf2,filename = paste0("figures/CO_total_linha.jpg"),
 #        width = 10.5,height = 5,units = "cm",scale = 2.2,dpi = 300)
 #
 # plot - age
 #
 df3 <- data.table::as.data.table(line)[,EM_CO := sum(EM_CO)/1000,
                            by = .(tipo_de_veiculo,frota_ano)][,.SD[1],
                                                               by = .(tipo_de_veiculo,frota_ano)]
 df3 <- df3[order(frota_ano,decreasing = FALSE),][,frota_ano := as.character(frota_ano)]
 pf3 <- ggplot(data = df3,aes(x = frota_ano,y = as.numeric(EM_CO), fill = tipo_de_veiculo)) + 
   scale_fill_discrete() + 
   geom_bar(stat = "identity",size=.3, alpha=1) +
   labs(fill = "Bus type") + 
   xlab("Year of fleet") + ylab("CO emissions [kg]") +
   guides(fill=guide_legend(ncol=2))
 
 # ggsave(plot = pf3,filename = paste0("figures/CO_total_fleet_age.jpg"),
 #        width = 10.5,height = 5,units = "cm",scale = 2.2,dpi = 300)
 #
 # plot - age and hour
 #
 df4 <- data.table::as.data.table(line)[,EM_CO := sum(EM_CO)/1000,
                            by = .(departure_time1,frota_ano)][,.SD[1],
                                                               by = .(departure_time1,frota_ano)]
 df4 <- df4[order(frota_ano,decreasing = TRUE),][,frota_ano := as.character(frota_ano)]
 df4$frota_ano <- factor(df4$frota_ano,unique(df4$frota_ano)[order(df4$frota_ano)])
pf4 <-  ggplot(data = df4,aes(x = departure_time1,y = as.numeric(EM_CO), fill = frota_ano)) + 
   geom_bar(stat = "identity",size=.3, alpha=1) + 
   labs(fill = "Fleet age") + 
   xlab("Hour") + ylab("CO emissions [kg]") + 
  guides(fill=guide_legend(ncol=2))
# ggsave(plot = pf4,filename = paste0("figures/CO_total_hour_age.jpg"),
#         width = 10.5,height = 5,units = "cm",scale = 2.2,dpi = 300)
#
 # patchwork
 #
 pp <- (pf / pf1 / pf2) 
 
  ggsave(plot = pp,filename = paste0("figures/CO_hour_patchwork.jpg"),
        width = 9,height = 8,units = "cm",scale = 2.2,dpi = 300) 
  
  # patchwork
  #
  pp1 <- (pf3 / pf4) 
  
  ggsave(plot = pp1,filename = paste0("figures/CO_age_patchwork.jpg"),
         width = 9,height = 7.5,units = "cm",scale = 2.5,dpi = 300) 
