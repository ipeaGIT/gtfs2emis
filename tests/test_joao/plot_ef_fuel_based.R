
veh_type <- c("BUS_MICRO_D",
              "BUS_URBAN_D")
pol <- c("CO","PM10","CH4","NOx")

library(magrittr)
library(data.table)
ef_bind <- lapply(seq_along(pol),function(i){
  tmp_ef <- lapply(seq_along(veh_type),function(j){
    my_ef <- ef_brazil_cetesb_fb(pollutant = pol[i]
                        ,veh_type = veh_type[j]
                        ,model_year = 2005:2016
                        ,fuel_based = TRUE
                        ,as_list = TRUE)
    my_ef <- emis_to_dt(emi_list = my_ef
                        ,emi_vars = "EF"
                        ,veh_vars = c("veh_type","model_year")
                        )
    return(my_ef)
    }) %>% data.table::rbindlist()
  return(tmp_ef)
})%>% data.table::rbindlist()

library(ggplot2)

ggplot(ef_bind)+
  geom_point(aes(x = model_year, y = 1000 * as.numeric(EF), color = veh_type))+
  facet_wrap(~pollutant,scales = "free",nrow = 3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "model year", y = "mg pollutant / g diesel"
       ,color = "vehicle type")
