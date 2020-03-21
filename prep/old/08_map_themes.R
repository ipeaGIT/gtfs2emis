theme_for_TMI <- function(base_size) {
  
  theme_void(base_family="Roboto Condensed") %+replace%
    
    theme(
      legend.position = "right",
      plot.margin=unit(c(2,0,0,0),"mm"),
      legend.key.size = unit(5,"line"),
      legend.key.width = unit(1,"line"),
      legend.key.height = unit(0.35,"cm"),
      legend.spacing.y = unit(0.5, "cm"),
      legend.text=element_text(size=rel(0.85)),
      legend.title=element_text(size=rel(1)),
      plot.title = element_text(hjust = 0, vjust = 0),
      strip.text = element_text(size = 6),
      #legend.spacing.y = unit(2.0, "cm"),
      # legend.key.height=unit(0.5,"cm")
      
    )
}  