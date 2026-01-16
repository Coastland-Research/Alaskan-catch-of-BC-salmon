library(tidyverse)
library(gridExtra)

# # figure 3 SEAK harvest by species

make.harvest.byspecies<-function(catch){
  
  ggplot(catch, aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000), fill = Species)) +
  geom_col() +
  theme_minimal() +
    scale_fill_brewer(palette = "Spectral")+
  ylab("Number of fish (millions)") +
  theme(legend.position = "bottom")
}

# figure 5 - SEAK harvest and value broken down by species over years

# make.harvest.value.byspecies<-function(data){
#   
#   fig5_val <- ggplot(value, aes(x = Year, y = (Value/1000000), fill = Species)) +
#   geom_col() +
#   facet_wrap(~Species, nrow = 5) +
#   theme_minimal() +
#   ylab("US Dollars (millions)") +
#   theme(legend.position = "")
#   
#   fig5_harv <- ggplot(catch, aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000), fill = Species)) +
#   geom_col() +
#   facet_wrap(~Species, nrow = 5) +
#   theme_minimal() +
#   ylab("Number of fish (millions)") +
#   theme(legend.position = "")
#   
#   grid.arrange(fig5_harv, fig5_val, nrow = 1)
#   # arrangeGrob(fig5_harv, fig5_val, nrow = 1)
# }

make.harvest.value.byspecies <- function(value, catch){
  
  fig5_val <- ggplot(value, aes(x = Year, y = Value / 1e6, fill = Species)) +
    geom_col() +
    facet_wrap(~Species, nrow = 5, scales = "free_y") +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral")+
    ylab("US Dollars (millions)") +
    theme(legend.position = "none")
  
  fig5_harv <- ggplot(catch, aes(x = Year, y = `Number.Of.Fish..estimated.` / 1e6, fill = Species)) +
    geom_col() +
    facet_wrap(~Species, nrow = 5, scales = "free_y") +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral")+
    ylab("Number of fish (millions)") +
    theme(legend.position = "none")
  
  gridExtra::grid.arrange(fig5_harv, fig5_val, nrow = 1)
}
