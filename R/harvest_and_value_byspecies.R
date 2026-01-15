library(tidyverse)
library(gridExtra)

# # figure 3 SEAK harvest by species
# catch <- read.csv("data-raw/Alaska Statewide Salmon Landings by Area and Species 1985-present Fish Tickets.csv") %>%
#   mutate(Species = factor(`Species.Name`))
# 
# value <- read.csv("data-raw/ADFG AK Harvest and Vaue 1980 2022.csv") %>%
#   mutate(Species = factor(`Species.Code`),
#          Value = `Estimated.Exvessel.Value..Nominal.`) %>%
#   filter(`Salmon.Area.Name` != "Bristol Bay")

make.harvest.byspecies<-function(catch){
  
  ggplot(catch, aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000), fill = Species)) +
  geom_col() +
  theme_minimal() +
  ylab("Number of fish (millions)") +
  theme(legend.position = "bottom")
}

# ggsave("fig3.png", fig3)

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
    ylab("US Dollars (millions)") +
    theme(legend.position = "none")
  
  fig5_harv <- ggplot(catch, aes(x = Year, y = `Number.Of.Fish..estimated.` / 1e6, fill = Species)) +
    geom_col() +
    facet_wrap(~Species, nrow = 5, scales = "free_y") +
    theme_minimal() +
    ylab("Number of fish (millions)") +
    theme(legend.position = "none")
  
  gridExtra::grid.arrange(fig5_harv, fig5_val, nrow = 1)
}
#make.harvest.value.byspecies(value, catch)
