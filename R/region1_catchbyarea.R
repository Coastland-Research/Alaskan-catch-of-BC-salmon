library(tidyverse)

# script for Figures 7-11 - SEAK salmon catch by year and fishery (one figure per species)

# bluesheet <- read.csv("data-raw/Region I - Blue Sheet (1).csv")
bluesheet <- read.csv("data-raw/Alaska Statewide Salmon Landings by Area and Species 1985-present Fish Tickets.csv")

bluesheet %>%
  ggplot(aes(x = Year, y = `Number.Of.Fish..estimated.`)) +
  geom_col()+
  facet_grid(~Area) +
  theme_minimal()


bluesheet$Number.Of.Fish..estimated.