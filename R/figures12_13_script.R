# Script for Figures 12 and 13
# figure 12: weekly catch of Pink salmon in district 104 seine fishery 
# figure 13: SEAK pink and sockeye catch in district 104 sub-areas by statistical week

library(data.table)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(dplyr)
library(tidyr)

harvests<-fread("data-raw/SEAK D101-D106 harvest by district by week.csv")

harvests %>%
  mutate(district = as.factor(`District Number`)) %>%
  mutate(`Stat Week` = as.factor(`Stat Week`)) %>%
  filter(`Number Of Fish (estimated)` != "Confidential") %>%
  mutate(`Number Of Fish (estimated)` = as.numeric(`Number Of Fish (estimated)`)) %>%
  ggplot(aes(x = `Stat Week`, y = `Number Of Fish (estimated)`)) +
  geom_boxplot() +
  facet_wrap(~`District Number`)

# need to figure out what's wrong with the data grouping - creates scatterplot instead
# of boxplots



  