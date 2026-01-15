# script for figure 34 - Canadian, SEAK, and Washington catch of Fraser sockeye
library(tidyverse)
library(ggpubr)

data<-read.csv("data-raw/CatchByRegion2023.csv") 

fraser.year <- data %>%
  filter(Year<2023) %>%
  group_by(Year) %>%
  summarise(
    TotalRunSize    = sum(TotalRunSize, na.rm = TRUE),
    Alaskan.Catch   = sum(Alaskan.Catch, na.rm = TRUE),
    Canadian.Catch  = sum(Canadian.Catch, na.rm = TRUE),
    Washington.Catch= sum(Washington.Catch, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(total.catch = Alaskan.Catch + Canadian.Catch + Washington.Catch,
         ak.er   = Alaskan.Catch   / TotalRunSize,
         cdn.er  = Canadian.Catch  / TotalRunSize,
         wash.er = Washington.Catch/ TotalRunSize,
         total.er = ak.er + cdn.er + wash.er,
         ak.p   = ak.er   / total.er,
         cdn.p  = cdn.er  / total.er,
         wash.p = wash.er / total.er)

fraser.catch <- fraser.year %>%
  select(Year, Alaskan.Catch, Canadian.Catch, Washington.Catch) %>%
  pivot_longer(-Year, names_to = "Region", values_to = "Catch")

p19 <- ggplot(fraser.catch, aes(x = Year, y = Catch/10^6, color = Region)) +
  geom_point() + geom_line() +
  labs(y = "Catch (millions)") +
  scale_color_brewer(palette = "Set1",
                     labels = c("Alaska","Canada","Washington")) +
  theme_bw() +
  theme(axis.title.x = element_blank())

fraser.ers <- fraser.year %>%
  select(Year, ak.er, cdn.er, wash.er) %>%
  pivot_longer(-Year, names_to = "Region", values_to = "ER")

p20 <- ggplot(fraser.ers, aes(x = Year, y = ER, color = Region)) +
  geom_point() + geom_line() +
  scale_color_brewer(palette = "Set1",
                     labels = c("Alaska","Canada","Washington")) +
  labs(y = "Exploitation Rate") +
  theme_bw() +
  theme(axis.title.x = element_blank())

fraser.p <- fraser.year %>%
  select(Year, ak.p, cdn.p, wash.p) %>%
  pivot_longer(-Year, names_to = "Region", values_to = "p")

p21 <- ggplot(fraser.p, aes(x = Year, y = p*100, color = Region)) +
  geom_point() + geom_line() +
  scale_color_brewer(palette = "Set1",
                     labels = c("Alaska","Canada","Washington")) +
  theme_bw() +
  labs(y = "Percent of Catch") +
  theme(legend.position = "bottom")

ggarrange(p19,p20,p21,ncol=1,align="v",common.legend=TRUE,legend="right")
