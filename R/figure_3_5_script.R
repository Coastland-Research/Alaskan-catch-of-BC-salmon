library(tidyverse)
library(gridExtra)

# figure 3 SEAK harvest by species
catch <- read.csv("data-raw/Alaska Statewide Salmon Landings by Area and Species 1985-present Fish Tickets.csv") %>%
  mutate(Species = factor(`Species.Name`))

fig3 <- catch %>%
  ggplot(aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000), fill = Species)) +
  geom_col() +
  theme_minimal() +
  ylab("Number of fish (millions)") +
  theme(legend.position = "bottom")

ggsave("fig3.png")
# figure 5 - SEAK harvest and value broken down by species over years

value <- read.csv("data-raw/ADFG AK Harvest and Vaue 1980 2022.csv") %>%
  mutate(Species = factor(`Species.Code`),
         Value = `Estimated.Exvessel.Value..Nominal.`)

fig5_val <- value %>%
  ggplot(aes(x = Year, y = (Value/1000000), fill = Species)) +
  geom_col() +
  facet_wrap(~Species, nrow = 5) +
  theme_minimal() +
  ylab("US Dollars (millions)") +
  theme(legend.position = "")

fig5_harv <- catch %>%
  ggplot(aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000), fill = Species)) +
  geom_col() +
  facet_wrap(~Species, nrow = 5) +
  theme_minimal() +
  ylab("Number of fish (millions)") +
  theme(legend.position = "")

grid.arrange(fig5_harv, fig5_val, nrow = 1)
fig5 <- arrangeGrob(fig5_harv, fig5_val, nrow = 1)


ggsave("fig5.png", fig5)
