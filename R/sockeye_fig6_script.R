# SEAK sockeye catch figure 6 - total catches (all gears) by District by year
library(tidyverse)
# read data: 
sk_harv <- read.csv("data-raw/SEAK D101-D106 harvest by district by week.csv") %>%
  filter(`Species.Name` == "Sockeye Salmon") %>%
  filter(`Number.Of.Fish..estimated.` != "Confidential") %>%
  mutate(`Number.Of.Fish..estimated.` = as.numeric ())

sk_harv %>%
  ggplot(aes(x = Year, y = `Number.Of.Fish..estimated.`)) +
  geom_point()+
  facet_wrap(~`District.Number`)

sk_annual <- sk_harv %>%filter(`District.Number` %in% 101:106) %>%
  group_by(Year, `District.Number`) %>%
  summarise(total_catch = sum(`Number.Of.Fish..estimated.`, na.rm = TRUE),
            .groups = "drop")
  

ggplot(sk_annual,
       aes(x = Year, y = total_catch)) +
  
  # yearly values
  geom_point(size = 1.5) +
  geom_line() +
  geom_smooth(method = "loess", se = TRUE) +
  facet_wrap(~ `District.Number`, ncol = 2) +
  labs(
    title = "SSEAK Catch All Gear by District (101–106)",
    subtitle = "Sockeye Salmon (1985–2021)",
    x = "Year",
    y = "Total Catch"
  ) +
  
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90")
  )
