library(tidyverse)

# script for Figures 7-11 - SEAK salmon catch by year and fishery (one figure per species)

# bluesheet <- read.csv("data-raw/Region I - Blue Sheet (1).csv")
bluesheet <- read.csv("data-raw/Alaska Statewide Salmon Landings by Area and Species 1985-present Fish Tickets.csv")

# Chinook Salmon
bluesheet %>%
  filter(Species.Name == "Chinook Salmon") %>%
  ggplot(aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000))) +
  geom_col()+
  ylab("Catch (millions)") +
  facet_wrap(~Area) +
  theme_bw() +
  ggtitle("SEAK Catch of Chinook Salmon by Fishery", 
          subtitle = "\nBlue Sheet Fisheries (1985-2024)") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# Chum Salmon
bluesheet %>%
  filter(Species.Name == "Chum Salmon") %>%
  ggplot(aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000))) +
  geom_col() +
  ylab("Catch (millions)") +
  facet_wrap(~Area) +
  theme_bw() +
  ggtitle("SEAK Catch of Chum Salmon by Fishery", 
          subtitle = "\nBlue Sheet Fisheries (1985-2024)") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# Pink Salmon
bluesheet %>%
  filter(Species.Name == "Pink Salmon") %>%
  ggplot(aes(x = Year, (`Number.Of.Fish..estimated.`/1000000))) +
  geom_col()+
  ylab("Catch (millions)") +
  facet_wrap(~Area) +
  theme_bw() +
  ggtitle("SEAK Catch of Pink Salmon by Fishery", 
          subtitle = "\nBlue Sheet Fisheries (1985-2024)") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# Coho Salmon
bluesheet %>%
  filter(Species.Name == "Coho Salmon") %>%
  ggplot(aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000))) +
  geom_col()+
  ylab("Catch (millions)") +
  facet_wrap(~Area) +
  theme_bw() +
  ggtitle("SEAK Catch of Coho Salmon by Fishery", 
          subtitle = "\nBlue Sheet Fisheries (1985-2024)") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# Sockeye Salmon
bluesheet %>%
  filter(Species.Name == "Sockeye Salmon") %>%
  ggplot(aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000))) +
  geom_col()+
  ylab("Catch (millions)") +
  facet_wrap(~Area) +
  theme_bw() +
  ggtitle("SEAK Catch of Sockeye Salmon by Fishery", 
          subtitle = "\nBlue Sheet Fisheries (1985-2024)") +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5, size = 11))


