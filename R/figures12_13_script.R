# Script for Figures 12 and 13
# figure 12: weekly catch of Pink salmon in district 104 seine fishery 
# figure 13: SEAK pink and sockeye catch in district 104 sub-areas by statistical week
harvests<-fread("data-raw/SEAK D101-D106 harvest by district by week.csv")
view(harvests)
names(harvests)
harvests %>%
  ggplot(aes(x = `Stat Week`, y = `Number of Fish (estimated)`, fill = `District Number`)) +
  