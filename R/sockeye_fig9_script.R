# SEAK sockeye figure 9 - Canada (red), US (blue), and Total (black) marine commercial catch 
# in Southeast Alaska and DFO areas 3/4/5 for Skeena and Nass sockeye. 1982-2021
library(tidyverse)

# Skeena Sockeye
skeena <- read.csv("data-raw/sockeye-skeena-trtc data.csv", check.names = FALSE)

skeena$USharvest = skeena$`Total Harvest`-skeena$`CDN Harvest`
skeena$USer = skeena$USharvest/skeena$`Total Run`
skeena$CDNer = skeena$`CDN Harvest`/skeena$`Total Run`
skeena$Totalharvest = skeena$USharvest+skeena$`CDN Harvest`
skeena$USp = skeena$USharvest/skeena$`Total Harvest`
skeena$CDNp = skeena$`CDN Harvest`/skeena$`Total Harvest`
  
skeena_catch <- skeena %>%
  select(StatArea, Year, USharvest, `CDN Harvest`) %>%
  pivot_longer(3:4,names_to="Fishery",values_to="Catch")

skeena_catch_sum <- skeena_catch %>%
  group_by(Year, Fishery) %>%
  summarise(Catch = sum(Catch, na.rm = TRUE), .groups = "drop") %>%
  filter(Year >= 1960)

skeena_us_cdn <- ggplot(
  skeena_catch_sum,
  aes(x = Year, y = Catch / 1000, color = Fishery)) +
  scale_x_continuous(breaks = seq(1960, 2030, by = 10))+
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  labs(x="Year",y="Catch (Thousands)",
       title=paste0("US and CDN Total Catch of Skeena sockeye"))+
  scale_colour_manual(values=c("red","blue")) 
  
skeena_us_cdn

# code for old dataset:
# totsn<-read.csv("data-raw/SX Skeena Nass ERs US CDN.csv", check.names = FALSE)

# sk_us_cdn <- ggplot(skeena_catch,aes(x=Year,y=Catch/1000,color=Fishery))+
#   geom_point(alpha=.5)+geom_line(alpha=.5)+
#   theme_bw()+
#   scale_colour_manual(values=c("red","blue"))+
#   # facet_wrap(~Area,ncol=1,scales="free_y")+
#   labs(x="Year",y="Catch (Thousands)",
#        title=paste0("US and CDN Total Catch of Skeena sockeye"))+
#   theme(legend.position ="right")

# totsn$USer=totsn$US.Catch/totsn$Total.Return
# totsn$CDNer=totsn$CDN.Catch/totsn$Total.Return
# totsn$Total.Catch=totsn$US.Catch+totsn$CDN.Catch
# totsn$USp=totsn$US.Catch/totsn$Total.Catch
# totsn$CDNp=totsn$CDN.Catch/totsn$Total.Catch

# tot.catch<-totsn%>%select(Area,Year,US.Catch,CDN.Catch)%>%
#   pivot_longer(3:4,names_to="Fishery",values_to="Catch")
# tot.catch.skeena <- tot.catch %>%
#   filter(Area == "Skeena")
  
# sk_us_cdn <- ggplot(tot.catch.skeena,aes(x=Year,y=Catch/1000,color=Fishery))+
#   geom_point(alpha=.5)+geom_line(alpha=.5)+
#   theme_bw()+
#   scale_colour_manual(values=c("red","blue"))+
#   # facet_wrap(~Area,ncol=1,scales="free_y")+
#   labs(x="Year",y="Catch (Thousands)",
#        title=paste0("US and CDN Total Catch of Skeena sockeye"))+
#   theme(legend.position ="right")

#Nass region

nass <- read.csv("data-raw/sockeye-nass-trtc data.csv", check.names = FALSE)
nass$USharvest = nass$`Total Harvest`-nass$`CDN Harvest`
nass$USer = nass$USharvest/nass$`Total Run`
nass$CDNer = nass$`CDN Harvest`/nass$`Total Run`
nass$Totalharvest = nass$USharvest+nass$`CDN Harvest`
nass$USp = nass$USharvest/nass$`Total Harvest`
nass$CDNp = nass$`CDN Harvest`/nass$`Total Harvest`

nass_catch <- nass %>%
  select(StatArea, Year, USharvest, `CDN Harvest`) %>%
  pivot_longer(3:4,names_to="Fishery",values_to="Catch")

nass_catch_sum <- nass_catch %>%
  group_by(Year, Fishery) %>%
  summarise(Catch = sum(Catch, na.rm = TRUE), .groups = "drop")

nass_us_cdn <- ggplot(
  nass_catch_sum,
  aes(x = Year, y = Catch / 1000, color = Fishery)) +
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  labs(x="Year",y="Catch (Thousands)",
               title=paste0("US and CDN Total Catch of Nass sockeye"))+
  scale_colour_manual(values=c("red","blue"))

nass_us_cdn

# tot.catch.nass <- tot.catch %>%
#   filter(Area == "Nass")

# nass_us_cdn <- ggplot(nass_catch,aes(x=Year,y=Catch/1000,color=Fishery, group = Fishery))+
#   geom_point(alpha=.5)+geom_line(alpha=.5)+
#   theme_bw()+
#   scale_colour_manual(values=c("red","blue"))+
#   # facet_wrap(~Area,ncol=1,scales="free_y")+
#   labs(x="Year",y="Catch (Thousands)",
#        title=paste0("US and CDN Total Catch of Nass sockeye"))+
#   theme(legend.position ="right")

