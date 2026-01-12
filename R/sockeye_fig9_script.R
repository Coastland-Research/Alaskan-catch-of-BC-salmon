# SEAK sockeye figure 9 - Canada (red), US (blue), and Total (black) marine commercial catch 
# in Southeast Alaska and DFO areas 3/4/5 for Skeena and Nass sockeye. 1982-2021
library(tidyverse)

#### SKEENA NASS PLOT TOTAL US AND CDN ERs and proportion ####
totsn<-read.csv("data-raw/SX Skeena Nass ERs US CDN.csv", check.names = FALSE)

totsn$USer=totsn$US.Catch/totsn$Total.Return
totsn$CDNer=totsn$CDN.Catch/totsn$Total.Return
totsn$Total.Catch=totsn$US.Catch+totsn$CDN.Catch
totsn$USp=totsn$US.Catch/totsn$Total.Catch
totsn$CDNp=totsn$CDN.Catch/totsn$Total.Catch

tot.catch<-totsn%>%select(Area,Year,US.Catch,CDN.Catch)%>%
  pivot_longer(3:4,names_to="Fishery",values_to="Catch")

#Skeena region
tot.catch.skeena <- tot.catch %>%
  filter(Area == "Skeena")
  
sk_us_cdn <- ggplot(tot.catch.skeena,aes(x=Year,y=Catch/1000,color=Fishery))+
  geom_point(alpha=.5)+geom_line(alpha=.5)+
  theme_bw()+
  scale_colour_manual(values=c("red","blue"))+
  # facet_wrap(~Area,ncol=1,scales="free_y")+
  labs(x="Year",y="Catch (Thousands)",
       title=paste0("US and CDN Total Catch of Skeena sockeye"))+
  theme(legend.position ="right")

sk_us_cdn

#Nass region
tot.catch.nass <- tot.catch %>%
  filter(Area == "Nass")

nass_us_cdn <- ggplot(tot.catch.nass,aes(x=Year,y=Catch/1000,color=Fishery))+
  geom_point(alpha=.5)+geom_line(alpha=.5)+
  theme_bw()+
  scale_colour_manual(values=c("red","blue"))+
  # facet_wrap(~Area,ncol=1,scales="free_y")+
  labs(x="Year",y="Catch (Thousands)",
       title=paste0("US and CDN Total Catch of Nass sockeye"))+
  theme(legend.position ="right")

nass_us_cdn


