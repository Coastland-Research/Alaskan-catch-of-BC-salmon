# script for SEAK sockeye report figure 15 - proportion of catch before Week 31
library(tidyverse)

#### PRE Week 31 versus total D104 sockeye catch ####
pre.sn<-read.csv("data-raw/D104 pre 31 versus total.csv", check.names = FALSE)  

pre.sn.long<-pre.sn%>%select(Year,Skeena=Sk.pre.p,Nass=Nass.pre.p)%>%
  pivot_longer(2:3,names_to="Area",values_to="p")

pre.skeena <- pre.sn.long %>%
  filter(Area == "Skeena")

pre.nass <- pre.sn.long %>%
  filter(Area == "Nass")

skeena_pre_31 <- ggplot(pre.skeena,aes(y=p,x=Year))+
  geom_bar(stat="identity",color="black", fill = "steelblue")+
  #geom_point(alpha=.5)+geom_line(alpha=.5)+
  theme_bw()+
  # facet_wrap(~Area,ncol=1)+
  labs(x="Year",y="Proportion of Total D104\nSockeye Catch",
       title=paste0("D104 Proportion of Catch before Week 31 - Skeena"))+
  # scale_fill_manual(breaks=c("Nass","Skeena"),values=c("firebrick","steelblue"))+
  theme(legend.position ="right")

nass_pre_31 <- ggplot(pre.nass,aes(y=p,x=Year))+
  geom_bar(stat="identity",color="black", fill = "firebrick")+
  #geom_point(alpha=.5)+geom_line(alpha=.5)+
  theme_bw()+
  # facet_wrap(~Area,ncol=1)+
  labs(x="Year",y="Proportion of Total D104\nSockeye Catch",
       title=paste0("D104 Proportion of Catch before Week 31 - Nass"))+
  # scale_fill_manual(breaks=c("Nass","Skeena"),values=c("firebrick","steelblue"))+
  theme(legend.position ="right")


# ggsave("SX AK fisheries D104 p pre week 31 skeena and nass.png",dpi=600,height=7,width=6)
