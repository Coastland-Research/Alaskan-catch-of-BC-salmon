# script for figure 34 - Canadian, SEAK, and Washington catch of Fraser sockeye
library(tidyverse)
library(ggpubr)

data<-read.csv("data-raw/CatchByRegion2023.csv") %>%
  mutate(`Total.Catch` = `Alaskan.Catch`+`Canadian.Catch`+`Washington.Catch`,
         `Prop.SEAK` = `Alaskan.Catch`/`Total.Catch`,
         `Prop.US` = (`Alaskan.Catch`+`Washington.Catch`)/`Total.Catch`,
         `Alaska.ER` = `Alaskan.Catch`/TotalRunSize)
view(data)

fraser.sx<-data%>%filter(Year<2023)%>%
  mutate(total.catch=rowSums(.[,4:6],na.rm=TRUE),
         ak.er=`Alaskan.Catch`/`TotalRunSize`,
         cdn.er=`Canadian.Catch`/`TotalRunSize`,
         wash.er=`Washington.Catch`/`TotalRunSize`)%>%
  mutate(total.er=rowSums(.[,8:10],na.rm=TRUE),
         ak.p=ak.er/total.er,
         wash.p=wash.er/total.er,
         cdn.p=cdn.er/total.er)

fraser.catch<-fraser.sx%>%select(Year,GroupName, `Alaskan.Catch`,`Canadian.Catch`,`Washington.Catch`)%>%
  pivot_longer(3:5,names_to="Region",values_to="Catch")

p19<-ggplot(fraser.catch,aes(x=Year,y=Catch/10^6,color=Region))+
  geom_point()+geom_line()+
  labs(y="Catch (millions)")+
  scale_color_brewer(palette="Set1",labels=c("Alaska","Canada","Washington"))+
  theme_bw()+
  facet_wrap(~GroupName)+
  theme(axis.title.x = element_blank())

p19

fraser.ers<-fraser.sx%>%select(Year,GroupName,ak.er,cdn.er,wash.er)%>%
  pivot_longer(3:5,names_to="Region",values_to="ER")

p20<-ggplot(fraser.ers,aes(x=Year,y=ER,color=Region))+
  geom_point()+geom_line()+
  scale_color_brewer(palette="Set1",labels=c("Alaska","Canada","Washington"))+
  labs(y="Exploitation Rate")+
  theme_bw()+
  facet_wrap(~GroupName)+
  theme(axis.title.x = element_blank())

p20

fraser.p<-fraser.sx%>%select(Year, GroupName, ak.p,cdn.p,wash.p)%>%
  pivot_longer(3:5,names_to="Region",values_to="p")

p21<-ggplot(fraser.p,aes(x=Year,y=p*100,color=Region))+
  geom_point()+geom_line()+
  scale_color_brewer(palette="Set1",labels=c("Alaska","Canada","Washington"))+
  theme_bw()+
  facet_wrap(~GroupName)+
  labs(y="Percent of Catch")+
  theme(legend.position="bottom")

p21

# ggarrange(p19,p20,p21,ncol=1,align="v",common.legend=TRUE,legend="right")