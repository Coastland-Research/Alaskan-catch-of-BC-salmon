library(tidyverse)
library(readxl)
library(data.table)

data<-read_excel("data/TRTC-Results--Skeena-Sockeye-2023.xlsx",sheet="ConservationUnit TRTC")

dat<-data%>%filter(CU_Name=="Babine-Late-Wild")%>%
  mutate(AK=`Total ER`-`CDN ER`)


ers<-dat%>%select(Year,CDN=`CDN ER`,AK)%>%
  pivot_longer(2:3,names_to="Region",values_to="ER")

ggplot(ers,aes(x=Year,y=ER,color=Region))+
  geom_point()+geom_line()+
  scale_color_brewer(palette="Set1")+
  facet_wrap(~Region,ncol=1)+
  theme(legend.position="bottom")

ggsave("figures/late babine cdn and ak ers.png",dpi=300,height=4,width=5,units="in")

er.p<-ers%>%group_by(Year)%>%
  mutate(p=ER/sum(ER))

ggplot(er.p,aes(x=Year,y=p,fill=Region))+
  geom_col(color="black")+
  scale_fill_brewer(palette="Set1")+
theme(legend.position="bottom")

ggsave("figures/late babine cdn and ak pers.png",dpi=300,height=4,width=5,units="in")

ggplot(er.p,aes(x=Year,y=p,color=Region))+
  geom_point()+geom_line()+
  scale_color_brewer(palette="Set1")+
  facet_wrap(~Region,ncol=1)+
  theme(legend.position="bottom")

ggsave("figures/late babine cdn and ak p ers lines.png",dpi=300,height=4,width=5,units="in")



data<-fread("data/sockeye--all-statarea.csv")

er<-data%>%filter(StatArea==4)%>%
  mutate(AK=`Total ER`-`CDN ER`)


ggplot(er,aes(x=Year,y=AK))+
  geom_col()+
  labs(y="Alaska Exploitation Rate")

ggsave("figures/skeena alaska er to 2017.png",dpi=300,height=4,width=5,units="in")

