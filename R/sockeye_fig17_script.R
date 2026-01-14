# Figure 17 - exploitation rates for Skeena and Nass sockeye by SEAK sub-area

library(tidyverse)

sn<-read.csv("data-raw/skeena nass ak fishery ers.csv",header=TRUE,
             check.names = FALSE)

sn.box<-sn%>%filter(Area%in%c("Skeena","Nass"))%>%
  pivot_longer("1982":"2020",names_to="year",values_to="er")


ggplot(sn.box,aes(y=reorder(Fishery,er,FUN=median,na.rm=TRUE),x=er,fill=Area))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_manual(values=c("seagreen3","royalblue3"))+
  # scale_fill_brewer(palette="Paired")+
  labs(x="Exploitation Rate",y="SEAK Fishing Area",fill="Stock Origin",
       title="SEAK Catch of Sockeye by Fishing Area",
       subtitle="Skeena and Nass sockeye:1982-2020")+
  theme(legend.position ="right")

#ggsave("SX AK fisheries er skeena versus nass boxplot nbrr.png",dpi=600,height=7,width=6)
