# script for Pink salmon Figure 14 - SEAK exploitation rate by CU by year

pink.seak.er.cu <- function(er_cu){
  
  er_cu$AKER<-er_cu$`Total ER`-er_cu$`CDN ER`
  er_cu$AKcatch<-er_cu$`Total Harvest`-er_cu$`CDN Harvest`
  
  peo<-er_cu%>%filter(SpeciesId%in%c("PKe","PKo"))
  
  peo.ak<-peo%>%filter(!CU_Name%in%c("West Haida Gwaii","North Haida Gwaii","Hecate Strait-Fjords",
                                   "East Haida Gwaii","Homathko-Klinaklini-Smith-Rivers-Bella Coola-Dean"))
  
  ggplot(peo.ak,aes(x=Year,y=AKER,colour=SpeciesId))+
  geom_point()+geom_line()+geom_smooth(color="grey50",alpha=.5,se=FALSE)+
  theme_bw()+
  scale_color_brewer(palette="Set1",labels=c("Even-Year","Odd-Year"))+
  labs(y="SEAK Exploitation Rate",x="Year",color="",
       title="SEAK Exploitation Rate by Conservation Unit",subtitle="Pink Salmon (1954-2017)")+
  theme(axis.text.y=element_text(size=7),legend.position = "bottom")+
  facet_wrap(SpeciesId~CU_Name,ncol=3)
}