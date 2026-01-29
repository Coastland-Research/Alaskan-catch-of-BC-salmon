# script for Chum figure 13 - SEAK exploitation rate by CU
chum.seak.er.cu <- function(er_cu){
  
  er_cu$AKER<-er_cu$`Total ER`-er_cu$`CDN ER`
  er_cu$AKcatch<-er_cu$`Total Harvest`-er_cu$`CDN Harvest`
  
  peo<-er_cu%>%filter(SpeciesId%in%c("CM"))
  
  peo%>%distinct(CU_Name,CU)
  peo.ak<-peo%>%filter(CU%in%c("CM_32","CM_31","CM_30","CM_26","CM_27","CM_28","CM_18")) %>%
    select(Year, AKER, `CDN ER`, CU_Name) %>%
    pivot_longer(cols = c(AKER, `CDN ER`), names_to = "Region", values_to = "ER")
  
  ggplot(peo.ak,aes(x=Year,y=ER, colour = Region))+
  geom_point()+geom_line()+geom_smooth(alpha=.5,se=FALSE)+
  theme_bw()+
  labs(color="Region", y="SEAK Exploitation Rate",x="Year",
       title="SEAK and CDN Exploitation Rates by Conservation Unit",subtitle="Chum Salmon (1954-2017)")+
  theme(axis.text.y=element_text(size=7),legend.position = "right")+
  facet_wrap(~CU_Name,ncol=3)+
    scale_colour_manual(values=c("#377EB8","#E41A1C"),labels=c("CDN","SEAK"),breaks=c("CDN ER","AKER"))
  
}

