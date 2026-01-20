# script for coho figure 13- SEAK exploitation rate by conservation unit
# prep data file with AK ERs
make.co.seak.ers <- function(data){
  data$AKER<-data$`Total ER`-data$`CDN ER`
  data$AKcatch<-data$`Total Harvest`-data$`CDN Harvest`
  
  peo<-data%>%filter(SpeciesId%in%c("CO"))
  
  ggplot(peo,aes(x=Year,y=AKER))+
  geom_point()+geom_line()+geom_smooth(alpha=.5,se=FALSE)+
  theme_bw()+
  labs(y="SEAK Exploitation Rate",x="Year",color="",
       title="SEAK Exploitation Rate by Conservation Unit",subtitle="Coho Salmon (1954-2017)")+
  theme(axis.text.y=element_text(size=7),legend.position = "bottom")+
  facet_wrap(~CU_Name,ncol=3)
}
