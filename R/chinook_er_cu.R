# prep data file with AK ERs
make.cn.er.cu <- function(seak_er){
  seak_er$AKER<-seak_er$`Total ER`-seak_er$`CDN ER`
  seak_er$AKcatch<-seak_er$`Total Harvest`-seak_er$`CDN Harvest`
  
  peo<-seak_er%>%filter(SpeciesId%in%c("CN"))%>%filter(!CU_Name%in%c("Haida Gwaii-North",
                                                                "NORTH & CENTRAL COAST-LATE TIMING",
                                                                "NORTH & CENTRAL COAST-EARLY TIMING",
                                                                "Dean River"))
  
  ggplot(peo,aes(x=Year,y=AKER))+
  geom_point()+geom_line()+geom_smooth(alpha=.5,se=FALSE)+
  theme_bw()+
  labs(y="SEAK Exploitation Rate",x="Year",color="",
       title="SEAK Exploitation Rate by Conservation Unit",subtitle="Chinook Salmon (1985-2017)")+
  theme(axis.text.y=element_text(size=7),legend.position = "bottom")+
  facet_wrap(~CU_Name,ncol=3)
} 
