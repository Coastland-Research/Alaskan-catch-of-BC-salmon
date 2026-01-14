# Figure 28 - Canadian and SEAK exploitation rates for Area 5 sockeye

sa.data<-read.csv("data-raw/NCConlystatareadata.csv")

sa.data$AKER<-sa.data$`Total.ER`-sa.data$`CDN.ER`
sa.data$AKcatch<-sa.data$`Total.Harvest`-sa.data$`CDN.Harvest`

sknass<-sa.data%>%filter(SpeciesId=="SX"&`StatArea.CU`%in%c("3","4"))

a5sx.ers<-sa.data%>%filter(SpeciesId=="SX"&`StatArea.CU`%in%c("5"))%>%
  select(Year,`CDN.ER`,`SEAK.ER`=AKER)%>%
  pivot_longer(2:3,names_to="region",values_to="ers")

ggplot(a5sx.ers,aes(x=Year,y=ers,color=region))+
  geom_point()+geom_line()+
  theme_bw()+
  scale_color_manual(values=c("#E41A1C","#377EB8"),labels=c("CDN","SEAK"))+
  theme(legend.position = "right")+
  xlim(1960,2020)+
  labs(y="Exploitation Rate",title="CDN and SEAK Exploitation of Area 5 Sockeye",color="Region")

# ggsave("CU and SA ERs/SX A5 CDN and US ERs.png",units="in",dpi=600,height=4,width=6)
