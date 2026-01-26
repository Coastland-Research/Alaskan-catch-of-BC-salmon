#### figure for chum salmon ERs by stat area ####

make.chum.ers.sa <- function(sa.data){
  sa.data$AKER<-sa.data$`Total ER`-sa.data$`CDN ER`
  sa.data$AKcatch<-sa.data$`Total Harvest`-sa.data$`CDN Harvest`
  
  sp<-"Chum"
  sa.data$p.US<-sa.data$AKER/(sa.data$AKER+sa.data$`CDN ER`)
  sa.sp<-sa.data%>%filter(`Species Name`==sp)%>%select(Year,`StatArea/CU`,`CDN ER`,`AK ER`=AKER,p.US)%>%
  pivot_longer(3:5,names_to="region",values_to="er")
  
  ggdata<-sa.sp
  
  if (sp=="Chum"){
  #CM
  ggdata$`StatArea/CU` <-factor(ggdata$`StatArea/CU`,levels=c("1","02E","02W","3","4","5","6","7","8","9","10"))
} else if (sp=="Chinook") {
  #Chinook
  ggdata$`StatArea/CU` <-factor(ggdata$`StatArea/CU`,levels=c("3","4","6","8","9W","9S","10"))
} else if (sp=="Coho") {
  #CO
  ggdata$`StatArea/CU` <-factor(ggdata$`StatArea/CU`,levels=c("02E","02W","3","4","5","6","7","8","9","10"))
} else if (sp=="PinkOdd") {
  #PKo
  ggdata$`StatArea/CU` <-factor(ggdata$`StatArea/CU`,levels=c("02E","3","4","5","6","7","8","9","10"))
} else if (sp=="PinkEven") {
  #PKe
  ggdata$`StatArea/CU` <-factor(ggdata$`StatArea/CU`,levels=c("1","02E","02W","3","4","5","6","7","8","9","10"))
} else if (sp=="Sockeye") {
  #SX
  ggdata$`StatArea/CU` <-factor(ggdata$`StatArea/CU`,levels=c("1","02E","02W","3","4","5","6","7","8","9","10"))
}
  
  min.year<-min(ggdata$Year)
  ggdata2<-ggdata%>%filter(region%in%c("CDN ER","AK ER"))
  
  ggplot(ggdata2,aes(x=Year,y=er,color=region))+
  geom_point()+geom_line()+
  theme_bw()+
  facet_wrap(~`StatArea/CU`)+
  labs(color="Region",y="Exploitation Rate",title=paste0("SEAK and CDN Exploitation Rates by Stat Area"),
       subtitle=(paste0(sp," (",min.year,"-2017)")))+
  scale_colour_manual(values=c("#377EB8","#E41A1C"),labels=c("CDN","SEAK"),breaks=c("CDN ER","AK ER"))
}
