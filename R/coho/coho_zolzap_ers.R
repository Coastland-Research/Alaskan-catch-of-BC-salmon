# script for coho Figure 14 - CDN (blue) and SEAK (red) exploitation rates (top) and the percent of SEAK harvest for Zolzap Creek coho. Source: LGL 2021b, Noble et al. 2020.
#### ZOLZAP ERs ####
make.co.zz.plot <- function(zz) {
  zz$type[zz$stat=="ER.CDN"]<-"ER"
  zz$type[zz$stat=="ER.US"]<-"ER"
  zz$type[zz$stat=="p.CDN"]<-"Proportion"
  zz$type[zz$stat=="p.US"]<-"Proportion"
  
  zz$country[zz$stat=="ER.CDN"]<-"CDN"
  zz$country[zz$stat=="ER.US"]<-"US"
  zz$country[zz$stat=="p.CDN"]<-"CDN"
  zz$country[zz$stat=="p.US"]<-"US"
  
  zzer<-zz%>%select(Year,CDN=ER.CDN,US=ER.US)%>%
  pivot_longer(2:3,names_to="Country",values_to="ER")
  
  p1<-ggplot(zzer,aes(x=Year,y=ER,color=factor(Country,levels=c("US","CDN"))))+
  geom_line()+geom_point()+
  scale_color_brewer(palette="Set1",labels=c("SEAK","CDN"))+
  labs(colour="Country",y="Exploitation Rate",
       title="Zolzap Coho Exploitation Rate and Proportion US Harvest")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(size=10))
  
  p2<-ggplot(zz,aes(x=Year,y=p.US*100))+
  geom_line()+geom_point()+
  ylim(0,100)+
  labs(y="Percent SEAK Harvest")+
  theme_bw()
  
  ggarrange(p1, p2,
  ncol = 1,
  common.legend = TRUE,
  legend = "right",
  align = "v")
}

