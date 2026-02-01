#### ALSEK CHINOOK catch escapement and proportion catch ####
make.alsek.cn.catch <- function(alsek){
  
  esc <- alsek %>%
  select(Year,Klukshu.counts)
  low.year <- 1976
  
  esc_harvest.plot <- ggplot(esc, aes(x=Year,y=Klukshu.counts))+
  geom_col(color="black",size=0.55,fill="grey30")+
  theme_bw()+
  xlim(low.year,2023)+
  scale_fill_manual(values=c("white","gray68","gray27"),labels=c("CDN harvest", "US harvest","Escapement"))+
  labs(fill="",y="Klukshu Chinook Count",x="Year")
  
  # cdn and us harvest beside each other position="dodge"
  cdn_us_harvest <- alsek %>%
  select(Year,CDN.total,US.total)%>%
  pivot_longer(CDN.total:US.total,names_to="variable",values_to="value")
  
  cdn_us_harvest$variable <-as.factor(as.character(cdn_us_harvest$variable))%>%
  factor(levels = c("CDN.total","US.total"))
  
  cdn_us_harvest.plot <- ggplot(cdn_us_harvest, aes(x=Year,y=value, fill=variable))+
  geom_bar(stat="identity",size=0.55, position="dodge")+
  theme_bw()+
  xlim(low.year,2023)+
  scale_fill_brewer(palette = "Set1",labels=c("CDN harvest", "US harvest"))+
  labs(fill="",y="Catch",x="Year")
  
  cols<-brewer.pal("Set1",n=3)[1:2]
  
  # TBR percent of total harvest
  percent_harvest <- alsek %>%
  mutate(total.harvest = US.total + CDN.total) %>%
  mutate(us.percent = US.total/total.harvest*100)%>%
  mutate(cdn.percent = CDN.total/total.harvest*100)%>%
  select(Year,us.percent,cdn.percent)%>%
  pivot_longer(us.percent:cdn.percent,names_to="variable",values_to="value")
  
  percent_harvest$variable <-as.factor(as.character(percent_harvest$variable))%>%
  factor(levels = c("cdn.percent","us.percent"))
  
  percent_harvest.plot<- ggplot(percent_harvest,aes(x=Year,y=value, fill=variable))+
  scale_fill_manual(values=cols, labels=c("CDN % of total harvest","US % of total harvest"))+
  geom_area()+
  theme_bw()+
  theme(legend.title = element_blank())+
  xlim(low.year,2023)+
  labs(y="Percent of total catch",x="Year")
  
  esc_harvest.plot+
  cdn_us_harvest.plot+
  percent_harvest.plot+
  plot_layout(ncol = 1)+plot_annotation(title="TBR: Alsek River Chinook Salmon")
}

