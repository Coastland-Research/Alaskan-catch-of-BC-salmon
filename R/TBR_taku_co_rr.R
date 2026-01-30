#### TAKU Coho D5 RUN RECONSTRUCTION ####
make.taku.co.rr <- function(taku_co){
  
  low.year<-1990
  esc_harvest <- taku.co %>%
  select(year,cdn_harvest,us_harvest_tr,escapement)%>%
  pivot_longer(cdn_harvest:escapement,names_to="variable",values_to="value")
  
  esc_harvest$variable <-as.factor(as.character(esc_harvest$variable))%>%
  factor(levels = c("cdn_harvest","us_harvest_tr","escapement"))          ###changed to "us_harvest_tr" --> it was creating NA which then mixed up US harvest & escapement data
  esc_harvest.plot <- ggplot(esc_harvest, aes(x=year,y=value, fill=variable))+
  geom_bar(stat="identity", color="black",size=0.55)+
  theme_bw()+
  xlim(low.year,2024)+
  scale_fill_manual(values=c("white","gray68","gray27"),labels=c("CDN harvest", "US harvest","Escapement"))+
  labs(fill="",y="Number of Fish",x="Year")
  
  # cdn and us harvest beside each other position="dodge"
  cdn_us_harvest <- taku.co %>%
  select(year,cdn_harvest,us_harvest_tr)%>%
  pivot_longer(cdn_harvest:us_harvest_tr,names_to="variable",values_to="value")
  
  cdn_us_harvest$variable <-as.factor(as.character(cdn_us_harvest$variable))%>%
  factor(levels = c("cdn_harvest","us_harvest_tr"))
  cdn_us_harvest.plot <- ggplot(cdn_us_harvest, aes(x=year,y=value, fill=variable))+
  geom_bar(stat="identity",size=0.55, position="dodge")+
  theme_bw()+
  xlim(low.year,2024)+
  scale_fill_brewer(palette = "Set1",labels=c("CDN harvest", "US harvest"))+
  labs(fill="",y="Number of Fish",x="Year")
  
  cols<-brewer.pal("Set1",n=3)[1:2]
  
  # TBR percent of total harvest
  percent_harvest <- taku.co %>%
  mutate(total.harvest = us_harvest_tr + cdn_harvest) %>%
  mutate(us.percent = us_harvest_tr/total.harvest*100)%>%
  mutate(cdn.percent = cdn_harvest/total.harvest*100)%>%
  select(year,us.percent,cdn.percent)%>%
  pivot_longer(us.percent:cdn.percent,names_to="variable",values_to="value")
  
  percent_harvest$variable <-as.factor(as.character(percent_harvest$variable))%>%
  factor(levels = c("cdn.percent","us.percent"))
  percent_harvest.plot<- ggplot(percent_harvest,aes(x=year,y=value, fill=variable))+
  scale_fill_manual(values=cols, labels=c("CDN % of total harvest","US % of total harvest"))+
  geom_area()+
  theme_bw()+
  theme(legend.title = element_blank())+
  xlim(low.year,2024)+
  labs(y="Percent of total harvest",x="Year")
  
  esc_harvest.plot+
  cdn_us_harvest.plot+
  percent_harvest.plot+
  plot_layout(ncol = 1)+plot_annotation(title="TBR: Taku River Coho Salmon")
  
}

