#### ALSEK sockeye catch escapement and proportion catch ####

make.alsek.sx.catchprop<-function(alsek.sx){
  
  esc_harvest <- alsek.sx %>%
    select(Year,canadian.harvest,us.harvest,spawning.esc)%>%
    pivot_longer(canadian.harvest:spawning.esc,names_to="variable",values_to="value")
  esc_harvest$variable <-as.factor(as.character(esc_harvest$variable))%>%
    factor(levels = c("canadian.harvest","us.harvest","spawning.esc"))
  esc_harvest.plot <- ggplot(esc_harvest, aes(x=Year,y=value, fill=variable))+
    geom_bar(stat="identity", color="black",size=0.55)+
    theme_bw()+
    scale_fill_manual(values=c("white","gray68","gray27"),labels=c("CDN harvest", "US harvest","Escapement"))+
    labs(fill="",y="Number of Fish",x="Year")
  
  # cdn and us harvest beside each other position="dodge"
  cdn_us_harvest <- alsek.sx %>%
    select(Year,canadian.harvest,us.harvest)%>%
    pivot_longer(canadian.harvest:us.harvest,names_to="variable",values_to="value")
  cdn_us_harvest$variable <-as.factor(as.character(cdn_us_harvest$variable))%>%
    factor(levels = c("canadian.harvest","us.harvest"))
  cdn_us_harvest.plot <- ggplot(cdn_us_harvest, aes(x=Year,y=value, fill=variable))+
    geom_bar(stat="identity",size=0.55, position="dodge")+
    theme_bw()+
    scale_fill_brewer(palette = "Set1",labels=c("CDN harvest", "US harvest"))+
    labs(fill="",y="Number of Fish",x="Year")+
    xlim(2000,2023)
    
  
  # TBR percent of total harvest
  percent_harvest <- alsek.sx %>%
    mutate(total.harvest = us.harvest + canadian.harvest) %>%
    mutate(us.percent = us.harvest/total.harvest*100)%>%
    mutate(cdn.percent = canadian.harvest/total.harvest*100)%>%
    select(Year,us.percent,cdn.percent)%>%
    pivot_longer(us.percent:cdn.percent,names_to="variable",values_to="value")
  percent_harvest$variable <-as.factor(as.character(percent_harvest$variable))%>%
    factor(levels = c("cdn.percent","us.percent"))
  percent_harvest.plot<- ggplot(percent_harvest,aes(x=Year,y=value, fill=variable))+
    scale_fill_manual(values=c("firebrick3","royalblue3"), labels=c("CDN % of total harvest","US % of total harvest"))+
    geom_area()+
    theme_bw()+
    theme(legend.title = element_blank())+
    labs(y="Percent of total harvest",x="Year")
  
  esc_harvest.plot + cdn_us_harvest.plot + percent_harvest.plot + plot_layout(ncol = 1)
  
}
