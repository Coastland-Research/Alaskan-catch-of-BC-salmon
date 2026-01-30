#### Stikine Chinook ####

data<-stikine.cn
# escapement and harvest to make total run stacked bar
esc_harvest <- data %>%
  select(year,cdn_harvest=harv.cdn,us_harvest_tr=harv.us,escapement)%>%
  pivot_longer(cdn_harvest:escapement,names_to="variable",values_to="value")

esc_harvest$variable <-as.factor(as.character(esc_harvest$variable))%>%
  factor(levels = c("cdn_harvest","us_harvest_tr","escapement"))

esc_harvest.plot <- ggplot(esc_harvest, aes(x=year,y=value, fill=variable))+
  geom_bar(stat="identity", color="black",size=0.55)+
  theme_bw()+
  scale_fill_manual(values=c("white","gray68","gray27"),labels=c("CDN harvest", "US harvest","Escapement"))+
  labs(fill="",y="Number of Fish",x="Year",
       title="TBR: Stikine River",subtitle="Chinook Salmon")
esc_harvest.plot

# cdn and us harvest beside each other position="dodge"
cdn_us_harvest <- data %>%
  select(year,cdn_harvest=harv.cdn,us_harvest_tr=harv.us)%>%
  pivot_longer(cdn_harvest:us_harvest_tr,names_to="variable",values_to="value")

cdn_us_harvest$variable <-as.factor(as.character(cdn_us_harvest$variable))%>%
  factor(levels = c("cdn_harvest","us_harvest_tr"))

cdn_us_harvest.plot <- ggplot(cdn_us_harvest, aes(x=year,y=value, fill=variable))+
  geom_bar(stat="identity",size=0.55, position="dodge")+
  theme_bw()+
  scale_fill_brewer(palette = "Set1",labels=c("CDN harvest", "US harvest"))+
  labs(fill="",y="Number of Fish",x="Year")

cdn_us_harvest.plot

# TBR percent of total harvest
percent_harvest <- data %>%
  mutate(total.harvest = harv.cdn+harv.us) %>%
  mutate(us.percent = harv.us/total.harvest*100)%>%
  mutate(cdn.percent = harv.cdn/total.harvest*100)%>%
  select(year,us.percent,cdn.percent)%>%
  pivot_longer(us.percent:cdn.percent,names_to="variable",values_to="value")

percent_harvest$variable <-as.factor(as.character(percent_harvest$variable))%>%
  factor(levels = c("cdn.percent","us.percent"))

percent_harvest.plot<- ggplot(percent_harvest,aes(x=year,y=value, fill=variable))+
  scale_fill_manual(values=c("firebrick3","royalblue3"), labels=c("CDN % of total harvest","US % of total harvest"))+
  geom_area()+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(y="Percent of total harvest",x="Year")+
  xlim(1995,2022)
percent_harvest.plot

stikine.cn.plot<-esc_harvest.plot + cdn_us_harvest.plot + percent_harvest.plot + plot_layout(ncol = 1)
stikine.cn.plot
