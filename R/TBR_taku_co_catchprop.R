#### Load Data ####
cn_alsek_E4E5E6E8_TBR<-fread("TBR/cn_alsek_E4E5E6E8_TBR.csv")
TBR_B21_StikineRiver_SX<-fread("TBR/TBR_B21_StikineRiver.csv")
TBR.D5.Taku.Lchinook<-fread("TBR/TBR.D5.Taku.Lchinook.csv")
TBR.D15.Taku.SX<-fread("TBR/TBR.D15.Taku.SX.csv")
TBR.D20.Taku.CO<-fread("TBR/TBR.D20.Taku.CO.csv")
MSA.101.102.103 <-bind_rows(MSA.table4.102.2015,MSA.table3.101.2015,MSA.table5.103.2015,
                            MSA.table3.101.2016,MSA.table4.102.2016,MSA.table5.103.2016,
                            MSA.table5.103.2018,MSA.table4.102.2018,MSA.table3.101.2018)
alsek.SX<-fread("archive/archived data/2022-01-05SX.alsek.inriver.run2006_2019.csv")
cn<-fread("archive/archived data/cn_all_mort_updated.csv")

#### plots#### plots#### plots  TBR report ####
# escapement and harvest to make total run stacked bar
esc_harvest <- TBR.D5.Taku.Lchinook %>%
  select(year,cdn_harvest,us_harvest,escapement)%>%
  pivot_longer(cdn_harvest:escapement,names_to="variable",values_to="value")
esc_harvest$variable <-as.factor(as.character(esc_harvest$variable))%>%
  factor(levels = c("cdn_harvest","us_harvest_tr","escapement"))
esc_harvest.plot <- ggplot(esc_harvest, aes(x=year,y=value, fill=variable))+
  geom_bar(stat="identity", color="black",size=0.55)+
  theme_bw()+
  scale_fill_manual(values=c("white","gray68","gray27"),labels=c("CDN harvest", "US harvest","Escapement"))+
  labs(fill="",y="Number of Fish",x="Year")

# cdn and us harvest beside each other position="dodge"
cdn_us_harvest <- TBR.D20.Taku.CO %>%
  select(year,cdn_harvest,us_harvest_tr)%>%
  pivot_longer(cdn_harvest:us_harvest_tr,names_to="variable",values_to="value")
cdn_us_harvest$variable <-as.factor(as.character(cdn_us_harvest$variable))%>%
  factor(levels = c("cdn_harvest","us_harvest_tr"))
cdn_us_harvest.plot <- ggplot(cdn_us_harvest, aes(x=year,y=value, fill=variable))+
  geom_bar(stat="identity",size=0.55, position="dodge")+
  theme_bw()+
  scale_fill_brewer(palette = "Set1",labels=c("CDN harvest", "US harvest"))+
  labs(fill="",y="Number of Fish",x="Year")

# TBR percent of total harvest
percent_harvest <- TBR.D20.Taku.CO %>%
  mutate(total.harvest = us_harvest_tr + cdn_harvest) %>%
  mutate(us.percent = us_harvest_tr/total.harvest*100)%>%
  mutate(cdn.percent = cdn_harvest/total.harvest*100)%>%
  select(year,us.percent,cdn.percent)%>%
  pivot_longer(us.percent:cdn.percent,names_to="variable",values_to="value")
percent_harvest$variable <-as.factor(as.character(percent_harvest$variable))%>%
  factor(levels = c("cdn.percent","us.percent"))
percent_harvest.plot<- ggplot(percent_harvest,aes(x=year,y=value, fill=variable))+
  scale_fill_manual(values=c("firebrick3","royalblue3"), labels=c("CDN % of total harvest","US % of total harvest"))+
  geom_area()+
  theme_bw()+
  theme(legend.title = element_blank())+
  labs(y="Percent of total harvest",x="Year")

TBR.D20.Taku.CO_plot<-esc_harvest.plot + cdn_us_harvest.plot + percent_harvest.plot + plot_layout(ncol = 1)
TBR.D20.Taku.CO_plot