#### SEAK HARVEST BY DISTRICT 101-106 FIGURES ####
make.cn.districts.plot <- function(districts){
  
  d101106totals<-districts%>%mutate_all(funs(replace(.,.=="Confidential",0)))%>%
  mutate_at(vars('Number Of Fish (estimated)'),funs(as.numeric))%>%
  filter(`District Number`%in%c(101,102,103,104,105,106)&Year==2021)%>%
  group_by(`Species Name`,`District Number`,`Gear Name`)%>%summarise(total=sum(`Number Of Fish (estimated)`))
  
  write.csv(d101106totals,"District 101-106 Totalssp.csv")
  sp="Chinook Salmon"
  
  justsomesp<-districts%>%mutate_all(funs(replace(.,.=="Confidential",0)))%>%
  mutate_at(vars('Number Of Fish (estimated)'),funs(as.numeric))%>%
  filter(`District Number`%in%c(101,102,103,104,105,106)&`Species Name`==sp)%>%
  group_by(`District Number`,Year)%>%summarise(total=sum(`Number Of Fish (estimated)`))
  
  ggplot(justsomesp,aes(x=Year,y=total))+
  geom_point()+geom_line()+
  theme_bw()+
  geom_smooth(alpha=.25)+
  labs(x="Year",y="Total Catch",title="SSEAK Catch All Gear by District (101-106)",
       subtitle="Chinook Salmon (1985-2021)")+
  facet_wrap(~`District Number`,ncol=2)+
  theme(legend.position = "none")
}
