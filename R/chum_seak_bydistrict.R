# script for chum salmon SEAK harvest by district
make.chum.harv.bydist <- function(data){
  sp="Chum Salmon"
  
  justsomesp<-data%>%mutate_all(funs(replace(.,.=="Confidential",0)))%>%
  mutate_at(vars('Number Of Fish (estimated)'),funs(as.numeric))%>%
  filter(`District Number`%in%c(101,102,103,104,105,106)&`Species Name`==sp)%>%
  group_by(`District Number`,Year)%>%summarise(total=sum(`Number Of Fish (estimated)`))
  
  ggplot(justsomesp,aes(x=Year,y=total/10^6))+
  geom_point()+geom_line()+
  theme_bw()+
  geom_smooth(alpha=.25)+
  labs(x="Year",y="Total Catch (millions)",title="SSEAK Catch All Gear by District (101-106)",
       subtitle="Chum Salmon (1985-2021)")+
  facet_wrap(~`District Number`,ncol=2)+
  theme(legend.position = "none")
}
