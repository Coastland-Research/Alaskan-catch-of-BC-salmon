# code for Chinook figure 17
# NEW DATA

make.cn.seak.ctc <- function(cn_new){
  
  cn_new[,6:35]<-cn_new[,6:35]/100
  
  cnstocks<-cn_new%>%filter(!Stock%in%c("Atnarko.Yearling","Kitsumkalum.Yearling","NWVI","SWVI","EVIN"))
  
  cn.seak.toter<-cnstocks%>%select(Stock,Region,Catch.Year,SEAK.Troll,SEAK.Net,SEAK.Sport,
                                 Term.SEAK.Troll,Term.SEAK.Net,Term.SEAK.Sport)%>%
  mutate(toter=rowSums(across("SEAK.Troll":"Term.SEAK.Sport")))
  
  cn.seak.all<-cnstocks%>%select(Stock,Region,Catch.Year,SEAK.Troll,SEAK.Net,SEAK.Sport,
                               Term.SEAK.Troll,Term.SEAK.Net,Term.SEAK.Sport)%>%
  pivot_longer("SEAK.Troll":"Term.SEAK.Sport",names_to="fishery",values_to="er")%>%
  filter(Region!="TBR")
  
  c1<-cn.seak.toter%>%group_by(Stock,Region)%>%summarise(med=median(toter,na.rm=TRUE))
  c2<-cn.seak.all%>%group_by(Stock,Region,fishery)%>%summarise(med=median(er,na.rm=TRUE))%>%
  pivot_wider(.,names_from="fishery",values_from="med")
  
  c.data<-merge(c1,c2,by=c("Stock","Region"))
  
  ggplot(cn.seak.toter,aes(x=Catch.Year,y=toter,color=Region))+
  geom_point()+geom_line()+
  labs(x="Year",y="Total SEAK Exploitation Rate",title="Total SEAK Exploitation Rate",
       subtitle="CTC Chinook Indicator Stocks")+
  theme_bw()+
  scale_color_brewer(palette="Set1")+
  facet_wrap(~factor(Stock),ncol=3)+
  geom_smooth(se=FALSE)+
  ylim(0,.6)
}


