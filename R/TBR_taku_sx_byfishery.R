
make.taku.sx.fishery <- function(d111.sx){
  
  d111.sx2<-d111.sx%>%select(-Gillnet.Wild,-Gillnet.Enhanced)%>%select(year,Total.Gillnet,Seine,Personal.Use)%>%
  pivot_longer("Total.Gillnet":"Personal.Use",names_to="Fishery",values_to="Catch")%>%
  mutate(Fishery=factor(Fishery,levels=c("Seine","Total.Gillnet","Personal.Use")))
  
  p1<-ggplot(d111.sx2,aes(x=year,y=Catch,fill=Fishery))+
  geom_col(position="dodge")+theme_bw()+
  scale_fill_brewer(palette="Set1")+
  labs(x="Year",y="Proportion of Catch")+
  theme(legend.position="bottom")
  
  d111.sx3<-d111.sx%>%select(-Total.Gillnet)%>%
  pivot_longer("Gillnet.Wild":"Personal.Use",names_to="Fishery",values_to="Catch")
  
  p2<-ggplot(d111.sx3,aes(x=year,y=Catch,fill=Fishery))+
  geom_col(position="fill")+theme_bw()+
  scale_fill_brewer(palette="Set1")+
  labs(x="Year")+
  theme(legend.position="bottom")
  
  gg<-ggarrange(p1,p2,common.legend = FALSE,legend="bottom",ncol=1)
  gg2<-annotate_figure(gg, top = text_grob("SEAK District 111 Taku River Sockeye Catch by Fishery", 
                                         color = "black", size = 12))
  gg2
  
}
