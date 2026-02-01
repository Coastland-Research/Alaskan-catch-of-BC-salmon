#### ALSEK COHO PINK CHUM CATCH ####

make.co.pk.cm.catch<-function(catch){
  
  cpue<-catch%>%select(-Coho.subsistence)%>%pivot_longer(Coho:Chum,names_to="Species",values_to="Catch")%>%
  mutate(cpue=Catch/Effort)
  
  f1<-ggplot(cpue,aes(x=Year,y=Catch,fill=Species))+
  geom_col()+
  facet_grid(Species~.,scales="free_y",switch="y")+
  theme_bw()+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="none")
  
  f2<-ggplot(cpue,aes(x=Year,y=Effort,fill=Species))+
  geom_col()+
  facet_grid(Species~.,scales="free_y",switch="y")+
  theme_bw()+labs(y="Effort (boat days)")+
  scale_fill_brewer(palette="Set1")+
  theme(legend.position="none",strip.text.y.left=element_blank())
  
  f3<-ggplot(cpue,aes(x=Year,y=cpue,color=Species))+
  geom_point()+geom_line()+
  facet_grid(Species~.,scales="free_y",switch="y")+
  theme_bw()+
  scale_color_brewer(palette="Set1")+labs(y="CPUE")+
  theme(legend.position="none",strip.text.y.left=element_blank())
  
  f1+f2+f3+plot_annotation(title="Alsek River Coho, Pink and Chum Salmon",subtitle="US Catch, Effort, and CPUE",
                         theme = theme(plot.title = element_text(hjust = 0.5),
                                       plot.subtitle=element_text(hjust=.5)))
}
  
  