# Transboundary - D106 and D108 SEAK CATCH

make.tbr.106108.catch <- function(d106, d108){
  
  gg.d106<-d106%>%pivot_longer("Chinook":"Chum",names_to="Species",values_to="Catch")%>%
  mutate(District="District 106")
  
  gg.d108<-d108%>%pivot_longer("Chinook":"Chum",names_to="Species",values_to="Catch")%>%
  mutate(District="District 108")
  
  gg.catch<-bind_rows(gg.d106,gg.d108)
  
  ggplot(gg.catch,aes(x=year,y=Catch,fill=Species))+
  geom_bar(stat="identity")+
  facet_grid(Species~District,scales="free_y")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  labs(title="SEAK District 106 and 108 Catch",subtitle="All salmon species (1960-2023)",x="Year")
  
}

