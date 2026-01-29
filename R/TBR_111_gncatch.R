#### D111 SEAK CATCH ####
d111<-read_excel("data-raw/TBR/TBR data_updated2023.xlsx",sheet="TBR.D111harvest")

make.tbr.d111.gncatch <- function(d111){
  
  gg.d111<-d111%>%pivot_longer("Chinook":"Chum",names_to="Species",values_to="Catch")%>%
  mutate(District="District 111")
  
  ggplot(gg.d111,aes(x=year,y=Catch,fill=Species))+
  geom_col(width=.9,color="black")+
  facet_wrap(~Species,scales="free_y",ncol=1)+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  labs(title="SEAK District 111 Commercial Gillnet Catch",subtitle="All salmon species (1960-2023)",x="Year")
  
}

