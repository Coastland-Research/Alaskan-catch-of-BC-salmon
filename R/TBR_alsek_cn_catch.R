#### ALSEK CHINOOK catch gn sport pu ####
make.alsek.cn.catch <- function(alsek){
  
  alsek.cn<-alsek%>%select(Year,US.Commercial,US.Subsistence,US.Test,CDN.FN,CDN.Sport)%>%
  pivot_longer("US.Commercial":"CDN.Sport",names_to="Fishery",values_to="Catch")%>%
  mutate(Country=case_when(Fishery%in%c("US.Commercial","US.Subsistence","US.Test")~"SEAK",
                           Fishery%in%c("CDN.FN","CDN.Sport")~"Canada"))
  
  ggplot(alsek.cn,aes(x=Year,y=Catch,fill=Fishery))+
  facet_wrap(~Country,ncol=1,scales="free_y")+
  geom_col(position="stack")+theme_bw()+
  scale_fill_brewer(palette="Set1")+
  labs(x="Year",title="Alsek River Chinook",subtitle="Catch by Fishery (1960-2022)")
  
}

