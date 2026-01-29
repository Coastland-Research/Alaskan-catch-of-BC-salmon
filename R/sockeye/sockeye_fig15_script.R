# script for SEAK sockeye report figure 15 - proportion of catch before Week 31

make.prop.week31.sk <- function(pre.sn){
  
  pre.sn.long<-pre.sn%>%select(Year,Skeena=Sk.pre.p,Nass=Nass.pre.p)%>%
    pivot_longer(2:3,names_to="Area",values_to="p")
  
  pre.skeena <- pre.sn.long %>%
  filter(Area == "Skeena")
  
  skeena_pre_31 <- ggplot(pre.skeena,aes(y=p,x=Year))+
  geom_bar(stat="identity",color="black", fill = "steelblue")+
  #geom_point(alpha=.5)+geom_line(alpha=.5)+
  theme_bw()+
  labs(x="Year",y="Proportion of Total D104\nSockeye Catch",
       title=paste0("D104 Proportion of Catch before Week 31 - Skeena"))+
  theme(legend.position ="right")
  
  skeena_pre_31
}

make.prop.week31.nass <- function(pre.sn){
  
  pre.sn.long<-pre.sn%>%select(Year,Skeena=Sk.pre.p,Nass=Nass.pre.p)%>%
    pivot_longer(2:3,names_to="Area",values_to="p")
  
  pre.nass <- pre.sn.long %>%
    filter(Area == "Nass")
  
  nass_pre_31 <- ggplot(pre.nass,aes(y=p,x=Year))+
  geom_bar(stat="identity",color="black", fill = "firebrick")+
  #geom_point(alpha=.5)+geom_line(alpha=.5)+
  theme_bw()+
  labs(x="Year",y="Proportion of Total D104\nSockeye Catch",
       title=paste0("D104 Proportion of Catch before Week 31 - Nass"))+
  theme(legend.position ="right")
  
  nass_pre_31
}

