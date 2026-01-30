
#### STIKINE CHINOOK D108 Catch and prop ####
make.stikine.cn.catch <- function(data, data3){
  data2<-data%>%select(-total)%>%pivot_longer(2:5,names_to="Fishery",values_to="Catch")
  p1<-ggplot(data2,aes(x=year,y=Catch,fill=Fishery))+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set1")+
  theme_bw()+labs(x="Year",title="TBR: Stikine River Chinook",subtitle="US District 108 Catch by Fishery and Stock Proportion")
  
  p2<-ggplot(data2,aes(x=year,y=Catch,fill=Fishery))+
  geom_bar(stat="identity",position="fill")+
  scale_fill_brewer(palette="Set1")+
  theme_bw()+labs(x="Year")
  
  data4<-data3%>%pivot_longer(2:3,names_to="Fishery",values_to="Proportion")
  
  p3<-ggplot(data4,aes(x=year,y=Proportion,color=Fishery))+
  geom_point()+geom_line()+
  scale_color_brewer(palette="Set1")+
  theme_bw()+labs(x="Year")+xlim(2005,2022)
  
  p1+p2+p3+plot_layout(ncol=1)
}
