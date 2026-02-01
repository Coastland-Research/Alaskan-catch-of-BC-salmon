#### ALSEK total run and ERs ####

make.alsek.cn.tr.ers <- function(alsek){
  alsek.cn<-alsek%>%select(Year,CDN.run,CDN.run.low,CDN.run.high)
  
  fig1<-ggplot(alsek.cn,aes(x=Year,y=CDN.run))+
  geom_col(fill="steelblue",color="black")+
  geom_errorbar(aes(ymin=CDN.run.low,ymax=CDN.run.high,width=.2))+
  theme_bw()+labs(y="Terminal Run")+
  scale_x_continuous(labels = seq(1995,2005,5),breaks=seq(1995,2005,5),limits=c(1995,2005))
  
  alsek.er<-alsek%>%select(Year,Canada=CDN.HR,US=US.HR)%>%pivot_longer(2:3,names_to="Country",values_to="Exploitation Rate")
  
  fig2<-ggplot(alsek.er,aes(x=Year,y=`Exploitation Rate`,color=Country))+
  geom_point()+geom_line()+theme_bw()+
  scale_color_brewer(palette="Set1")+
  scale_x_continuous(labels = seq(1995,2005,5),breaks=seq(1995,2005,5),limits=c(1995,2005))
  
  fig1+fig2+plot_layout(ncol = 1)+plot_annotation(title="Alsek River Chinook",subtitle="Above border run 1998-2004",tag_levels='A')
}
