# script for pink salmon figure 3

make.pink.harv <- function(harv){
  
  harv.se<-harv%>%filter(Region=="Southeast")
  
  sp<-"Pink"
  harv.value<-harv.se%>%filter(`Species Code`==sp)%>%arrange(Year)
  
  ggplot(harv.value,aes(x=Year,y=`Number of Fish (estimated)`/10^6))+
  geom_bar(stat="identity",color="black",fill="grey75")+
  # geom_smooth(se=FALSE)+
  labs(title=paste0("SEAK Harvest: ",sp," Salmon (1979-2021)"),
       y="Catch (millions)")+
  expand_limits(y=0)+
  theme_bw()
  
}