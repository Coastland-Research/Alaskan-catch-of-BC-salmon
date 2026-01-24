# script for chum salmon Figure 3 - SEAK harvest 1979-2021. remove LOESS line
make.chum.harv.plot <- function(harv){
  harv.se<-harv%>%filter(Region=="Southeast")
  sp<-"Chum"
  harv.value<-harv.se%>%filter(`Species Code`==sp)%>%arrange(Year)
  
  ggplot(harv.value,aes(x=Year,y=`Number of Fish (estimated)`/10^6))+
  geom_bar(stat="identity",color="black",fill="grey75")+
  labs(title=paste0("SEAK Harvest: ",sp," Salmon (1980-2022)"),
       y="Catch (millions)")+
  expand_limits(y=0)+
  theme_bw()
}
