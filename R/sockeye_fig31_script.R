# figure 31 - sockeye stock composition by week for Districts 101 and 104 (2016-2018)

make.stockcomp.d101104 <- function(data){
  scold<-data%>%filter(year>2015)
  
  ggplot(scold,aes(x=week,y=mean/100,fill=stock))+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Blues") +
  # scale_fill_manual(values=mycolors)+
  labs(y="Proportion",x="Week",fill="Region",
       title="Weekly Stock Composition 2016-2018",subtitle="Sockeye: Districts 101 and 104")+
  facet_grid(year~district)+
  ylim(0,1)
}