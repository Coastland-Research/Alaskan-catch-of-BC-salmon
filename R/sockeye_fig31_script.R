

#### SOCKEYE D101 and D014 WEEKLY STOCK COMPS ####
data<-read.csv("data-raw/2022-01-10d101.104GSI2006_2018.csv")
names(data)


scold<-data%>%filter(year>2015)

nb.cols <- 14
# mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

ggplot(scold,aes(x=week,y=mean/100,fill=stock))+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette = "Blues") +
  # scale_fill_manual(values=mycolors)+
  labs(y="Proportion",x="Week",fill="Region",
       title="Weekly Stock Composition 2016-2018",subtitle="Sockeye: Districts 101 and 104")+
  facet_grid(year~district)+
  ylim(0,1)