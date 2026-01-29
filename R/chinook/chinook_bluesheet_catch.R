# Chinook - Blue Sheet fisheries SEAK catch by fishery 
make.cn.blusheet.box <- function(data){
  
  sp="CHINOOK"
  harv<-data%>%filter(SPECIES==sp)%>%
  filter(!FISHERY%in%c("Region Totals","Northern Totals","Total Troll",
                       "Power Troll Total","Hand Troll Total","Total Purse Seine",
                       "Southern Purse Seine Total","Northern Purse Seine Total",
                       "Total Drift Gillnet","Total Annette Island Reservation",
                       "Total Annette Island Troll","Southern Totals"))%>%
  mutate_at(vars(HARVEST),funs(as.numeric))
  
  harv$HARVEST[harv$HARVEST==0]<-NA
  
  harvsplist<-harv%>%group_by(FISHERY)%>%mutate_at(vars(HARVEST),funs(as.numeric))%>%
  summarise(med=median(HARVEST,na.rm=TRUE))%>%arrange(med)%>%filter(!is.na(med))
  
  harvsp<-harv%>%filter(FISHERY%in%harvsplist$FISHERY)%>%mutate_at(vars(HARVEST),funs(as.numeric))
  
  cn<-harv%>%filter(FISHERY=="Southern Purse Seine Traditional")%>%filter(!is.na(HARVEST))
ecdf(cn$HARVEST)(6836)

ggplot(harvsp,aes(x=HARVEST/10^6,y=factor(FISHERY,levels=harvsplist$FISHERY)))+
  geom_boxplot(fill="steelblue")+theme_bw()+
  labs(x="Catch (millions)",y="Fishery",title="SEAK Catch of Chinook Salmon by Fishery"
       ,subtitle="Blue Sheet Fisheries (1980-2020)")+
  theme(axis.text.y = element_text(size=8))
}


