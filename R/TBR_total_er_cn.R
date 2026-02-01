make.total.er.cn <- function(cn){
  
  cn[,6:35]<-cn[,6:35]/100
  cnstocks<-cn%>%filter(!Stock%in%c("Atnarko.Yearling","Kitsumkalum.Yearling","NWVI","SWVI","EVIN"))
  cnstocks.TBR<-cnstocks%>%filter(Region == "TBR")
  us<-cnstocks.TBR%>%select("Stock","Catch.Year","SEAK.Troll","SEAK.Net","SEAK.Sport",
                            "ISBM.NF.Troll","ISBM.NF.Sport","ISBM.SF.Troll","ISBM.SF.Sport",
                            "ISBM.WAC.Net","ISBM.WAC.Net","ISBM.PS.Net","ISBM.PS.Sport",
                            "Term.SEAK.Troll","Term.SEAK.Net","Term.SEAK.Sport",
                            "Term.SUS.Troll","Term.SUS.Net","Term.SUS.Sport")%>%
    mutate(toter=rowSums(across("SEAK.Troll":"Term.SUS.Sport")))%>%
    select(Stock,Catch.Year,toter)%>%
    mutate(country = "US")
  can<-cnstocks.TBR%>%select(-c("SEAK.Troll","SEAK.Net","SEAK.Sport",
                                "ISBM.NF.Troll","ISBM.NF.Sport","ISBM.SF.Troll","ISBM.SF.Sport",
                                "ISBM.WAC.Net","ISBM.WAC.Net","ISBM.PS.Net","ISBM.PS.Sport",
                                "Term.SEAK.Troll","Term.SEAK.Net","Term.SEAK.Sport",
                                "Term.SUS.Troll","Term.SUS.Net","Term.SUS.Sport", "Ages","CWTs","Escapement", "Stray"))%>%
    mutate(toter=rowSums(across("NBC.Troll":"Term.CAN.Sport")))%>%
    select(Stock,Catch.Year,toter)%>%
    mutate(country = "CAN")
  stock.box.country<-rbind(us,can)
  ggplot(stock.box.country,aes(x=Catch.Year,y=toter,color=country))+
    geom_point()+geom_line()+
    labs(x="Year",y="Total Exploitation Rate",title="Total Exploitation Rate: Transboundary Rivers",
         subtitle="Chinook (Taku and Stikine)")+
    theme_bw()+
    scale_color_brewer(palette="Set1")+
    facet_wrap(~Stock,ncol=2)+
    geom_smooth()+
    ylim(0,.6)
}
