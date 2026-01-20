# prep data file with AK ERs
make.cn.er.cu <- function(seak_er){
  seak_er$AKER<-seak_er$`Total ER`-seak_er$`CDN ER`
  seak_er$AKcatch<-seak_er$`Total Harvest`-seak_er$`CDN Harvest`
  
  peo<-seak_er%>%filter(SpeciesId%in%c("CN"))%>%filter(!CU_Name%in%c("Haida Gwaii-North",
                                                                "NORTH & CENTRAL COAST-LATE TIMING",
                                                                "NORTH & CENTRAL COAST-EARLY TIMING",
                                                                "Dean River")) %>%
    mutate(Region = factor(case_when(CU_Name %in% c("Wannock", "Rivers Inlet", "Docee", "Bella Coola-Bentinck")~"Central Coast",
                                     CU_Name %in% c("Upper Nass", "Portland Sound-Observatory Inlet-Lower Nass")~"Nass",
                                     CU_Name %in% c("Ecstall", "Lower Skeena", "KALUM-EARLY TIMING", "KALUM-LATE TIMING",
                                                    "Middle Skeena-large lakes", "Middle Skeena-mainstem tributaries",
                                                    "Upper Bulkley River") ~ "Skeena")))
  peo_sk <- peo %>%
    filter(Region == "Skeena")
  peo_cc <- peo %>%
    filter(Region == "Central Coast")
  peo_nass <- peo %>%
    filter(Region == "Nass")
  
  sk <- ggplot(peo_sk,aes(x=Year,y=AKER))+
  geom_point()+geom_line()+geom_smooth(alpha=.5,se=FALSE)+
  theme_bw()+
  labs(y = "", x = "", color="", title="Skeena")+
  theme(axis.text.y=element_text(size=7),legend.position = "bottom")+
    facet_wrap(~CU_Name)
  cc <- ggplot(peo_cc,aes(x=Year,y=AKER))+
    geom_point()+geom_line()+geom_smooth(alpha=.5,se=FALSE)+
    theme_bw()+
    labs(y = "", x = "", color="", title="Central Coast")+
    theme(axis.text.y=element_text(size=7),legend.position = "bottom")+
    facet_wrap(~CU_Name)
  nass <- ggplot(peo_nass,aes(x=Year,y=AKER))+
    geom_point()+geom_line()+geom_smooth(alpha=.5,se=FALSE)+
    theme_bw()+
    labs(y = "", x="Year",color="", title="Nass")+
    theme(axis.text.y=element_text(size=7),legend.position = "bottom")+
    facet_wrap(~CU_Name)
  
  grid.arrange(sk, cc, nass, heights = c(3.5, 3, 2), 
               top = textGrob("SEAK Exploitation Rate on Chinook Salmon by Conservation Unit (1985-2017)"),
               left = textGrob("SEAK Exploitation Rate", rot=90))
} 

