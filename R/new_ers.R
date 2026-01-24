# older dataset:
sa.data<-fread("data-raw/NCConlystatareadata.csv")

sa.data$AKER<-sa.data$`Total ER`-sa.data$`CDN ER`
sa.data$AKcatch<-sa.data$`Total Harvest`-sa.data$`CDN Harvest`
sp<-"Coho"

sa.data$p.US<-sa.data$AKER/(sa.data$AKER+sa.data$`CDN ER`)
sa.sp<-sa.data%>%filter(`Species Name`==sp)%>%select(Year,`StatArea/CU`,`CDN ER`,`AK ER`=AKER,p.US)%>%
  pivot_longer(3:5,names_to="region",values_to="er")

table_data <-fread("data-raw/table10data.csv") %>%
  mutate(AKER = as.numeric(AKER),
         `Stat Area/CU` = "3")




