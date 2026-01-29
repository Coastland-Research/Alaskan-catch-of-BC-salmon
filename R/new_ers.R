# older dataset:
sa.data<-fread("data-raw/NCConlystatareadata.csv")

sa.data$AKER<-sa.data$`Total ER`-sa.data$`CDN ER`
sa.data$AKcatch<-sa.data$`Total Harvest`-sa.data$`CDN Harvest`
# sp<-"Coho"

sa.data$p.US<-sa.data$AKER/(sa.data$AKER+sa.data$`CDN ER`)

sa.data<-sa.data%>%
  # filter(`Species Name`==sp)%>%
  select(Year,`StatArea/CU`,`CDN ER`,`AK ER`=AKER)%>%
  pivot_longer(3:4,names_to="region",values_to="er")

table_data <-fread("data-raw/table10data.csv") %>%
  mutate(AKER = as.numeric(AKER),
         `Stat Area/CU` = "3")

newdata <- fread("data-raw/catch_and_run_size_2026-01-26_FIXED.csv") %>%
  select(year, region, species_name, cu_name_pse, us_catch, cdn_catch, run_size)




