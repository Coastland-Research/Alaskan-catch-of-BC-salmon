library(data.table)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(dplyr)
library(tidyr)

##### Figure 15: 2023 Harvest by species, gear type, district, and statistical week #####

# D101-106 harvest by district, stat week, & gear type requested and provided by Sabrina Donnellan (ADFG)
# ensure data file received begins with 1985
# rename file to: "SEAK D101-D106 harvest by district by week"  (copy & paste name)

harvests<-fread("data-raw/SEAK D101-D106 harvest by district by week.csv")

sse.gear<-harvests%>%mutate_all(funs(replace(.,.=="Confidential",0)))%>%
  mutate_at(vars('Number Of Fish (estimated)'),funs(as.numeric))%>%
  filter(`District Number`%in%c(101,102,103,104,106),Year==2023)%>%    #filter for Districts 101-104,106 and current year
  group_by(`District Number`,`Stat Week`,`Species Name`,`Gear Name`)%>%summarise(total=sum(`Number Of Fish (estimated)`))

ignore<-c("Fish ladder/race way","Hand troll","Power gurdy troll")
ignore2<-c("Coho Salmon","Chum Salmon")

ps<-sse.gear%>%
  filter(!`Gear Name`%in%ignore)%>%
  filter(!`Species Name`%in%ignore2)

names(ps)[4]<-'Gear Type'

fig15 <- ggplot(ps,aes(x=`Stat Week`,y=total,color=`Gear Type`))+
  geom_point()+geom_line()+
  facet_grid(`Species Name`~`District Number`,scales="free_y")+
  theme_bw()+
  labs(x="Week",y="Catch (number of fish)",title="2023 SSEAK Catch by Statistical Week",
       subtitle="Chinook, Pink, and Sockeye Salmon in Districts 101-104,106")+   
  theme(legend.position = "bottom")

ggsave("fig15.png", fig15)

