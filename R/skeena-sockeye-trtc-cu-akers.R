
library(data.table)
library(tidyverse)
library(ggridges)

data<-fread("data-raw/sockeye-skeena-trtc data.csv")

data$`AK ER`<-data$`Total ER`-data$`CDN ER`

#line plot of ak ers for each skeena cu
ggplot(data,aes(x=Year,y=`AK ER`))+
  geom_point()+geom_line()+
  facet_wrap(CU_Name~.,ncol=3)+
  labs(y="Alaskan Exploitation Rate")+
  theme_bw()

#boxplot of ak ers for each skeena cu
ggplot(data,aes(x=`AK ER`,y=reorder(CU_Name,`AK ER`,median),fill=CU_Name))+
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(height=.1)+
  labs(y="Conservation Unit",x="Alaskan Exploitation Rate")+
  theme_bw()+
  guides(fill="none")

