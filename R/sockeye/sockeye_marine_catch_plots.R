# SEAK sockeye figure 9 - Canada (red), US (blue), and Total (black) marine commercial catch 
# in Southeast Alaska and DFO areas 3/4/5 for Skeena and Nass sockeye. 1982-2021

# Skeena Sockeye
make.sk.marine.catch.plot <- function(skeena.sx.trtc) {
  skeena.sx.trtc$USharvest = skeena.sx.trtc$`Total Harvest`-skeena.sx.trtc$`CDN Harvest`
  skeena.sx.trtc$USer = skeena.sx.trtc$USharvest/skeena.sx.trtc$`Total Run`
  skeena.sx.trtc$CDNer = skeena.sx.trtc$`CDN Harvest`/skeena.sx.trtc$`Total Run`
  skeena.sx.trtc$Totalharvest = skeena.sx.trtc$USharvest+skeena.sx.trtc$`CDN Harvest`
  skeena.sx.trtc$USp = skeena.sx.trtc$USharvest/skeena.sx.trtc$`Total Harvest`
  skeena.sx.trtc$CDNp = skeena.sx.trtc$`CDN Harvest`/skeena.sx.trtc$`Total Harvest`
  
  skeena_catch <- skeena.sx.trtc %>%
  select(StatArea, Year, USharvest, `CDN Harvest`) %>%
  pivot_longer(3:4,names_to="Fishery",values_to="Catch")
  
  skeena_catch_sum <- skeena_catch %>%
  group_by(Year, Fishery) %>%
  summarise(Catch = sum(Catch, na.rm = TRUE), .groups = "drop") %>%
  filter(Year >= 1960)
  
  skeena_us_cdn <- ggplot(
  skeena_catch_sum,
  aes(x = Year, y = Catch / 1000, color = Fishery)) +
  scale_x_continuous(breaks = seq(1960, 2030, by = 10))+
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  labs(x="Year",y="Catch (Thousands)",
       title=paste0("US and CDN Total Catch of Skeena sockeye"))+
  scale_colour_manual(values=c("red","blue")) 
  
  skeena_us_cdn
}

#Nass region
make.nass.marine.catch.plot <- function(nass.sx.trtc) {
  nass.sx.trtc$USharvest = nass.sx.trtc$`Total Harvest`-nass.sx.trtc$`CDN Harvest`
  nass.sx.trtc$USer = nass.sx.trtc$USharvest/nass.sx.trtc$`Total Run`
  nass.sx.trtc$CDNer = nass.sx.trtc$`CDN Harvest`/nass.sx.trtc$`Total Run`
  nass.sx.trtc$Totalharvest = nass.sx.trtc$USharvest+nass.sx.trtc$`CDN Harvest`
  nass.sx.trtc$USp = nass.sx.trtc$USharvest/nass.sx.trtc$`Total Harvest`
  nass.sx.trtc$CDNp = nass.sx.trtc$`CDN Harvest`/nass.sx.trtc$`Total Harvest`
  
  nass_catch <- nass.sx.trtc %>%
  select(StatArea, Year, USharvest, `CDN Harvest`) %>%
  pivot_longer(3:4,names_to="Fishery",values_to="Catch")
  
  nass_catch_sum <- nass_catch %>%
  group_by(Year, Fishery) %>%
  summarise(Catch = sum(Catch, na.rm = TRUE), .groups = "drop")
  
  nass_us_cdn <- ggplot(
  nass_catch_sum,
  aes(x = Year, y = Catch / 1000, color = Fishery)) +
  geom_line(alpha = 0.6) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  labs(x="Year",y="Catch (Thousands)",
               title=paste0("US and CDN Total Catch of Nass sockeye"))+
  scale_colour_manual(values=c("red","blue"))
  
  nass_us_cdn
}

