# make new figure showing stock comps by CU of Fraser sockeye from yearly 2000-2021 data

make.stockcomp.fr <- function(stockgroups){
  #generate 20-colour palette
  n_max_colors <- 11
  base_colors <- brewer.pal(n_max_colors, "Spectral") # Use a base palette
  my_palette_function <- colorRampPalette(base_colors)
  colors_20 <- my_palette_function(20)
  
  stockgroups_long <- stockgroups %>%
  mutate(across("2000":"2021", as.numeric)) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  pivot_longer("2000":"2021",names_to="year",values_to="Catch") %>%
  rename("Conservation Unit" = `Row Labels`, "Year" = year) %>%
  mutate(Proportion = Catch/Total,
         Percentage = (Catch/Total*100)) 
  
  ggplot(stockgroups_long, aes(x = Year, y = Catch, fill = `Conservation Unit`))+
  geom_col(position = "stack")+
  scale_fill_manual(values = colors_20)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.25, hjust=0.25))+
  ggtitle("Stock composition of Fraser River sockeye catch")
}

  
  

