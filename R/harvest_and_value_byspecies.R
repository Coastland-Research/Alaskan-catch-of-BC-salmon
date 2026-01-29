# figure 3 SEAK harvest by species

make.harvest.byspecies<-function(catch){
  
  catch <- catch%>%
    mutate(Species = factor(`Species.Name`)) %>%
    filter(Area != "Bristol Bay")
  
  ggplot(catch, aes(x = Year, y = (`Number.Of.Fish..estimated.`/1000000), fill = Species)) +
  geom_col() +
  theme_minimal() +
    scale_fill_brewer(palette = "Spectral")+
  ylab("Number of fish (millions)") +
  theme(legend.position = "bottom")
}

# figure 5 - SEAK harvest and value broken down by species over years

make.harvest.value.byspecies <- function(value, catch){
  catch <- catch%>%
    mutate(Species = factor(`Species.Name`)) %>%
    filter(Area != "Bristol Bay")
  value <- value %>%
    mutate(Species = factor(`Species.Code`),
           Value = `Estimated.Exvessel.Value..Nominal.`) %>%
    filter(`Salmon.Area.Name` != "Bristol Bay")
  
  fig5_val <- ggplot(value, aes(x = Year, y = Value / 1e6, fill = Species)) +
    geom_col() +
    facet_wrap(~Species, nrow = 5, scales = "free_y") +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral")+
    ylab("US Dollars (millions)") +
    theme(legend.position = "none")
  
  fig5_harv <- ggplot(catch, aes(x = Year, y = `Number.Of.Fish..estimated.` / 1e6, fill = Species)) +
    geom_col() +
    facet_wrap(~Species, nrow = 5, scales = "free_y") +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral")+
    ylab("Number of fish (millions)") +
    theme(legend.position = "none")
  
  gridExtra::grid.arrange(fig5_harv, fig5_val, nrow = 1)
}
