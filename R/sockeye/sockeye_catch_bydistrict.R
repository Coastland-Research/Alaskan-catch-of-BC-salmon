# SEAK sockeye catch figure 6 - total catches (all gears) by District by year

make.catch.bydistrict <- function(sk_harv){
  
  sk_annual <- sk_harv %>%
    filter(`Species.Name` == "Sockeye Salmon") %>%
    filter(`Number.Of.Fish..estimated.` != "Confidential") %>%
    mutate(`Number.Of.Fish..estimated.` = as.numeric (`Number.Of.Fish..estimated.`)) %>%
    filter(`District.Number` %in% 101:106) %>%
    group_by(Year, `District.Number`) %>%
    summarise(total_catch = sum(`Number.Of.Fish..estimated.`, na.rm = TRUE),
              .groups = "drop")
  
  ggplot(sk_annual,
       aes(x = Year, y = total_catch)) +
    geom_point(size = 1.5) +
    geom_line() +
    geom_smooth(method = "loess", se = TRUE) +
    facet_wrap(~ `District.Number`, ncol = 2) +
    labs(
      title = "SSEAK Catch All Gear by District (101–106)",
      subtitle = "Sockeye Salmon (1985–2021)",
      x = "Year", y = "Total Catch") +
    theme_bw() +
    theme(strip.background = element_rect(fill = "grey90"))
}
