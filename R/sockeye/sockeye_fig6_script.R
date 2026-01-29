# SEAK sockeye catch figure 6 - total catches (all gears) by District by year

  
make.catch.bydistrict <- function(sk_annual){
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
