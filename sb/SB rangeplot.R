regional_data <-
  read.csv("sb_rates_revised.csv")

regional_data$region <- 
  factor(regional_data$region,
         levels = regional_data$region)

regions <-
  data.frame(
    region = rep(regional_data$region,2),
    residence = c( rep("urban", ))
  )

regional_data %>%
  ggplot(aes(x = region)) +
  geom_linerange(
    aes(ymin = rural, ymax = urban, x = region),
    linewidth = 1, colour = "blue") +
  geom_point(aes(y = rural, size = 3), colour="green", show.legend = FALSE) +
  geom_point(aes(y = urban, size = 3), colour="darkorange", show.legend = FALSE) +
  coord_flip() +
  ylab("Stillbirth Rate (per 1000 Births)") +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  theme_bw(base_size = 16) +
  theme(axis.title.y = element_blank()) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "blue")
