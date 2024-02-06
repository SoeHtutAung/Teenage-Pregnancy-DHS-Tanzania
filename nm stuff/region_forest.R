#Cleaning dataset
new <- read.csv("~/Downloads/nm_strata.csv")

new <- new %>%
  mutate(region = case_when(
    v024 == 1 ~ "Dodoma", 
    v024 == 2 ~ "Arusha", 
    v024 == 3 ~ "Kilimanjaro",
    v024 == 4 ~ "Tanga",
    v024 == 5 ~ "Morogoro",
    v024 == 6 ~ "Pwani",
    v024 == 7 ~ "Dar es Salaam",
    v024 == 8 ~ "Lindi",
    v024 == 9 ~ "Mtwara",
    v024 == 10 ~ "Ruvuma",
    v024 == 11 ~ "Iringa",
    v024 == 12 ~ "Mbeya",
    v024 == 13 ~ "Singida",
    v024 == 14 ~ "Tabora",
    v024 == 15 ~ "Rukwa",
    v024 == 16 ~ "Kigoma", 
    v024 == 17 ~ "Shinyanga", 
    v024 == 18 ~ "Kagera", 
    v024 == 19 ~ "Mwanza",
    v024 == 20 ~ "Mara", 
    v024 == 21 ~ "Manyara",
    v024 == 22 ~ "Njombe", 
    v024 == 23 ~ "Katavi", 
    v024 == 24 ~ "Simiyu",
    v024 == 25 ~ "Geita",
    v024 == 26 ~ "Songwe", 
    v024 == 51 ~ "Kaskazini Unguja",
    v024 == 52 ~ "Kusini Unguja",
    v024 == 53 ~ "Mjini Magharibi",
    v024 == 54 ~ "Kaskazini Pemba",
    v024 == 55 ~ "Kusini Pemba"
    ))

urban_nm <- new %>% select(v025, region, nm) %>% filter(v025 == 1)
rural_nm <- new %>% select(v024, v025, region, nm) %>% filter(v025 == 2)
cleaned_region <- merge(urban_nm, rural_nm, by = "region") %>% 
  select("v024", "region", "nm.x", "nm.y") %>% 
  dplyr::rename(urban = "nm.x", rural = "nm.y")



cleaned_region$v024 <- 
  factor(cleaned_region$v024,
         levels = cleaned_region$region)

regions <-
  data.frame(
    region = rep(regional_data$v024,2),
    residence = c( rep("urban", ))
  )

regional_data %>%
  ggplot(aes(x = region)) +
  geom_linerange(
    aes(ymin = rural, ymax = urban, x = region),
    linewidth = 1, colour = "blue") +
  geom_point(aes(y = rural, size = 3), colour="green", show.legend = FALSE) +
  geom_point(aes(y = urban, size = 3), colour="darkgreen", show.legend = FALSE) +
  coord_flip() +
  ylab("Stillbirth Rate (per 1000 Births)") +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(breaks = seq(0,120,20)) +
  theme_bw(base_size = 16) +
  theme(axis.title.y = element_blank()) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "blue")
