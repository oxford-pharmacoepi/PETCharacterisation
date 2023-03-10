# figure 1 ----
figure_1_data_sidiap <- readr::read_csv(
  here::here("results", paste0("figure_1_data_SIDIAP.csv")),
  show_col_types = FALSE) %>%
  filter(pregnancy_year > 2006 & pregnancy_year < 2022)

# combine results
figure_1_data <- bind_rows(figure_1_data_sidiap) # add dbs here

figure_1_data %>%
  ggplot(aes(pregnancy_year, n, colour=db)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year of pregnancy start date")+
  ylim(0, NA)+
  theme(legend.title = element_blank())
# ggsave("figure_1.png")

# figure 2 ----
figure_2_data_sidiap <- readr::read_csv(
  here::here("results", paste0("figure_2_data_SIDIAP.csv")),
  show_col_types = FALSE) %>%
  filter(pregnancy_year > 2006 & pregnancy_year < 2022)

# combine results
figure_2_data <- bind_rows(figure_2_data_sidiap) # add dbs here

figure_2_data %>%
  ggplot(aes(pregnancy_year, percentage, colour=concept_name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_grid(. ~ db) +
  xlab("Year of pregnacy start date") +
  ylim(0, 1)+
  theme(legend.title = element_blank())


# figure 3 ----
figure_3_data_sidiap <- readr::read_csv(
  here::here("results", paste0("figure_3_data_SIDIAP.csv")),
  show_col_types = FALSE) %>%
  filter(pregnancy_year > 2006 & pregnancy_year < 2022)

# combine results
figure_3_data <- bind_rows(figure_3_data_sidiap) # add dbs here

figure_3_data %>%
  ggplot(aes(pregnancy_year, percentage, colour=concept_name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_grid(. ~ db) +
  xlab("Year of pregnacy start date") +
  ylim(0, 1)+
  theme(legend.title = element_blank())




# figure 4 ----
figure_4_data_sidiap <- readr::read_csv(
  here::here("results", paste0("figure_4_data_SIDIAP.csv")),
  show_col_types = FALSE) %>%
  filter(pregnancy_year > 2006 & pregnancy_year < 2022)

# combine results
figure_4_data <- bind_rows(figure_4_data_sidiap) # add dbs here

figure_4_data %>%
  collect() %>%
  ggplot()+
  geom_col(aes(week, n))+
  theme_bw() +
  facet_grid(concept_name ~ db , scales = "free_y")
