
source("summary_pet.R")


library(ggplot2)

# figure 1 ----
figure_1_data_mock <- readr::read_csv(
  here::here("results", paste0("figure_1_data_mock.csv")),
  show_col_types = FALSE)

# combine results
figure_1_data <- bind_rows(figure_1_data_mock) # remove mock and add dbs here with comma in between

figure_1_data %>%
  ggplot(aes(pregnancy_year, n, colour=db)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  xlab("Year of pregnancy end date")+
  ylim(0, NA)+
  theme(legend.title = element_blank())

# save file, either use tempdir() or replace tempdir() with your desired folder path
ggsave("figure_1.png", path = tempdir(), dpi = 300)

# figure 2 ----
figure_2_data_mock <- readr::read_csv(
  here::here("results", paste0("figure_2_data_mock.csv")),
  show_col_types = FALSE)

# combine results
figure_2_data <- bind_rows(figure_2_data_mock) # remove mock and add dbs here with comma in between

figure_2_data %>%
  ggplot(aes(pregnancy_year,percentage, colour=concept_name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_grid(. ~ db) +
  xlab("Year of pregnacy end date") +
  ylim(0, 100)+
  theme(legend.title = element_blank())

# save file, either use tempdir() or replace tempdir() with your desired folder path
ggsave("figure_2.png", path = tempdir(), dpi = 300)


# figure 3 ----
figure_3_data_mock <- readr::read_csv(
  here::here("results", paste0("figure_3_data_mock.csv")),
  show_col_types = FALSE)

# combine results
figure_3_data <- bind_rows(figure_3_data_mock) # remove mock and add dbs here with comma in between

figure_3_data %>%
  ggplot(aes(pregnancy_year,percentage, colour=concept_name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_grid(. ~ db) +
  xlab("Year of pregnacy end date") +
  ylim(0, 100)+
  theme(legend.title = element_blank())

# save file, either use tempdir() or replace tempdir() with your desired folder path
ggsave("figure_3.png", path = tempdir(), dpi = 300)


# figure 4 ----

figure_4_data_mock <-
  readr::read_csv(
    here::here("results", paste0("figure_4_data_mock.csv")),
    show_col_types = FALSE) %>%  collect()


# combine results
figure_4_data <- bind_rows(figure_4_data_mock) # remove mock and add dbs here with comma in between

figure_4_data %>%
  collect() %>%
  ggplot()+
  geom_col(aes(week, n))+
  theme_bw() +
  facet_grid(concept_name ~ db , scales = "free_y")
