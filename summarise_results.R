
source("summary_pet.R")


library(ggplot2)


# figure 1 ----
assign(paste0("figure_1_data_", db_name), readr::read_csv(
  here::here("results", paste0("figure_1_data_", db_name, ".csv")),
  show_col_types = FALSE))

figure_1_data <- bind_rows(mget(paste0("figure_1_data_", db_name)))

# combine results
# add dbs here with comma in between if there are several
# figure_1_data <- bind_rows(figure_1_data_db1, figure_1_data_db2, ... )


figure_1_data %>%
  ggplot(aes(pregnancy_year, n, colour=db)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  xlab("Year of pregnancy start date")+
  ylim(0, NA)+
  theme(legend.title = element_blank())

# save file, either use tempdir() or replace tempdir() with your desired folder path
ggsave("figure_1.png", path = tempdir(), dpi = 300)


# figure 2 ----
assign(paste0("figure_2_data_", db_name), readr::read_csv(
  here::here("results", paste0("figure_2_data_", db_name, ".csv")),
  show_col_types = FALSE))

figure_2_data <- bind_rows(mget(paste0("figure_2_data_", db_name)))

# combine results
# add dbs here with comma in between if there are several
# figure_2_data <- bind_rows(figure_2_data_db1, figure_2_data_db2, ... )



figure_2_data %>%
  ggplot(aes(pregnancy_year,percentage, fill=concept_name)) +
  geom_col(colour = "black") +
  theme_bw() +
  facet_grid(. ~ db) +
  xlab("Year of pregnacy start date") +
  theme(legend.title = element_blank())

# save file, either use tempdir() or replace tempdir() with your desired folder path
ggsave("figure_2.png", path = tempdir(), dpi = 300)



# figure 3 ----
assign(paste0("figure_3_data_", db_name), readr::read_csv(
  here::here("results", paste0("figure_3_data_", db_name, ".csv")),
  show_col_types = FALSE))

figure_3_data <- bind_rows(mget(paste0("figure_3_data_", db_name)))

# combine results
# add dbs here with comma in between if there are several
# figure_3_data <- bind_rows(figure_3_data_db1, figure_3_data_db2, ... )



figure_3_data %>%
  ggplot(aes(pregnancy_year,percentage, fill=concept_name)) +
  geom_col(colour = "black") +
  theme_bw() +
  facet_grid(. ~ db) +
  xlab("Year of pregnacy start date") +
  theme(legend.title = element_blank())

# save file, either use tempdir() or replace tempdir() with your desired folder path
ggsave("figure_3.png", path = tempdir(), dpi = 300)



# figure 4 ----
assign(paste0("figure_4_data_", db_name), readr::read_csv(
  here::here("results", paste0("figure_4_data_", db_name, ".csv")),
  show_col_types = FALSE))

figure_4_data <- bind_rows(mget(paste0("figure_4_data_", db_name)))

# combine results
# add dbs here with comma in between if there are several
# figure_4_data <- bind_rows(figure_4_data_db1, figure_4_data_db2, ... )



figure_4_data %>%
  collect() %>%
  ggplot()+
  geom_col(aes(week, n))+
  theme_bw() +
  facet_grid(concept_name ~ db , scales = "free_y")

