
table_one <- bind_rows(
  # n pregnancies
cdm$motherTable %>% tally() %>%
  mutate(n=as.character(n)) %>%
  rename("Characteristic"="n") %>%
  mutate(missing = NA) %>%
  mutate(var="N Pregnancies") %>% collect() ,
# n unique women
cdm$motherTable %>%
  select("person_id") %>%
  distinct() %>%
  tally() %>%
  mutate(n=as.character(n)) %>%
  rename("Characteristic"="n") %>%
  mutate(missing = NA) %>%
  mutate(var="N women") %>% collect(),
# Period of pregnancies captured
cdm$motherTable %>%
  summarise(Characteristic= paste0(min(pregnancy_start_date, na.rm = TRUE),
                                   " to ",
                                   max(pregnancy_start_date, na.rm = TRUE)),
            missing = as.character(sum(as.integer(is.na(pregnancy_start_date))))) %>%
  mutate(var="Period of pregnancies captured") %>% collect(),
# Gestational length
cdm$motherTable %>%
  summarise(Characteristic= paste0(median(gestational_length_in_day),
                                   " [",
                                   quantile(gestational_length_in_day, 0.25),
                                   " to ",
                                   quantile(gestational_length_in_day, 0.75),
                                   "]"),
            missing = as.character(sum(as.integer(is.na(gestational_length_in_day))))) %>%
  mutate(var="Gestational length, median (IQR)") %>%
  collect(),
# Gestational length
cdm$motherTable %>%
  summarise(Characteristic= NA,
            missing = as.character(sum(as.integer(is.na(pregnancy_outcome))))) %>%
  mutate(var="Pregnancy outcome, n (%)") %>% collect(),
# Pregnancy outcome
cdm$motherTable %>%
  group_by(pregnancy_outcome) %>%
  rename("concept_id"="pregnancy_outcome") %>%
  left_join(cdm$concept,
            by="concept_id") %>%
  select("concept_id", "concept_name") %>%
  rename("var"="concept_name") %>%
  group_by(var) %>%
  tally() %>%
  rename("Characteristic"="n") %>%
  mutate("Characteristic"=as.character(Characteristic)) %>%
  mutate(missing = NA) %>% collect(),
# mode of delivery
cdm$motherTable %>%
  group_by(pregnancy_mode_delivery) %>%
  rename("concept_id"="pregnancy_mode_delivery") %>%
  left_join(cdm$concept,
            by="concept_id") %>%
  select("concept_id", "concept_name") %>%
  rename("var"="concept_name") %>%
  group_by(var) %>%
  tally() %>%
  rename("Characteristic"="n") %>%
  mutate("Characteristic"=as.character(Characteristic)) %>%
  mutate(missing = NA) %>% collect(),
# marital status
cdm$motherTable %>%
  group_by(pregnancy_marital_status) %>%
  rename("concept_id"="pregnancy_marital_status") %>%
  left_join(cdm$concept,
            by="concept_id") %>%
  select("concept_id", "concept_name") %>%
  rename("var"="concept_name") %>%
  group_by(var) %>%
  tally() %>%
  rename("Characteristic"="n") %>%
  mutate("Characteristic"=as.character(Characteristic)) %>%
  mutate(missing = NA) %>% collect(),
# pregnancy_number_fetuses
cdm$motherTable %>%
  summarise(Characteristic= paste0(median(pregnancy_number_fetuses),
                                   " [",
                                   min(pregnancy_number_fetuses, na.rm=T), ", ",
                                   quantile(pregnancy_number_fetuses, 0.25),
                                   " to ",
                                   quantile(pregnancy_number_fetuses, 0.75), ", ",
                                   max(pregnancy_number_fetuses, na.rm=T),
                                   "]"),
            missing = as.character(sum(as.integer(is.na(pregnancy_number_fetuses))))) %>%
  collect(),
# pregnancy_number_liveborn
cdm$motherTable %>%
  summarise(Characteristic= paste0(median(pregnancy_number_liveborn),
                                   " [",
                                   min(pregnancy_number_liveborn, na.rm=T), ", ",
                                   quantile(pregnancy_number_liveborn, 0.25),
                                   " to ",
                                   quantile(pregnancy_number_liveborn, 0.75), ", ",
                                   max(pregnancy_number_liveborn, na.rm=T),
                                   "]"),
            missing = as.character(sum(as.integer(is.na(pregnancy_number_liveborn))))) %>%
  collect(),
cdm$motherTable %>%
  group_by(prev_pregnancy_parity) %>%
  rename("concept_id"="prev_pregnancy_parity") %>%
  left_join(cdm$concept,
            by="concept_id") %>%
  select("concept_id", "concept_name") %>%
  rename("var"="concept_name") %>%
  group_by(var) %>%
  tally() %>%
  rename("Characteristic"="n") %>%
  mutate("Characteristic"=as.character(Characteristic)) %>%
  mutate(missing = NA) %>% collect(),
cdm$motherTable %>%
  summarise(Characteristic= paste0(median(prev_pregnancy_gravidity),
                                   " [",
                                   min(prev_pregnancy_gravidity, na.rm=T), ", ",
                                   quantile(prev_pregnancy_gravidity, 0.25),
                                   " to ",
                                   quantile(prev_pregnancy_gravidity, 0.75), ", ",
                                   max(prev_pregnancy_gravidity, na.rm=T),
                                   "]"),
            missing = as.character(sum(as.integer(is.na(prev_pregnancy_gravidity)))))%>%
  mutate(var="Pregnancy BMI, mean (SD)") %>%
  collect(),
cdm$motherTable %>%
  summarise(Characteristic= paste0(round(mean(pregnancy_bmi),1), " (",
                                   round(sd(pregnancy_bmi), 1),
                                   ")"),
            missing = as.character(sum(as.integer(is.na(pregnancy_bmi))))) %>%
  mutate(var="Pregnancy BMI, mean (SD)") %>%
  collect()) %>%
  select("var", "Characteristic", "missing")

write.csv(table_one, here::here("results","table_one.csv"))
