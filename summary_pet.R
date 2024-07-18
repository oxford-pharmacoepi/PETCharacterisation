library(dplyr)
library(lubridate)

motherTable <-cdm$motherTable %>%
  mutate(pregnancy_bmi_fix=ifelse(pregnancy_bmi == 0, NA,
                                  pregnancy_bmi),
         pregnancy_year = lubridate::year(pregnancy_start_date)
         ) %>%
  dplyr::filter(pregnancy_year %in% 2010:2020) %>% collect()

concept <-cdm$concept %>% collect()

# table one ----
table_one <- bind_rows(
  # n pregnancies
  motherTable %>% tally() %>%
    mutate(n=as.character(n)) %>%
    rename("Characteristic"="n") %>%
    mutate(missing = NA) %>%
    mutate(var="N Pregnancies") %>% collect() ,
  # n unique women
  motherTable %>%
    select("person_id") %>%
    distinct() %>%
    tally() %>%
    mutate(n=as.character(n)) %>%
    rename("Characteristic"="n") %>%
    mutate(missing = NA) %>%
    mutate(var="N women") %>% collect(),
  # Period of pregnancies captured
  motherTable %>% collect() %>%
    summarise(Characteristic= paste0(min(pregnancy_start_date, na.rm = TRUE),
                                     " to ",
                                     max(pregnancy_start_date, na.rm = TRUE)),
              missing = as.integer(sum(as.integer(is.na(pregnancy_start_date))))) %>%
    mutate(var="Period of pregnancies captured") %>% collect(),
  # Gestational length
  motherTable %>% collect() %>%
    summarise(Characteristic= paste0(median(gestational_length_in_day,na.rm=TRUE),
                                     " [",
                                     quantile(gestational_length_in_day, 0.25,na.rm=TRUE),
                                     " to ",
                                     quantile(gestational_length_in_day, 0.75,na.rm=TRUE),
                                     "]"),
              missing = as.integer(sum(as.integer(is.na(gestational_length_in_day))))) %>%
    mutate(var="Gestational length, median (IQR)") %>%
    collect(),
  # Pregnancy outcome
  motherTable %>% collect()  %>%
    summarise(missing = as.integer(sum(as.integer(is.na(pregnancy_outcome)))),
              Characteristic= as.character(nrow(motherTable) - as.integer(sum(as.integer(is.na(pregnancy_outcome)))))) %>%
    mutate(var="Pregnancy outcome, n (%)") %>% collect(),
  motherTable %>%
    group_by(pregnancy_outcome) %>%
    rename("concept_id"="pregnancy_outcome") %>%
    left_join(concept,
              by="concept_id") %>%
    select("concept_id", "concept_name") %>%
    rename("var"="concept_name") %>%
    group_by(var) %>%
    tally() %>%
    rename("Characteristic"="n") %>%
    mutate("Characteristic"=as.character(Characteristic)) %>%
    mutate(missing = NA) %>% collect(),
  # mode of delivery
  motherTable %>% collect()  %>%
    summarise(missing = as.integer(sum(as.integer(is.na(pregnancy_mode_delivery)))),
              Characteristic= as.character(nrow(motherTable) - as.integer(sum(as.integer(is.na(pregnancy_mode_delivery)))))) %>%
    mutate(var="Pregnancy mode of delivery, n (%)") %>% collect(),
  motherTable %>%
    group_by(pregnancy_mode_delivery) %>%
    rename("concept_id"="pregnancy_mode_delivery") %>%
    left_join(concept,
              by="concept_id") %>%
    select("concept_id", "concept_name") %>%
    rename("var"="concept_name") %>%
    group_by(var) %>%
    tally() %>%
    rename("Characteristic"="n") %>%
    mutate("Characteristic"=as.character(Characteristic)) %>%
    mutate(missing = NA) %>% collect(),
  # pregnancy single
  motherTable %>% collect()  %>%
    summarise(missing = as.integer(sum(as.integer(is.na(pregnancy_single)))),
              Characteristic= as.character(nrow(motherTable) - as.integer(sum(as.integer(is.na(pregnancy_single)))))) %>%
    mutate(var="Single Pregnancy n (%)") %>% collect(),
  motherTable %>%
    group_by(pregnancy_single) %>%
    rename("concept_id"="pregnancy_single") %>%
    left_join(concept,
              by="concept_id") %>%
    select("concept_id", "concept_name") %>%
    rename("var"="concept_name") %>%
    group_by(var) %>%
    tally() %>%
    rename("Characteristic"="n") %>%
    mutate("Characteristic"=as.character(Characteristic)) %>%
    mutate(missing = NA) %>% collect(),
  # marital status
  motherTable %>% collect()  %>%
    summarise(missing = as.integer(sum(as.integer(is.na(pregnancy_marital_status)))),
              Characteristic= as.character(nrow(motherTable) - as.integer(sum(as.integer(is.na(pregnancy_marital_status)))))) %>%
    mutate(var="Marital status, n (%)") %>% collect(),
  motherTable %>%
    group_by(pregnancy_marital_status) %>%
    rename("concept_id"="pregnancy_marital_status") %>%
    left_join(concept,
              by="concept_id") %>%
    select("concept_id", "concept_name") %>%
    rename("var"="concept_name") %>%
    group_by(var) %>%
    tally() %>%
    rename("Characteristic"="n") %>%
    mutate("Characteristic"=as.character(Characteristic)) %>%
    mutate(missing = NA) %>% collect(),
  # pregnancy_number_fetuses
  motherTable %>% collect() %>%
    summarise(Characteristic= paste0(median(pregnancy_number_fetuses, na.rm=TRUE),
                                     " [",
                                     min(pregnancy_number_fetuses, na.rm=TRUE), ", ",
                                     quantile(pregnancy_number_fetuses, 0.25, na.rm=TRUE),
                                     " to ",
                                     quantile(pregnancy_number_fetuses, 0.75, na.rm=TRUE), ", ",
                                     max(pregnancy_number_fetuses, na.rm=TRUE),
                                     "]"),
              missing = as.integer(sum(as.integer(is.na(pregnancy_number_fetuses)))))%>%
    mutate(var="Number of fetuses, median (min, IQR, max)") %>%
    collect(),
  # pregnancy_number_liveborn
  motherTable %>% collect() %>%
    summarise(Characteristic= paste0(median(pregnancy_number_liveborn, na.rm=TRUE),
                                     " [",
                                     min(pregnancy_number_liveborn, na.rm=TRUE), ", ",
                                     quantile(pregnancy_number_liveborn, 0.25, na.rm=TRUE),
                                     " to ",
                                     quantile(pregnancy_number_liveborn, 0.75, na.rm=TRUE), ", ",
                                     max(pregnancy_number_liveborn, na.rm=TRUE),
                                     "]"),
              missing = as.integer(sum(as.integer(is.na(pregnancy_number_liveborn))))) %>%
    mutate(var="Number of liveborn, median (min, IQR, max)")%>%
    collect(),
  # parity
  motherTable %>% collect() %>%
    summarise(missing = as.integer(sum(as.integer(is.na(prev_pregnancy_parity)))),
              Characteristic= as.character(nrow(motherTable) - as.integer(sum(as.integer(is.na(prev_pregnancy_parity)))))) %>%
    mutate(var="Previous parity, n (%)") %>% collect(),
  motherTable %>%
    group_by(prev_pregnancy_parity) %>%
    rename("concept_id"="prev_pregnancy_parity") %>%
    left_join(concept,
              by="concept_id") %>%
    select("concept_id", "concept_name") %>%
    rename("var"="concept_name") %>%
    group_by(var) %>%
    tally() %>%
    rename("Characteristic"="n") %>%
    mutate("Characteristic"=as.character(Characteristic)) %>%
    mutate(missing = NA) %>% collect(),
  # gravidity
  motherTable %>%  collect() %>%
    summarise(Characteristic= paste0(median(prev_pregnancy_gravidity, na.rm=TRUE),
                                     " [",
                                     min(prev_pregnancy_gravidity, na.rm=TRUE), ", ",
                                     quantile(prev_pregnancy_gravidity, 0.25, na.rm=TRUE),
                                     " to ",
                                     quantile(prev_pregnancy_gravidity, 0.75, na.rm=TRUE), ", ",
                                     max(prev_pregnancy_gravidity, na.rm=TRUE),
                                     "]"),
              missing = as.integer(sum(as.integer(is.na(prev_pregnancy_gravidity)))))%>%
    mutate(var="Previous gravidity, median (min, IQR, max)") %>%
    collect(),
  # BMI
  motherTable %>% collect() %>%
    summarise(Characteristic= paste0(round(mean(pregnancy_bmi_fix, na.rm = TRUE),1), " (",
                                     round(sd(pregnancy_bmi_fix, na.rm = TRUE), 1),
                                     ")"),
              missing = as.integer(sum(as.integer(is.na(pregnancy_bmi_fix))))) %>%
    mutate(var="BMI, mean (SD)") %>%
    collect()
) %>% collect() %>%
  select("var", "Characteristic", "missing") %>%
  mutate_at(c("missing"), ~ ifelse(is.na(missing),0,.)) %>%
  mutate(db=db_name)

table_one <- table_one %>% mutate_at(c("missing"), ~ ifelse(is.na(missing),0,.))



write.csv(table_one, here::here("results",paste0("table_one_",db_name ,".csv")))


# table two ----
if(db_name != "SIDIAP") {

  babyTable <-cdm$babyTable %>%
    mutate(birth_weight_fix=ifelse(birth_weight == 0, NA,
                                   birth_weight)) %>% collect() %>%
    right_join(select(motherTable,
                      pregnancy_id),multiple = "all",
               by = c("pregnancy_id"))


  table_two <- bind_rows(
    # n pregnancies
    babyTable %>%
      select("pregnancy_id") %>%
      distinct() %>%
      tally() %>%
      mutate(n=as.character(n)) %>%
      rename("Characteristic"="n") %>%
      mutate(missing = NA) %>%
      mutate(var="N Pregnancies") %>% collect(),
    # n unique fetus
    babyTable %>%
      select("fetus_id") %>%
      distinct() %>%
      tally() %>%
      mutate(n=as.character(n)) %>%
      rename("Characteristic"="n") %>%
      mutate(missing = NA) %>%
      mutate(var="N fetus") %>% collect(),
    # Birth outcome
    babyTable %>% collect() %>%
      summarise(missing = as.integer(sum(as.integer(is.na(birth_outcome)))),
                Characteristic= as.character(nrow(babyTable) - as.integer(sum(as.integer(is.na(birth_outcome)))))) %>%
      mutate(var="Birth outcome, n (%)") %>% collect(),
    babyTable %>%
      group_by(birth_outcome) %>%
      rename("concept_id"="birth_outcome") %>%
      left_join(concept,
                by="concept_id") %>%
      select("concept_id", "concept_name") %>%
      rename("var"="concept_name") %>%
      group_by(var) %>%
      tally() %>%
      rename("Characteristic"="n") %>%
      mutate("Characteristic"= as.character(Characteristic)) %>%
      mutate(missing = NA) %>% collect(),
    # Birth weight
    babyTable %>% collect() %>%
      summarise(Characteristic= paste0(median(birth_weight_fix, na.rm=TRUE),
                                       " [",
                                       quantile(birth_weight_fix, 0.25, na.rm=TRUE),
                                       " to ",
                                       quantile(birth_weight_fix, 0.75, na.rm=TRUE),
                                       "]"),
                missing = as.integer(sum(as.integer(is.na(birth_weight_fix))))) %>%
      mutate(var="Birth weight, median (IQR)") %>%
      collect(),
    # birth_con_malformation
    babyTable %>% collect() %>%
      summarise(missing = as.integer(sum(as.integer(is.na(birth_con_malformation)))),
                Characteristic= as.character(nrow(babyTable) - as.integer(sum(as.integer(is.na(birth_con_malformation)))))) %>%
      mutate(var="Congenital malformations, n (%)") %>% collect(),
    babyTable %>%
      group_by(birth_con_malformation) %>%
      rename("concept_id"="birth_con_malformation") %>%
      left_join(concept,
                by="concept_id") %>%
      select("concept_id", "concept_name") %>%
      rename("var"="concept_name") %>%
      group_by(var) %>%
      tally() %>%
      rename("Characteristic"="n") %>%
      mutate("Characteristic"=as.character(Characteristic)) %>%
      mutate(missing = NA) %>% collect(),
    # birth_sga
    babyTable %>% collect() %>%
      summarise(missing = as.integer(sum(as.integer(is.na(birth_sga)))),
                Characteristic= as.character(nrow(babyTable) - as.integer(sum(as.integer(is.na(birth_sga)))))) %>%
      mutate(var="Small for gestational age, n (%)") %>% collect(),
    babyTable %>%
      group_by(birth_sga) %>%
      rename("concept_id"="birth_sga") %>%
      left_join(concept,
                by="concept_id") %>%
      select("concept_id", "concept_name") %>%
      rename("var"="concept_name") %>%
      group_by(var) %>%
      tally() %>%
      rename("Characteristic"="n") %>%
      mutate("Characteristic"=as.character(Characteristic)) %>%
      mutate(missing = NA) %>% collect(),
    # birth_fgr
    babyTable %>% collect() %>%
      summarise(missing = as.integer(sum(as.integer(is.na(birth_fgr)))),
                Characteristic= as.character(nrow(babyTable) - as.integer(sum(as.integer(is.na(birth_fgr)))))) %>%
      mutate(var="Fetal growth restriction, n (%)") %>% collect(),
    babyTable %>%
      group_by(birth_fgr) %>%
      rename("concept_id"="birth_fgr") %>%
      left_join(concept,
                by="concept_id") %>%
      select("concept_id", "concept_name") %>%
      rename("var"="concept_name") %>%
      group_by(var) %>%
      tally() %>%
      rename("Characteristic"="n") %>%
      mutate("Characteristic"=as.character(Characteristic)) %>%
      mutate(missing = NA) %>% collect(),
    # birth_apgar
    babyTable %>% collect() %>%
      summarise(Characteristic= paste0(median(birth_apgar, na.rm=TRUE),
                                       " [",
                                       quantile(birth_apgar, 0.25, na.rm=TRUE),
                                       " to ",
                                       quantile(birth_apgar, 0.75, na.rm=TRUE),
                                       "]"),
                missing = as.integer(sum(as.integer(is.na(birth_apgar))))) %>%
      mutate(var="APGAR score, median (IQR)") %>%
      collect())  %>%
    select("var", "Characteristic", "missing") %>%
    mutate_at(c("missing"), ~ ifelse(is.na(missing),0,.))

  write.csv(table_two, here::here("results",paste0("table_two_",db_name ,".csv")))
}




# figure 1 data  -----
figure_1_data <- motherTable %>%
  select("person_id", "pregnancy_year") %>%
  group_by(pregnancy_year) %>%
  count() %>%
  mutate(db=db_name)

write.csv(figure_1_data,
          file = here::here("results",
                            paste0("figure_1_data_", db_name, ".csv")))




# figure 2 data  -----
figure_2_data <- motherTable %>%
  group_by(person_id, pregnancy_year, pregnancy_mode_delivery) %>%
  left_join(concept,
            by=c("pregnancy_mode_delivery"="concept_id")) %>%
  select("person_id", "pregnancy_year", "pregnancy_mode_delivery",
         "concept_name")  %>%
  group_by(pregnancy_year, pregnancy_mode_delivery, concept_name) %>%
  count() %>%
  collect()

figure_2_data<- figure_2_data %>%
  left_join(figure_2_data %>%
              group_by(pregnancy_year) %>%
              summarise(tot = sum(n))) %>%
  mutate(percentage=n/tot*100) %>%
  mutate(db=db_name)

write.csv(figure_2_data,
          file = here::here("results",
                            paste0("figure_2_data_", db_name, ".csv")))

# figure 3 data  -----
figure_3_data <- motherTable %>%
  group_by(person_id, pregnancy_year, pregnancy_outcome) %>%
  left_join(concept,
            by=c("pregnancy_outcome"="concept_id")) %>%
  select("person_id", "pregnancy_year", "pregnancy_outcome",
         "concept_name") %>% collect() %>%
  group_by(pregnancy_year, pregnancy_outcome, concept_name) %>%
  count() %>%
  collect()


figure_3_data<- figure_3_data %>%
  left_join(figure_3_data %>%
              group_by(pregnancy_year) %>%
              summarise(tot = sum(n))) %>%
  mutate(percentage=n/tot*100) %>%
  mutate(db=db_name)

figure_3_data<-figure_3_data %>%
  mutate(concept_name =ifelse(concept_name=="No matching concept",
                              "Unknown", concept_name))

write.csv(figure_3_data,
          file = here::here("results",
                            paste0("figure_3_data_", db_name, ".csv")))



# figure 4 data  -----
figure_4_data <- motherTable %>%
  filter(gestational_length_in_day<300) %>%
  filter(gestational_length_in_day!=0) %>%
  left_join(concept,
            by=c("pregnancy_outcome"="concept_id")) %>%
  select(gestational_length_in_day, pregnancy_outcome,
         concept_name,pregnancy_year) %>% collect()

weeks <- seq(0,320, 7)
figure_4_data$week <- NA

for(i in 1:(length(weeks)-1)){
  start <- weeks[[i]]
  end <- weeks[[i+1]]
  name<-i-1

  figure_4_data <- figure_4_data %>%
    mutate(week=ifelse(gestational_length_in_day >= start &
                         gestational_length_in_day < end  ,
                       name, week))
}

figure_4_data <- figure_4_data %>%
  group_by(week,concept_name) %>%
  count() %>%
  mutate(db=db_name)

write.csv(figure_4_data,
          file = here::here("results",
                            paste0("figure_4_data_", db_name, ".csv")))
