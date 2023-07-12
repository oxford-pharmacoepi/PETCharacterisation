## table 5 - use case
library(CodelistGenerator)
library(PatientProfiles)
library(Capr)
library(DrugUtilisation)
library(dplyr)
library(tidyr)
library(here)

antiepileptics_codes <- as.list(getDescendants(cdm, conceptId = 21604390)[1])
nsaids_codes <- as.list(getDescendants(cdm, conceptId = 21604303)[1])
antidepressants_codes <- as.list(getDescendants(cdm, conceptId = 21604686)[1])
obstr_resp_dis_rx_codes <- as.list(getDescendants(cdm, conceptId = 21603248)[1])

medications <- c(antiepileptics_codes,nsaids_codes,
                 antidepressants_codes,obstr_resp_dis_rx_codes)

names(medications) <- c("antiepileptics","nsaids",
                        "antidepressants","obstr_resp_dis_rx")

print(medications)

## adding the medications table to the existing cdm
cdm <- generateDrugUtilisationCohortSet(cdm, "medications", medications)

head(cdm$medications, n = 5)


######## condition codes


concept_sets <- list(
  anxiety = cs(descendants(441542)),
  depression = cs(descendants(444100)),
  hypertension = cs(descendants(316866)),
  DM = cs(descendants(201820)),
  epilepsy = cs(descendants(380378)),
  asthma = cs(descendants(317009)),
  renal_impairment = cs(descendants(4030518)),
  pre_eclampsia = cs(descendants(c(443700, 439393, 141084))),
  gestational_diabetes = cs(descendants(c(4024659, 43020791, 438480))),
  pregnancy_induced_hypertension = cs(descendants(4167493))
)


condition_cohort_template <- function(concept_set) {
  cohort(entry = entry(condition(concept_set), primaryCriteriaLimit = "All"),
         exit = exit(fixedExit("startDate", 0L)))
}


cohortsConditions <- list()

for (i in 1:length(concept_sets)) {
  cohortsConditions[[i]] <- condition_cohort_template(concept_sets[[i]])
}

# Lets add names to the list of cohorts
names(cohortsConditions) <- names(concept_sets)

cdm <- generateCohortSet(cdm, cohortsConditions, "cohort_conditions", overwrite = TRUE)

head(cdm$cohort_conditions, n = 5)


## Cohort intersection
# We can also see whether individuals had an exposure or condition in some time period relative to their cohort start date using the `flagCohortPresence` function.
# In this case we'll look at two time periods, before their cohort start date and between cohort start date and end date.


## make motherTable into a "generated cohort set"



cdm$pregnancy_cohort <- cdm$motherTable %>%
                                   select("person_id","pregnancy_start_date","pregnancy_end_date") %>%
                                   rename(cohort_start_date = pregnancy_start_date,
                                          cohort_end_date = pregnancy_end_date,
                                          subject_id = person_id) %>%
  mutate(cohort_definition_id = 1) %>% relocate(cohort_definition_id)



DBI::dbWriteTable(con, "pregnancy_cohort_set",
                  dplyr::tibble(cohort_definition_id = c(1),
                                cohort_name = c("pregnancy")), overwrite=T)

DBI::dbWriteTable(con, "pregnancy_cohort_count",
                  cdm$pregnancy_cohort %>%
                    group_by(cohort_definition_id) %>%
                    tally(name = "number_records") %>%
                    collect() %>%
                    inner_join(cdm$pregnancy_cohort %>%
                                 select(subject_id, cohort_definition_id) %>%
                                 distinct() %>%
                                 group_by(cohort_definition_id) %>%
                                 tally(name = "number_subjects") %>%
                                 collect(),
                               by = "cohort_definition_id"), overwrite =T)

cdm$pregnancy_cohort_set <- con %>% dplyr::tbl(CDMConnector::in_schema("main",
                                                                       "pregnancy_cohort_set"))
cdm$pregnancy_cohort_count <- con %>% dplyr::tbl(CDMConnector::in_schema("main",
                                                                         "pregnancy_cohort_count"))

cdm$pregnancy_cohort <- newGeneratedCohortSet(cdm$pregnancy_cohort,
                                              cohortSetRef = cdm$pregnancy_cohort_set,
                                              cohortCountRef = cdm$pregnancy_cohort_count
                                              )

#### addAge
age <- cdm$pregnancy_cohort %>%
  addAge(
    cdm = cdm,
    indexDate = "cohort_start_date"
  ) %>%
  summarise(mean_age = mean(age),
            sd_age = sd(age))


#### get the intersection
############## MEDICATIONS -----------------------------------------------
##### before pregnancy

medication_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "medications",
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "{cohort_name}_{window_name}"
  ) %>% collect()


antidepressants_before_pregnancy <- medication_before_pregnancy %>%
  summarise(antidepressants_before = sum(antidepressants_minf_to_m1))

antiepileptics_before_pregnancy <- medication_before_pregnancy %>%
  summarise(antiepileptics_before = sum(antiepileptics_minf_to_m1))

nsaids_before_pregnancy <- medication_before_pregnancy %>%
  summarise(nsaids_before = sum(nsaids_minf_to_m1))

obstr_resp_dis_rx_before_pregnancy <- medication_before_pregnancy %>%
  summarise(obstr_resp_dis_rx_before = sum(obstr_resp_dis_rx_minf_to_m1))



############## CONDITIONS -----------------------------------------------
## before pregnancy

conditions_before_pregnancy <- cdm$pregnancy_cohort %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = "cohort_conditions",
    indexDate = "cohort_start_date",
    window = c(-Inf, -1),
    nameStyle = "{cohort_name}_{window_name}"
  ) %>% collect()


anxiety_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(anxiety_before = sum(anxiety_minf_to_m1))

asthma_before_pregnancy <- conditions_before_pregnancy%>%
  summarise(asthma_before = sum(asthma_minf_to_m1))

depression_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(depression_before = sum(depression_minf_to_m1))

DM_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(DM_before = sum(DM_minf_to_m1))

epilepsy_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(epilepsy_before = sum(epilepsy_minf_to_m1))

gestational_diabetes_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(gestational_diabetes_before = sum(gestational_diabetes_minf_to_m1))

hypertension_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(hypertension_before = sum(hypertension_minf_to_m1))

pre_eclampsia_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(pre_eclampsia_before = sum(pre_eclampsia_minf_to_m1))

pregnancy_induced_hypertension_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(pregnancy_induced_hypertension_before = sum(pregnancy_induced_hypertension_minf_to_m1))

renal_impairment_before_pregnancy <- conditions_before_pregnancy %>%
  summarise(renal_impairment_before = sum(renal_impairment_minf_to_m1))

#### combine all the information


table5 <- cbind(as_tibble(age),
                as_tibble(antidepressants_before_pregnancy),
                as_tibble(antiepileptics_before_pregnancy),
                as_tibble(nsaids_before_pregnancy),
                as_tibble(obstr_resp_dis_rx_before_pregnancy),
                as_tibble(anxiety_before_pregnancy),
                as_tibble(asthma_before_pregnancy),
                as_tibble(depression_before_pregnancy),
                as_tibble(DM_before_pregnancy),
                as_tibble(epilepsy_before_pregnancy),
                as_tibble(gestational_diabetes_before_pregnancy),
                as_tibble(hypertension_before_pregnancy),
                as_tibble(pre_eclampsia_before_pregnancy),
                as_tibble(pregnancy_induced_hypertension_before_pregnancy),
                as_tibble(renal_impairment_before_pregnancy)
                ) %>% pivot_longer(everything())

write.csv(table5, file = here::here("table5.csv"))
