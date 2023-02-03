
#library(PETDiagnostics)
library(CDMConnector)
# Warning: package 'CDMConnector' was built under R version 4.2.2
library(dplyr)

# Database connection details -----
server_dbi<-Sys.getenv("SERVER_DBI")
port<-Sys.getenv("DB_PORT")
host<-Sys.getenv("DB_HOST")
user<-Sys.getenv("DB_USER")
password<-Sys.getenv("DB_PASSWORD")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password,
                bigint = "integer")

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-Sys.getenv("DB_CDM_SCHEMA")

# The name of the schema that contains the vocabularies
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-cdm_database_schema

# The name of the schema where results tables will be created
results_database_schema<-Sys.getenv("DB_WRITE_SCHEMA")

# Name of outcome table in the result table where the outcome cohorts will be stored
# Note, if there is an existing table in your results schema with the same names
# it will be overwritten
# Also note, this must be lower case
outcome_table<-"petdxresults"

# create cdm reference ----
# cdm <- cdm_From_Con(db, cdm_schema = cdm_schema)
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)
cdm$motherTable <- tbl(db, sql("SELECT * FROM omop22t2_cmbd.pregnancy_episode"))

cdm$motherTable%>% tally()

workTable <- cdm$motherTable %>% dplyr::collect()


workTable %>%
  summarise(across(where(is.numeric),
                   mean,
                   na.rm = TRUE))

workTable %>%
  summarise(across(where(is.numeric),
                   sd,
                   na.rm = TRUE))

#for mean and IQR
summary(workTable)


#counts and frequencies per category per variable
sapply(workTable,table)


# counts for missings and unknowns separately and combined
n_missing <- tibble::tibble(.rows=1)
prop_missing <- tibble::tibble(.rows=1)
n_unknown <- tibble::tibble(.rows=1)
prop_unknown <- tibble::tibble(.rows=1)
n_not_missing_only <- tibble::tibble(.rows=1)
prop_not_missing_only <- tibble::tibble(.rows=1)
n_not_missing_not_unknown <- tibble::tibble(.rows=1)
prop_not_missing_not_unknown <- tibble::tibble(.rows=1)


#loop through all columnnames
for (i in colnames(workTable)){



  n_missing[i] <- sum(as.integer(is.na(workTable[i]), na.rm = TRUE))
  prop_missing[i] <- round(n_missing[i]/nrow(workTable),3)*100

  n_not_missing_only[i] <- sum(as.integer(!is.na(workTable[i])), na.rm = TRUE)
  prop_not_missing_only[i] <- round(n_not_missing_only[i]/nrow(workTable),3)*100

  n_not_missing_not_unknown[i] <- sum(as.integer((!is.na(workTable[i]) & workTable[i] != 0), na.rm = TRUE))
  prop_not_missing_not_unknown[i] <- round(n_not_missing_not_unknown[i]/nrow(workTable),3)*100

  n_unknown[i] <- sum(workTable[i] == 0, na.rm=TRUE)
  prop_unknown[i] <- round(n_unknown[i]/nrow(workTable),3)*100

}

n_missing_long <-  tibble::as_tibble(reshape2::melt(n_missing, variable.names="variable",value.name = "count"))
prop_missing_long <- tibble::as_tibble(reshape2::melt(prop_missing, variable.names="variable",value.name = "Percentage"))

summMissings <- n_missing_long %>% dplyr::left_join(prop_missing_long, by = "variable")

n_not_missing_only_long <-  tibble::as_tibble(reshape2::melt(n_not_missing_only, variable.names="variable",value.name = "count"))
prop_not_missing_only_long <- tibble::as_tibble(reshape2::melt(prop_not_missing_only, variable.names="variable",value.name = "Percentage"))

summ_not_missing_only<- n_not_missing_only_long %>% dplyr::left_join(prop_not_missing_only_long, by = "variable")

n_not_missing_not_unknown_long <-  tibble::as_tibble(reshape2::melt(n_not_missing_not_unknown, variable.names="variable",value.name = "count"))
prop_not_missing_not_unknown_long <- tibble::as_tibble(reshape2::melt(prop_not_missing_not_unknown, variable.names="variable",value.name = "Percentage"))

summ_not_missing_not_unknown <- n_not_missing_not_unknown_long %>% dplyr::left_join(prop_not_missing_not_unknown_long, by = "variable")

n_unknown_long <-  tibble::as_tibble(reshape2::melt(n_unknown, variable.names="variable",value.name = "count"))
prop_unknown_long <- tibble::as_tibble(reshape2::melt(prop_unknown, variable.names="variable",value.name = "Percentage"))

summUnknown<- n_unknown_long %>% dplyr::left_join(prop_unknown_long, by = "variable")



cdm$motherTable

