library(CDMConnector)
library(dplyr)
library(ggplot2)
library(readr)

# Database name ----
db_name <- "mock"

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

# create cdm reference ----
# cdm <- cdm_From_Con(db, cdm_schema = cdm_schema)
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = cdm_database_schema,
                                  write_schema = results_database_schema)

# fill in table names for your database
cdm$motherTable <- tbl(db, sql("SELECT * FROM omop22t2_cmbd.pregnancy_episode"))
cdm$babyTable <- tbl(db, sql("SELECT * FROM omop22t2_cmbd. ...."))

#go to summary_pet.R and adapt the pregnancy year range (pregnancy_end_date counts for the classification of pregnancy year)
source("summarise_results.R")

