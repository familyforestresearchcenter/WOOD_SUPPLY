#### Wood Supply ####
# Get Plot Data for Model

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### Plot Model Data ####
model.data.plot.ffo.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "12018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("(PLOT.STATECD || ',' ||",
                                                 "PLOT.CN || ',' ||",
                                                 "COND.TRTCD1  || ',' ||",
                                                 "PLOT.REMPER || ',' ||",
                                                 "PCOND.BALIVE || ',' ||",
                                                 "PCOND.STDORGCD || ',' ||",
                                                 "PCOND.FORTYPCD)"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", paste("AND COND.OWNCD = 45 AND PCOND.OWNCD = 45",
                                          "AND COND.CONDID = 1"), SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(model.data.plot.ffo.sql, "SQL/MODEL/MODEL_DATA_PLOTS_FFO.sql")

model.data.plot.ffo <- as_tibble(sqlQuery(nims, model.data.plot.ffo.sql))
write_csv(model.data.plot.ffo, "DATA/MODEL/MODEL_DATA_PLOTS_FFO.csv")
