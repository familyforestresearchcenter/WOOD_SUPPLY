#### Wood Supply ####
# FFO Removals by BALIVE

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by BALIVE Average ####
ffo.forestarea.balive.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN PCOND.BALIVE >= 0 AND",
                                                 "PCOND.BALIVE < 43.56 THEN",
                                                 "0",
                                                 "WHEN PCOND.BALIVE >= 43.56 AND",
                                                 "PCOND.BALIVE < 87.12 THEN",
                                                 "1",
                                                 "WHEN PCOND.BALIVE >= 87.12 AND",
                                                 "PCOND.BALIVE < 130.68 THEN",
                                                 "2",
                                                 "WHEN PCOND.BALIVE >= 130.68 AND",
                                                 "PCOND.BALIVE < 174.24 THEN",
                                                 "3",
                                                 "WHEN PCOND.BALIVE >= 174.24 THEN",
                                                 "4",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.balive.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_BALIVE_AVERAGE.sql")

ffo.forestarea.balive.average <- as_tibble(sqlQuery(nims, ffo.forestarea.balive.average.sql))
write_csv(ffo.forestarea.balive.average, "DATA/BIVARIATE/FFO_FORESTAREA_BALIVE_AVERAGE.csv")

#### FFO Removals by BALIVE Total ####
ffo.removals.balive.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN PCOND.BALIVE >= 0 AND",
                                                 "PCOND.BALIVE < 43.56 THEN",
                                                 "0",
                                                 "WHEN PCOND.BALIVE >= 43.56 AND",
                                                 "PCOND.BALIVE < 87.12 THEN",
                                                 "1",
                                                 "WHEN PCOND.BALIVE >= 87.12 AND",
                                                 "PCOND.BALIVE < 130.68 THEN",
                                                 "2",
                                                 "WHEN PCOND.BALIVE >= 130.68 AND",
                                                 "PCOND.BALIVE < 174.24 THEN",
                                                 "3",
                                                 "WHEN PCOND.BALIVE >= 174.24 THEN",
                                                 "4",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.balive.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_BALIVE_TOTAL.sql")

ffo.removals.balive.total <- as_tibble(sqlQuery(nims, ffo.removals.balive.total.sql))
write_csv(ffo.removals.balive.total, "DATA/BIVARIATE/FFO_REMOVALS_BALIVE_TOTAL.csv")

#### FFO Removals by BALIVE Average ####
ffo.removals.balive.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN PCOND.BALIVE >= 0 AND",
                                                 "PCOND.BALIVE < 43.56 THEN",
                                                 "0",
                                                 "WHEN PCOND.BALIVE >= 43.56 AND",
                                                 "PCOND.BALIVE < 87.12 THEN",
                                                 "1",
                                                 "WHEN PCOND.BALIVE >= 87.12 AND",
                                                 "PCOND.BALIVE < 130.68 THEN",
                                                 "2",
                                                 "WHEN PCOND.BALIVE >= 130.68 AND",
                                                 "PCOND.BALIVE < 174.24 THEN",
                                                 "3",
                                                 "WHEN PCOND.BALIVE >= 174.24 THEN",
                                                 "4",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.balive.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_BALIVE_AVERAGE.sql")

ffo.removals.balive.average <- as_tibble(sqlQuery(nims, ffo.removals.balive.average.sql))
write_csv(ffo.removals.balive.average, "DATA/BIVARIATE/FFO_REMOVALS_BALIVE_AVERAGE.csv")
