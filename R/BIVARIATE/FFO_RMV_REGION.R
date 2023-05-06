#### Wood Supply ####
# FFO Removals by REGION

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by REGION Average ####
ffo.forestarea.region.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(9, 10, 17, 18, 19, 23, 24, 25, 26, 27, 29,",
                                                 "33, 34, 36, 39, 42, 44, 50, 54, 55)",
                                                 "THEN 1",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(1, 5, 12, 13, 21, 22, 28, 37, 40, 45, 47, 48, 51)",
                                                 "THEN 2",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(2, 4, 6, 8, 15, 16, 20, 30, 31, 32, 35, 38, 41, 46, 49, 53, 56)",
                                                 "THEN 3",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.region.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_REGION_AVERAGE.sql")

ffo.forestarea.region.average <- as_tibble(sqlQuery(nims, ffo.forestarea.region.average.sql))
write_csv(ffo.forestarea.region.average, "DATA/BIVARIATE/FFO_FORESTAREA_REGION_AVERAGE.csv")

#### FFO Removals by REGION Total ####
ffo.removals.region.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(9, 10, 17, 18, 19, 23, 24, 25, 26, 27, 29,",
                                                 "33, 34, 36, 39, 42, 44, 50, 54, 55)",
                                                 "THEN 1",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(1, 5, 12, 13, 21, 22, 28, 37, 40, 45, 47, 48, 51)",
                                                 "THEN 2",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(2, 4, 6, 8, 15, 16, 20, 30, 31, 32, 35, 38, 41, 46, 49, 53, 56)",
                                                 "THEN 3",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.region.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_REGION_TOTAL.sql")

ffo.removals.region.total <- as_tibble(sqlQuery(nims, ffo.removals.region.total.sql))
write_csv(ffo.removals.region.total, "DATA/BIVARIATE/FFO_REMOVALS_REGION_TOTAL.csv")

#### FFO Removals by REGION Average ####
ffo.removals.region.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(9, 10, 17, 18, 19, 23, 24, 25, 26, 27, 29,",
                                                 "33, 34, 36, 39, 42, 44, 50, 54, 55)",
                                                 "THEN 1",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(1, 5, 12, 13, 21, 22, 28, 37, 40, 45, 47, 48, 51)",
                                                 "THEN 2",
                                                 "WHEN PLOT.STATECD IN",
                                                 "(2, 4, 6, 8, 15, 16, 20, 30, 31, 32, 35, 38, 41, 46, 49, 53, 56)",
                                                 "THEN 3",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.region.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_REGION_AVERAGE.sql")

ffo.removals.region.average <- as_tibble(sqlQuery(nims, ffo.removals.region.average.sql))
write_csv(ffo.removals.region.average, "DATA/BIVARIATE/FFO_REMOVALS_REGION_AVERAGE.csv")
