#### Wood Supply ####
# FFO Removals by STDORGCD

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by STDORGCD Average ####
ffo.forestarea.stdorgcd.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "PCOND.STDORGCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.stdorgcd.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_STDORGCD_AVERAGE.sql")

ffo.forestarea.stdorgcd.average <- as_tibble(sqlQuery(nims, ffo.forestarea.stdorgcd.average.sql))
write_csv(ffo.forestarea.stdorgcd.average, "DATA/BIVARIATE/FFO_FORESTAREA_STDORGCD_AVERAGE.csv")

#### FFO Removals by STDORGCD Total ####
ffo.removals.stdorgcd.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "PCOND.STDORGCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.stdorgcd.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_STDORGCD_TOTAL.sql")

ffo.removals.stdorgcd.total <- as_tibble(sqlQuery(nims, ffo.removals.stdorgcd.total.sql))
write_csv(ffo.removals.stdorgcd.total, "DATA/BIVARIATE/FFO_REMOVALS_STDORGCD_TOTAL.csv")

#### FFO Removals by STDORGCD Average ####
ffo.removals.stdorgcd.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "PCOND.STDORGCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.stdorgcd.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_STDORGCD_AVERAGE.sql")

ffo.removals.stdorgcd.average <- as_tibble(sqlQuery(nims, ffo.removals.stdorgcd.average.sql))
write_csv(ffo.removals.stdorgcd.average, "DATA/BIVARIATE/FFO_REMOVALS_STDORGCD_AVERAGE.csv")
