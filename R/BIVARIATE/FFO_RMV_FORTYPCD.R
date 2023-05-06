#### Wood Supply ####
# FFO Removals by FORTYPCD

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by FORTYPCD Average ####
ffo.forestarea.fortypcd.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "PCOND.FORTYPCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.fortypcd.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_FORTYPCD_AVERAGE.sql")

ffo.forestarea.fortypcd.average <- as_tibble(sqlQuery(nims, ffo.forestarea.fortypcd.average.sql))
write_csv(ffo.forestarea.fortypcd.average, "DATA/BIVARIATE/FFO_FORESTAREA_FORTYPCD_AVERAGE.csv")

#### FFO Removals by FORTYPCD Total ####
ffo.removals.fortypcd.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "PCOND.FORTYPCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.fortypcd.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_FORTYPCD_TOTAL.sql")

ffo.removals.fortypcd.total <- as_tibble(sqlQuery(nims, ffo.removals.fortypcd.total.sql))
write_csv(ffo.removals.fortypcd.total, "DATA/BIVARIATE/FFO_REMOVALS_FORTYPCD_TOTAL.csv")

#### FFO Removals by FORTYPCD Average ####
ffo.removals.fortypcd.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "PCOND.FORTYPCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.fortypcd.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_FORTYPCD_AVERAGE.sql")

ffo.removals.fortypcd.average <- as_tibble(sqlQuery(nims, ffo.removals.fortypcd.average.sql))
write_csv(ffo.removals.fortypcd.average, "DATA/BIVARIATE/FFO_REMOVALS_FORTYPCD_AVERAGE.csv")
