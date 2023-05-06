#### Wood Supply ####
# Removals by OWNCD

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### Get Forest Area by OWNCD Total ####
forestarea.owncd.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
         # SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(forestarea.owncd.total.sql, "SQL/OWNCD/FORESTAREA_OWNCD_TOTAL.sql")

forestarea.owncd.total <- as_tibble(sqlQuery(nims, forestarea.owncd.total.sql))
write_csv(forestarea.owncd.total, "DATA/OWNCD/FORESTAREA_OWNCD_TOTAL.csv")

#### Get Forest Area by OWNCD Average ####
forestarea.owncd.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "COND.OWNCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = PCOND.OWNCD", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(forestarea.owncd.average.sql, "SQL/OWNCD/FORESTAREA_OWNCD_AVERAGE.sql")

forestarea.owncd.average <- as_tibble(sqlQuery(nims, forestarea.owncd.average.sql))
write_csv(forestarea.owncd.average, "DATA/OWNCD/FORESTAREA_OWNCD_AVERAGE.csv")

#### Removals by OWNCD Total ####
removals.owncd.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "coalesce(COND.OWNCD, PCOND.OWNCD)", SQL_STMT),
         SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(removals.owncd.total.sql, "SQL/OWNCD/REMOVALS_OWNCD_TOTAL.sql")

removals.owncd.total <- as_tibble(sqlQuery(nims, removals.owncd.total.sql))
write_csv(removals.owncd.total, "DATA/OWNCD/REMOVALS_OWNCD_TOTAL.csv")

#### Removals by OWNCD Average ####
removals.owncd.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "COND.OWNCD", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = PCOND.OWNCD", SQL_STMT),
         SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(removals.owncd.average.sql, "SQL/REMOVALS_OWNCD_AVERAGE.sql")

removals.owncd.average <- as_tibble(sqlQuery(nims, removals.owncd.average.sql))
write_csv(removals.owncd.average, "DATA/OWNCD/REMOVALS_OWNCD_AVERAGE.csv")
