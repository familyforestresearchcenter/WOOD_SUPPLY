#### Wood Supply ####
# FFO Removals by POP_DENS

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by POP_DENS Average ####
ffo.forestarea.pop.dens.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN POP_DENS >= 0 AND",
                                                 "POP_DENS < 5 THEN",
                                                 "0",
                                                 "WHEN POP_DENS >= 5 AND",
                                                 "POP_DENS < 10 THEN",
                                                 "5",
                                                 "WHEN POP_DENS >= 10 AND",
                                                 "POP_DENS < 20 THEN",
                                                 "10",
                                                 "WHEN POP_DENS >= 20 AND",
                                                 "POP_DENS < 50 THEN",
                                                 "20",
                                                 "WHEN POP_DENS >= 50 AND",
                                                 "POP_DENS < 150 THEN",
                                                 "50",
                                                 "WHEN POP_DENS >= 150 AND",
                                                 "POP_DENS < 400 THEN",
                                                 "150",
                                                 "WHEN POP_DENS >= 400 THEN",
                                                 "400",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)",
                                         "LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.POP_DENS POP_DENS",
                                         "ON (PLOT.CN = POP_DENS.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.pop.dens.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_POP_DENS_AVERAGE.sql")

ffo.forestarea.pop.dens.average <- as_tibble(sqlQuery(nims, ffo.forestarea.pop.dens.average.sql))
write_csv(ffo.forestarea.pop.dens.average, "DATA/BIVARIATE/FFO_FORESTAREA_POP_DENS_AVERAGE.csv")

#### FFO Removals by POP_DENS Total ####
ffo.removals.pop.dens.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN POP_DENS >= 0 AND",
                                                 "POP_DENS < 5 THEN",
                                                 "0",
                                                 "WHEN POP_DENS >= 5 AND",
                                                 "POP_DENS < 10 THEN",
                                                 "5",
                                                 "WHEN POP_DENS >= 10 AND",
                                                 "POP_DENS < 20 THEN",
                                                 "10",
                                                 "WHEN POP_DENS >= 20 AND",
                                                 "POP_DENS < 50 THEN",
                                                 "20",
                                                 "WHEN POP_DENS >= 50 AND",
                                                 "POP_DENS < 150 THEN",
                                                 "50",
                                                 "WHEN POP_DENS >= 150 AND",
                                                 "POP_DENS < 400 THEN",
                                                 "150",
                                                 "WHEN POP_DENS >= 400 THEN",
                                                 "400",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.POP_DENS POP_DENS",
                                         "ON (PLOT.CN = POP_DENS.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.pop.dens.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_POP_DENS_TOTAL.sql")

ffo.removals.pop.dens.total <- as_tibble(sqlQuery(nims, ffo.removals.pop.dens.total.sql))
write_csv(ffo.removals.pop.dens.total, "DATA/BIVARIATE/FFO_REMOVALS_POP_DENS_TOTAL.csv")

#### FFO Removals by POP_DENS Average ####
ffo.removals.pop.dens.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN POP_DENS >= 0 AND",
                                                 "POP_DENS < 5 THEN",
                                                 "0",
                                                 "WHEN POP_DENS >= 5 AND",
                                                 "POP_DENS < 10 THEN",
                                                 "5",
                                                 "WHEN POP_DENS >= 10 AND",
                                                 "POP_DENS < 20 THEN",
                                                 "10",
                                                 "WHEN POP_DENS >= 20 AND",
                                                 "POP_DENS < 50 THEN",
                                                 "20",
                                                 "WHEN POP_DENS >= 50 AND",
                                                 "POP_DENS < 150 THEN",
                                                 "50",
                                                 "WHEN POP_DENS >= 150 AND",
                                                 "POP_DENS < 400 THEN",
                                                 "150",
                                                 "WHEN POP_DENS >= 400 THEN",
                                                 "400",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.POP_DENS POP_DENS",
                                         "ON (PLOT.CN = POP_DENS.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.pop.dens.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_POP_DENS_AVERAGE.sql")

ffo.removals.pop.dens.average <- as_tibble(sqlQuery(nims, ffo.removals.pop.dens.average.sql))
write_csv(ffo.removals.pop.dens.average, "DATA/BIVARIATE/FFO_REMOVALS_POP_DENS_AVERAGE.csv")
