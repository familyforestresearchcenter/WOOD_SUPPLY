#### Wood Supply ####
# FFO Removals by MILL_SAW

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by MILL_SAW Average ####
ffo.forestarea.mill.saw.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN SAW_INDEX >= 0 AND",
                                                 "SAW_INDEX < 50 THEN",
                                                 "0",
                                                 "WHEN SAW_INDEX >= 50 AND",
                                                 "SAW_INDEX < 250 THEN",
                                                 "50",
                                                 "WHEN SAW_INDEX >= 250 AND",
                                                 "SAW_INDEX < 500 THEN",
                                                 "250",
                                                 "WHEN SAW_INDEX >= 500 AND",
                                                 "SAW_INDEX < 1000 THEN",
                                                 "500",
                                                 "WHEN SAW_INDEX >= 1000 THEN",
                                                 "1000",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)",
                                         "LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.MILL_INDEX MILL",
                                         "ON (PLOT.CN = MILL.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.mill.saw.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_MILL_SAW_AVERAGE.sql")

ffo.forestarea.mill.saw.average <- as_tibble(sqlQuery(nims, ffo.forestarea.mill.saw.average.sql))
write_csv(ffo.forestarea.mill.saw.average, "DATA/BIVARIATE/FFO_FORESTAREA_MILL_SAW_AVERAGE.csv")

#### FFO Removals by MILL_SAW Total ####
ffo.removals.mill.saw.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN SAW_INDEX >= 0 AND",
                                                 "SAW_INDEX < 50 THEN",
                                                 "0",
                                                 "WHEN SAW_INDEX >= 50 AND",
                                                 "SAW_INDEX < 250 THEN",
                                                 "50",
                                                 "WHEN SAW_INDEX >= 250 AND",
                                                 "SAW_INDEX < 500 THEN",
                                                 "250",
                                                 "WHEN SAW_INDEX >= 500 AND",
                                                 "SAW_INDEX < 1000 THEN",
                                                 "500",
                                                 "WHEN SAW_INDEX >= 1000 THEN",
                                                 "1000",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.MILL_INDEX MILL",
                                         "ON (PLOT.CN = MILL.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.mill.saw.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_MILL_SAW_TOTAL.sql")

ffo.removals.mill.saw.total <- as_tibble(sqlQuery(nims, ffo.removals.mill.saw.total.sql))
write_csv(ffo.removals.mill.saw.total, "DATA/BIVARIATE/FFO_REMOVALS_MILL_SAW_TOTAL.csv")

#### FFO Removals by MILL_SAW Average ####
ffo.removals.mill.saw.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE",
                                                 "WHEN SAW_INDEX >= 0 AND",
                                                 "SAW_INDEX < 50 THEN",
                                                 "0",
                                                 "WHEN SAW_INDEX >= 50 AND",
                                                 "SAW_INDEX < 250 THEN",
                                                 "50",
                                                 "WHEN SAW_INDEX >= 250 AND",
                                                 "SAW_INDEX < 500 THEN",
                                                 "250",
                                                 "WHEN SAW_INDEX >= 500 AND",
                                                 "SAW_INDEX < 1000 THEN",
                                                 "500",
                                                 "WHEN SAW_INDEX >= 1000 THEN",
                                                 "1000",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.MILL_INDEX MILL",
                                         "ON (PLOT.CN = MILL.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.mill.saw.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_MILL_SAW_AVERAGE.sql")

ffo.removals.mill.saw.average <- as_tibble(sqlQuery(nims, ffo.removals.mill.saw.average.sql))
write_csv(ffo.removals.mill.saw.average, "DATA/BIVARIATE/FFO_REMOVALS_MILL_SAW_AVERAGE.csv")
