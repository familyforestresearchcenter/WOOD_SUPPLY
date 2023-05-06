#### Wood Supply ####
# FFO Removals by LC_FOREST

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by LC_FOREST Average ####
ffo.forestarea.lc.forest.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE", 
                                                 "WHEN FOREST_PROP >= 0.00 AND", 
                                                 "FOREST_PROP < 0.25 THEN", 
                                                 "0",
                                                 "WHEN FOREST_PROP >= 0.25 AND",
                                                 "FOREST_PROP < 0.50 THEN",
                                                 "25",
                                                 "WHEN FOREST_PROP >= 0.50 AND",
                                                 "FOREST_PROP < 0.75 THEN",
                                                 "50",
                                                 "WHEN FOREST_PROP >= 0.75 AND",
                                                 "FOREST_PROP <= 1.00 THEN",
                                                 "75",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)",
                                         "LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.LAND_COVER LAND_COVER",
                                         "ON (PLOT.CN = LAND_COVER.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.lc.forest.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_LC_FOREST_AVERAGE.sql")

ffo.forestarea.lc.forest.average <- as_tibble(sqlQuery(nims, ffo.forestarea.lc.forest.average.sql))
write_csv(ffo.forestarea.lc.forest.average, "DATA/BIVARIATE/FFO_FORESTAREA_LC_FOREST_AVERAGE.csv")

#### FFO Removals by LC_FOREST Total ####
ffo.removals.lc.forest.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE", 
                                                 "WHEN FOREST_PROP >= 0.00 AND", 
                                                 "FOREST_PROP < 0.25 THEN", 
                                                 "0",
                                                 "WHEN FOREST_PROP >= 0.25 AND",
                                                 "FOREST_PROP < 0.50 THEN",
                                                 "25",
                                                 "WHEN FOREST_PROP >= 0.50 AND",
                                                 "FOREST_PROP < 0.75 THEN",
                                                 "50",
                                                 "WHEN FOREST_PROP >= 0.75 AND",
                                                 "FOREST_PROP <= 1.00 THEN",
                                                 "75",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.LAND_COVER LAND_COVER",
                         "ON (PLOT.CN = LAND_COVER.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.lc.forest.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_LC_FOREST_TOTAL.sql")

ffo.removals.lc.forest.total <- as_tibble(sqlQuery(nims, ffo.removals.lc.forest.total.sql))
write_csv(ffo.removals.lc.forest.total, "DATA/BIVARIATE/FFO_REMOVALS_LC_FOREST_TOTAL.csv")

#### FFO Removals by LC_FOREST Average ####
ffo.removals.lc.forest.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", paste("CASE", 
                                                 "WHEN FOREST_PROP >= 0.00 AND", 
                                                 "FOREST_PROP < 0.25 THEN", 
                                                 "0",
                                                 "WHEN FOREST_PROP >= 0.25 AND",
                                                 "FOREST_PROP < 0.50 THEN",
                                                 "25",
                                                 "WHEN FOREST_PROP >= 0.50 AND",
                                                 "FOREST_PROP < 0.75 THEN",
                                                 "50",
                                                 "WHEN FOREST_PROP >= 0.75 AND",
                                                 "FOREST_PROP <= 1.00 THEN",
                                                 "75",
                                                 "ELSE",
                                                 "-1",
                                                 "END"), SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN ANL_NRS_FIA_ANALYSIS.LAND_COVER LAND_COVER",
                         "ON (PLOT.CN = LAND_COVER.PLT_CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.lc.forest.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_LC_FOREST_AVERAGE.sql")

ffo.removals.lc.forest.average <- as_tibble(sqlQuery(nims, ffo.removals.lc.forest.average.sql))
write_csv(ffo.removals.lc.forest.average, "DATA/BIVARIATE/FFO_REMOVALS_LC_FOREST_AVERAGE.csv")
