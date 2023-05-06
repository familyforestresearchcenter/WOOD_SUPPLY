#### Wood Supply ####
# FFO Removals by OWN_SIZE

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by OWN_SIZE Average ####
ffo.forestarea.own.size.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "NWOS.RESPONSE_VALUE", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("'&JOINS' JOINS", "'NWOS' JOINS", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)",
                                         "LEFT OUTER JOIN (SELECT PLT_CN,",
                                         "METADATA_CN,",
                                         "QUEST_NAME,",
                                         "CASE",
                                         "WHEN RESPONSE_VALUE >= 1 AND",
                                         "RESPONSE_VALUE < 10 THEN",
                                         "1",
                                         "WHEN RESPONSE_VALUE >= 10 AND",
                                         "RESPONSE_VALUE < 20 THEN",
                                         "10",
                                         "WHEN RESPONSE_VALUE >= 20 AND",
                                         "RESPONSE_VALUE < 50 THEN",
                                         "20",
                                         "WHEN RESPONSE_VALUE >= 50 AND",
                                         "RESPONSE_VALUE < 100 THEN",
                                         "50",
                                         "WHEN RESPONSE_VALUE >= 100 AND",
                                         "RESPONSE_VALUE < 200 THEN",
                                         "100",
                                         "WHEN RESPONSE_VALUE >= 200 AND",
                                         "RESPONSE_VALUE < 500 THEN",
                                         "200",
                                         "WHEN RESPONSE_VALUE >= 500 AND",
                                         "RESPONSE_VALUE < 1000 THEN",
                                         "500",
                                         "WHEN RESPONSE_VALUE >= 1000 AND",
                                         "RESPONSE_VALUE < 5000 THEN",
                                         "1000",
                                         "WHEN RESPONSE_VALUE >= 5000 THEN",
                                         "5000",
                                         "ELSE",
                                         "NULL",
                                         "END AS RESPONSE_VALUE",
                                         "FROM FS_NWOS.PLOT_OWNER PO,",
                                         "FS_NWOS.OWNER      O,",
                                         "FS_NWOS.SAMPLE     S,",
                                         "FS_NWOS.RESPONSE   R,",
                                         "FS_NWOS.QUEST      Q",
                                         "WHERE PO.OWNER_CN = O.CN",
                                         "AND S.OWNER_CN = O.CN",
                                         "AND R.SAMPLE_CN = S.CN",
                                         "AND Q.RESPONSE_CN = R.CN",
                                         "AND S.NWOS_STUDY = 'base'",
                                         "AND PO.ORIGIN = 'P2'",
                                         "AND O.OWNCD_NWOS = 45",
                                         "AND R.RESPONSE_CD = 1",
                                         "AND Q.METADATA_CN = 'QMD2') NWOS",
                                         "ON (NWOS.PLT_CN = PLOT.CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.own.size.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_OWN_SIZE_AVERAGE.sql")

ffo.forestarea.own.size.average <- as_tibble(sqlQuery(nims, ffo.forestarea.own.size.average.sql))
write_csv(ffo.forestarea.own.size.average, "DATA/BIVARIATE/FFO_FORESTAREA_OWN_SIZE_AVERAGE.csv")

#### FFO Removals by OWN_SIZE Total ####
ffo.removals.own.size.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "NWOS.RESPONSE_VALUE", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("'&JOINS' JOINS", "'NWOS' JOINS", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN (SELECT PLT_CN,",
                                         "METADATA_CN,",
                                         "QUEST_NAME,",
                                         "CASE",
                                         "WHEN RESPONSE_VALUE >= 1 AND",
                                         "RESPONSE_VALUE < 10 THEN",
                                         "1",
                                         "WHEN RESPONSE_VALUE >= 10 AND",
                                         "RESPONSE_VALUE < 20 THEN",
                                         "10",
                                         "WHEN RESPONSE_VALUE >= 20 AND",
                                         "RESPONSE_VALUE < 50 THEN",
                                         "20",
                                         "WHEN RESPONSE_VALUE >= 50 AND",
                                         "RESPONSE_VALUE < 100 THEN",
                                         "50",
                                         "WHEN RESPONSE_VALUE >= 100 AND",
                                         "RESPONSE_VALUE < 200 THEN",
                                         "100",
                                         "WHEN RESPONSE_VALUE >= 200 AND",
                                         "RESPONSE_VALUE < 500 THEN",
                                         "200",
                                         "WHEN RESPONSE_VALUE >= 500 AND",
                                         "RESPONSE_VALUE < 1000 THEN",
                                         "500",
                                         "WHEN RESPONSE_VALUE >= 1000 AND",
                                         "RESPONSE_VALUE < 5000 THEN",
                                         "1000",
                                         "WHEN RESPONSE_VALUE >= 5000 THEN",
                                         "5000",
                                         "ELSE",
                                         "NULL",
                                         "END AS RESPONSE_VALUE",
                                         "FROM FS_NWOS.PLOT_OWNER PO,",
                                         "FS_NWOS.OWNER      O,",
                                         "FS_NWOS.SAMPLE     S,",
                                         "FS_NWOS.RESPONSE   R,",
                                         "FS_NWOS.QUEST      Q",
                                         "WHERE PO.OWNER_CN = O.CN",
                                         "AND S.OWNER_CN = O.CN",
                                         "AND R.SAMPLE_CN = S.CN",
                                         "AND Q.RESPONSE_CN = R.CN",
                                         "AND S.NWOS_STUDY = 'base'",
                                         "AND PO.ORIGIN = 'P2'",
                                         "AND O.OWNCD_NWOS = 45",
                                         "AND R.RESPONSE_CD = 1",
                                         "AND Q.METADATA_CN = 'QMD2') NWOS",
                                         "ON (NWOS.PLT_CN = PLOT.CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.own.size.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_OWN_SIZE_TOTAL.sql")

ffo.removals.own.size.total <- as_tibble(sqlQuery(nims, ffo.removals.own.size.total.sql))
write_csv(ffo.removals.own.size.total, "DATA/BIVARIATE/FFO_REMOVALS_OWN_SIZE_TOTAL.csv")

#### FFO Removals by OWN_SIZE Average ####
ffo.removals.own.size.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "NWOS.RESPONSE_VALUE", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("'&JOINS' JOINS", "'NWOS' JOINS", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN (SELECT PLT_CN,",
                                         "METADATA_CN,",
                                         "QUEST_NAME,",
                                         "CASE",
                                         "WHEN RESPONSE_VALUE >= 1 AND",
                                         "RESPONSE_VALUE < 10 THEN",
                                         "1",
                                         "WHEN RESPONSE_VALUE >= 10 AND",
                                         "RESPONSE_VALUE < 20 THEN",
                                         "10",
                                         "WHEN RESPONSE_VALUE >= 20 AND",
                                         "RESPONSE_VALUE < 50 THEN",
                                         "20",
                                         "WHEN RESPONSE_VALUE >= 50 AND",
                                         "RESPONSE_VALUE < 100 THEN",
                                         "50",
                                         "WHEN RESPONSE_VALUE >= 100 AND",
                                         "RESPONSE_VALUE < 200 THEN",
                                         "100",
                                         "WHEN RESPONSE_VALUE >= 200 AND",
                                         "RESPONSE_VALUE < 500 THEN",
                                         "200",
                                         "WHEN RESPONSE_VALUE >= 500 AND",
                                         "RESPONSE_VALUE < 1000 THEN",
                                         "500",
                                         "WHEN RESPONSE_VALUE >= 1000 AND",
                                         "RESPONSE_VALUE < 5000 THEN",
                                         "1000",
                                         "WHEN RESPONSE_VALUE >= 5000 THEN",
                                         "5000",
                                         "ELSE",
                                         "NULL",
                                         "END AS RESPONSE_VALUE",
                                         "FROM FS_NWOS.PLOT_OWNER PO,",
                                         "FS_NWOS.OWNER      O,",
                                         "FS_NWOS.SAMPLE     S,",
                                         "FS_NWOS.RESPONSE   R,",
                                         "FS_NWOS.QUEST      Q",
                                         "WHERE PO.OWNER_CN = O.CN",
                                         "AND S.OWNER_CN = O.CN",
                                         "AND R.SAMPLE_CN = S.CN",
                                         "AND Q.RESPONSE_CN = R.CN",
                                         "AND S.NWOS_STUDY = 'base'",
                                         "AND PO.ORIGIN = 'P2'",
                                         "AND O.OWNCD_NWOS = 45",
                                         "AND R.RESPONSE_CD = 1",
                                         "AND Q.METADATA_CN = 'QMD2') NWOS",
                                         "ON (NWOS.PLT_CN = PLOT.CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.own.size.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_OWN_SIZE_AVERAGE.sql")

ffo.removals.own.size.average <- as_tibble(sqlQuery(nims, ffo.removals.own.size.average.sql))
write_csv(ffo.removals.own.size.average, "DATA/BIVARIATE/FFO_REMOVALS_OWN_SIZE_AVERAGE.csv")
