#### Wood Supply ####
# FFO Removals by OWN_PROGRAM

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

fia.nims.queries <- readRDS("SQL/FIA_NIMS_QUERIES.RDS")
eval.grps.expcurr <- readRDS("SQL/EVAL_GRPS_EXPCURR.RDS")
eval.grps.expremv <- readRDS("SQL/EVAL_GRPS_EXPREMV.RDS")

nims <- odbcConnect("fiadb01p")

#### FFO Forestland by OWN_PROGRAM Average ####
ffo.forestarea.own.program.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 2) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "NWOS.RESPONSE_VALUE", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("'&JOINS' JOINS", "'NWOS' JOINS", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
                                         "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)",
                                         "LEFT OUTER JOIN (SELECT NWOS_CERT.PLT_CN,",
                                         "CERT,",
                                         "COST,",
                                         "TAX,",
                                         "CASE",
                                         "WHEN CERT = 1 OR",
                                         "COST = 1 OR",
                                         "TAX = 1 THEN 1",
                                         "WHEN CERT IN (0, 9) OR",
                                         "COST IN (0, 9) OR",
                                         "TAX IN (0, 9) THEN 0",
                                         "ELSE -1",
                                         "END AS RESPONSE_VALUE",
                                         "FROM (SELECT PLT_CN,",
                                         "RESPONSE_VALUE CERT",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD114'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_CERT", 
                                         "JOIN (SELECT PLT_CN,",
                                         "RESPONSE_VALUE COST",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD121'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_COST",
                                         "ON (NWOS_CERT.PLT_CN =",
                                         "NWOS_COST.PLT_CN)",
                                         "JOIN (SELECT PLT_CN,",
                                         "RESPONSE_VALUE TAX",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD116'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_TAX",
                                         "ON (NWOS_CERT.PLT_CN = NWOS_TAX.PLT_CN)) NWOS",
                                         "ON (NWOS.PLT_CN = PLOT.CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.forestarea.own.program.average.sql, "SQL/BIVARIATE/FFO_FORESTAREA_OWN_PROGRAM_AVERAGE.sql")

ffo.forestarea.own.program.average <- as_tibble(sqlQuery(nims, ffo.forestarea.own.program.average.sql))
write_csv(ffo.forestarea.own.program.average, "DATA/BIVARIATE/FFO_FORESTAREA_OWN_PROGRAM_AVERAGE.csv")

#### FFO Removals by OWN_PROGRAM Total ####
ffo.removals.own.program.total.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "NWOS.RESPONSE_VALUE", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND coalesce(COND.OWNCD, PCOND.OWNCD) = 45", SQL_STMT),
         SQL_STMT = gsub("'&JOINS' JOINS", "'NWOS' JOINS", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN (SELECT NWOS_CERT.PLT_CN,",
                                         "CERT,",
                                         "COST,",
                                         "TAX,",
                                         "CASE",
                                         "WHEN CERT = 1 OR",
                                         "COST = 1 OR",
                                         "TAX = 1 THEN 1",
                                         "WHEN CERT IN (0, 9) OR",
                                         "COST IN (0, 9) OR",
                                         "TAX IN (0, 9) THEN 0",
                                         "ELSE -1",
                                         "END AS RESPONSE_VALUE",
                                         "FROM (SELECT PLT_CN,",
                                         "RESPONSE_VALUE CERT",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD114'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_CERT", 
                                         "JOIN (SELECT PLT_CN,",
                                         "RESPONSE_VALUE COST",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD121'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_COST",
                                         "ON (NWOS_CERT.PLT_CN =",
                                         "NWOS_COST.PLT_CN)",
                                         "JOIN (SELECT PLT_CN,",
                                         "RESPONSE_VALUE TAX",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD116'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_TAX",
                                         "ON (NWOS_CERT.PLT_CN = NWOS_TAX.PLT_CN)) NWOS",
                                         "ON (NWOS.PLT_CN = PLOT.CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.own.program.total.sql, "SQL/BIVARIATE/FFO_REMOVALS_OWN_PROGRAM_TOTAL.sql")

ffo.removals.own.program.total <- as_tibble(sqlQuery(nims, ffo.removals.own.program.total.sql))
write_csv(ffo.removals.own.program.total, "DATA/BIVARIATE/FFO_REMOVALS_OWN_PROGRAM_TOTAL.csv")

#### FFO Removals by OWN_PROGRAM Average ####
ffo.removals.own.program.average.sql <- fia.nims.queries %>% 
  filter(ATTRIBUTE_NBR == 237) %>% 
  mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
         SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
         SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
         # SQL_STMT = gsub("&EVAL_GRP", "252018", SQL_STMT), # For testing
         SQL_STMT = gsub("&GRP_BY_ATTRIB", "NWOS.RESPONSE_VALUE", SQL_STMT),
         SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = 45 AND PCOND.OWNCD = 45", SQL_STMT),
         SQL_STMT = gsub("'&JOINS' JOINS", "'NWOS' JOINS", SQL_STMT),
         SQL_STMT = gsub("&JOINS", paste("LEFT OUTER JOIN (SELECT NWOS_CERT.PLT_CN,",
                                         "CERT,",
                                         "COST,",
                                         "TAX,",
                                         "CASE",
                                         "WHEN CERT = 1 OR",
                                         "COST = 1 OR",
                                         "TAX = 1 THEN 1",
                                         "WHEN CERT IN (0, 9) OR",
                                         "COST IN (0, 9) OR",
                                         "TAX IN (0, 9) THEN 0",
                                         "ELSE -1",
                                         "END AS RESPONSE_VALUE",
                                         "FROM (SELECT PLT_CN,",
                                         "RESPONSE_VALUE CERT",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD114'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_CERT", 
                                         "JOIN (SELECT PLT_CN,",
                                         "RESPONSE_VALUE COST",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD121'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_COST",
                                         "ON (NWOS_CERT.PLT_CN =",
                                         "NWOS_COST.PLT_CN)",
                                         "JOIN (SELECT PLT_CN,",
                                         "RESPONSE_VALUE TAX",
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
                                         "AND Q.RESPONSE_VALUE <> -1",
                                         "AND Q.METADATA_CN = 'QMD116'",
                                         "GROUP BY PLT_CN,",
                                         "RESPONSE_VALUE) NWOS_TAX",
                                         "ON (NWOS_CERT.PLT_CN = NWOS_TAX.PLT_CN)) NWOS",
                                         "ON (NWOS.PLT_CN = PLOT.CN)"), SQL_STMT)) %>%
  pull(SQL_STMT)
write_file(ffo.removals.own.program.average.sql, "SQL/BIVARIATE/FFO_REMOVALS_OWN_PROGRAM_AVERAGE.sql")

ffo.removals.own.program.average <- as_tibble(sqlQuery(nims, ffo.removals.own.program.average.sql))
write_csv(ffo.removals.own.program.average, "DATA/BIVARIATE/FFO_REMOVALS_OWN_PROGRAM_AVERAGE.csv")
