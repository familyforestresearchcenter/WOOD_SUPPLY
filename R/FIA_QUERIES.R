#### Wood Supply ####
# Get FIA NIMS SQL Query Templates
# Generate Specific Queries

#### General Setup ####
rm(list=ls())
library(tidyverse)
library(RODBC)

nims <- odbcConnect("fiadb01p") # Requires 32-bit R

#### Get Full Query List - Estimates & SE ####
# Run SQL/FIA_NIMS_QUERIES.SQL in PL/SQL Developer (in Citrix)
fia.nims.queries <- read_csv("SQL/FIA_NIMS_QUERIES.csv",
                             col_types = "icffc") %>%
  mutate(SQL_STMT = toupper(SQL_STMT))
saveRDS(fia.nims.queries, "SQL/FIA_NIMS_QUERIES.RDS")

#### EVAL_GRPS ####
# Run SQL/FIA_NIMS_QUERIES.SQL in PL/SQL Developer (in Citrix)
eval.grps <- read_csv("SQL/FIA_NIMS_EVAL_GRPS_2018.csv",
                      col_types = "ccfnnf")
eval.grps.expcurr <- paste(eval.grps %>% 
                             filter(EVAL_TYP == "EXPCURR") %>% 
                             pull(EVAL_GRP), collapse = ", ")
eval.grps.expremv <- paste(eval.grps %>% 
                             filter(EVAL_TYP == "EXPREMV") %>% 
                             pull(EVAL_GRP), collapse = ", ")
saveRDS(eval.grps.expcurr, "SQL/EVAL_GRPS_EXPCURR.RDS")
saveRDS(eval.grps.expremv, "SQL/EVAL_GRPS_EXPREMV.RDS")


# #### Forest Area by OWNCD ####
# forestarea.owncd.total.sql <- fia.nims.queries %>% 
#   filter(ATTRIBUTE_NBR == 2) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          # SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
#          SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# # forestarea_owncd.sql
# write_file(forestarea.owncd.total.sql, "SQL/FORESTAREA_OWNCD_TOTAL.sql")
# 
# forestarea.owncd.average.sql <- fia.nims.queries %>% 
#   filter(ATTRIBUTE_NBR == 2) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          # SQL_STMT = gsub("&EVAL_GRP", eval.grps.expcurr, SQL_STMT),
#          SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "COND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = PCOND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", paste("JOIN FS_FIADB.SDS_COND_VW PCOND",
#                                          "ON (PLOT.PREV_PLT_CN = PCOND.PLT_CN)"), SQL_STMT)) %>%
#   pull(SQL_STMT)
# # forestarea_owncd.sql
# write_file(forestarea.owncd.average.sql, "SQL/FORESTAREA_OWNCD_AVERAGE.sql")
# 
# #### Removals by OWNCD ####
# removals.owncd.total.sql <- fia.nims.queries %>% 
#   filter(ATTRIBUTE_NBR == 237) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          # SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
#          SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "coalesce(COND.OWNCD, PCOND.OWNCD)", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# # removals.owncd.sql
# write_file(removals.owncd.total.sql, "SQL/REMOVALS_OWNCD_TOTAL.sql")
# 
# removals.owncd.average.sql <- fia.nims.queries %>% 
#   filter(ATTRIBUTE_NBR == 237) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          # SQL_STMT = gsub("&EVAL_GRP", eval.grps.expremv, SQL_STMT),
#          SQL_STMT = gsub("&EVAL_GRP", "252019", SQL_STMT), # For testing
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "COND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", "AND COND.OWNCD = PCOND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# # removals.owncd.sql
# write_file(removals.owncd.average.sql, "SQL/REMOVALS_OWNCD_AVERAGE.sql")
# 
# #### Forest Type ####
# foresttype.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 2) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "FORTYPCD||','||OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# foresttype.sql
# write_file(foresttype.sql, "SQL/FIA/GET/GET_FIA_FORESTTYPE.sql")
# 
# #### Stand Size ####
# standsize.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 2) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "STDSZCD||','||OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# standsize.sql
# write_file(standsize.sql, "SQL/FIA/GET/GET_FIA_STANDSIZE.sql")
# 
# #### Site Class ####
# siteclass.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 2) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "SITECLCD||','||OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# siteclass.sql
# write_file(siteclass.sql, "SQL/FIA/GET/GET_FIA_SITECLASS.sql")
# 
# #### Stand Origin ####
# standorigin.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 2) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "STDORGCD||','||OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# standorigin.sql
# write_file(standorigin.sql, "SQL/FIA/GET/GET_FIA_STANDORIGIN.sql")
# 
# #### Volume ####
# volume.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 14) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "SPCD||','||OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# volume.sql
# write_file(volume.sql, "SQL/FIA/GET/GET_FIA_VOLUME.sql")
# 
# #### GRM ####
# growth.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 25) %>%
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "TREE.SPCD||','||PCOND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", "AND PCOND.OWNCD = COND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# growth.sql
# write_file(growth.sql, "SQL/FIA/GET/GET_FIA_GROWTH.sql")
# 
# removals.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 39) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND",
#                          "FS_FIADB.SDS_COND_VW",
#                          SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "TREE.SPCD||','||PCOND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", "AND PCOND.OWNCD = COND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# removals.sql
# write_file(removals.sql, "SQL/FIA/GET/GET_FIA_REMOVALS.sql")
# 
# mortality.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 31) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND",
#                          "FS_FIADB.SDS_COND_VW",
#                          SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "TREE.SPCD||','||PCOND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", "AND PCOND.OWNCD = COND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# mortality.sql
# write_file(mortality.sql, "SQL/FIA/GET/GET_FIA_MORTALITY.sql")
# 
# #### Disturbance ####
# disturbance.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 2) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "DSTRBCD1||','||OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# write_file(disturbance.sql, "SQL/FIA/GET/GET_FIA_DISTURBANCE.sql")
# 
# #### Forest Area Loss ####
# forest.area.loss.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 137) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND PCOND", "FS_FIADB.SDS_COND_VW PCOND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "COND.COND_STATUS_CD||','||COND.PRESNFCD||','||COND.OWNCD||','||PCOND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", "AND PCOND.COND_STATUS_CD = 1", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# forest.area.loss.sql
# write_file(forest.area.loss.sql, "SQL/FIA/GET/GET_FIA_FOREST_AREA_LOSS.sql")
# 
# #### Forest Area Gain ####
# forest.area.gain.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 137) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND PCOND", "FS_FIADB.SDS_COND_VW PCOND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "PCOND.COND_STATUS_CD||','||PCOND.PRESNFCD||','||PCOND.OWNCD||','||COND.OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", "AND COND.COND_STATUS_CD = 1", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# forest.area.gain.sql
# write_file(forest.area.gain.sql, "SQL/FIA/GET/GET_FIA_FOREST_AREA_GAIN.sql")
# 
# #### Forest Area Change ####
# forest.area.change.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 137) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND COND", "FS_FIADB.SDS_COND_VW COND", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND PCOND", "FS_FIADB.SDS_COND_VW PCOND", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", 
#                          paste0("PCOND.COND_STATUS_CD||','||PCOND.PRESNFCD||','||PCOND.OWNCD||','||",
#                                 "COND.COND_STATUS_CD||','||COND.PRESNFCD||','||COND.OWNCD"), 
#                          SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# forest.area.change.sql
# write_file(forest.area.change.sql, "SQL/FIA/GET/GET_FIA_FOREST_AREA_CHANGE.sql")
# 
# #### Carbon - Above ground ####
# carbon.above.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 98) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# carbon.above.sql
# write_file(carbon.above.sql, "SQL/FIA/GET/GET_FIA_CARBON_ABOVE.sql")
# 
# #### Carbon - Below ground ####
# carbon.below.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 99) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# carbon.below.sql
# write_file(carbon.below.sql, "SQL/FIA/GET/GET_FIA_CARBON_BELOW.sql")
# 
# #### Carbon - Dead ####
# carbon.dead.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 100) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# carbon.dead.sql
# write_file(carbon.dead.sql, "SQL/FIA/GET/GET_FIA_CARBON_DEAD.sql")
# 
# #### Carbon - Litter ####
# carbon.litter.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 101) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# carbon.litter.sql
# write_file(carbon.litter.sql, "SQL/FIA/GET/GET_FIA_CARBON_LITTER.sql")
# 
# #### Carbon - Soil ####
# carbon.soil.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 102) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# carbon.soil.sql
# write_file(carbon.soil.sql, "SQL/FIA/GET/GET_FIA_CARBON_SOIL.sql")
# 
# #### Carbon - Total ####
# carbon.total.sql <- nims.se.queries %>% 
#   filter(ATTRIBUTE_NBR == 103) %>% 
#   mutate(SQL_STMT = gsub("&FIADB_SCHEMA", "FS_FIADB", SQL_STMT),
#          SQL_STMT = gsub("FS_FIADB.COND", "FS_FIADB.SDS_COND_VW", SQL_STMT),
#          SQL_STMT = gsub("&GRP_BY_ATTRIB", "OWNCD", SQL_STMT),
#          SQL_STMT = gsub("&FILTER", " ", SQL_STMT),
#          SQL_STMT = gsub("&JOINS", " ", SQL_STMT)) %>%
#   pull(SQL_STMT)
# carbon.total.sql
# write_file(carbon.total.sql, "SQL/FIA/GET/GET_FIA_CARBON_TOTAL.sql")
