#### WOOWD_SUPPLY ####
# Run SQL Scripts

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(RODBC)

#### Establish Connection ####
nims <- odbcConnect("fiadb01p")

# [1] "_ARCHIVE"                           "FORESTAREA_FFO.sql"                 "FORESTAREA_LC_AG.~sql"             
# [4] "FORESTAREA_LC_AG.sql"               "FORESTAREA_LC_FOREST.~sql"          "FORESTAREA_LC_FOREST.sql"          
# [7] "FORESTAREA_LC_URBAN.~sql"           "FORESTAREA_LC_URBAN.sql"            "FORESTAREA_MILL_PULP.~sql"         
# [10] "FORESTAREA_MILL_PULP.sql"           "FORESTAREA_MILL_SAW.~sql"           "FORESTAREA_MILL_SAW.sql"           
# [13] "FORESTAREA_OWN_AGE.sql"             "FORESTAREA_PLOT_REGION.sql"         "FORESTAREA_PLOT_ROAD.~sql"         
# [16] "FORESTAREA_PLOT_ROAD.sql"           "FORESTAREA_POP_DENS.~sql"           "FORESTAREA_POP_DENS.sql"           
# [19] "FORESTAREA_STAND_BA.sql"            "FORESTAREA_STAND_ORIGIN.~sql"       "FORESTAREA_STAND_ORIGIN.sql"       
# [22] "FORESTAREA_STAND_PRODUCTIVITY.~sql" "FORESTAREA_STAND_PRODUCTIVITY.sql"  "FORESTAREA_STAND_SIZE.~sql"        
# [25] "FORESTAREA_STAND_SIZE.sql"          "FORESTAREA_STAND_TYPE.~sql"         "FORESTAREA_STAND_TYPE.sql"         
# [28] "PLOTS_LAT_LON.~sql"                 "REMOVALS_EVALGRP.sql"               "REMOVALS_LC_AG.~sql"               
# [31] "REMOVALS_LC_AG.sql"                 "REMOVALS_LC_FOREST.~sql"            "REMOVALS_LC_FOREST.sql"            
# [34] "REMOVALS_LC_URBAN.~sql"             "REMOVALS_LC_URBAN.sql"              "REMOVALS_MANAGE_ADVICE.sql"        
# [37] "REMOVALS_MANAGE_PLAN.sql"           "REMOVALS_MILL_PULP.~sql"            "REMOVALS_MILL_PULP.sql"            
# [40] "REMOVALS_MILL_SAW.sql"              "REMOVALS_PLANTED.sql"               "REMOVALS_PLOT_ORIGIN.~sql"         
# [52] "REMOVALS_PLOT_PRODUCTIVITY.~sql"    "REMOVALS_PLOT_REGION.~sql"          "REMOVALS_PLOT_REGION.sql"          
# [55] "REMOVALS_PLOT_ROAD.~sql"            "REMOVALS_PLOT_ROAD.sql"             "REMOVALS_POP_DENS.~sql"            
# [58] "REMOVALS_POP_DENS.sql"              "REMOVALS_PROGRAM_CERT.sql"          "REMOVALS_PROGRAM_COST.sql"         
# [61] "REMOVALS_PROGRAM_TAX.sql"           "REMOVALS_REMPER.sql"                "REMOVALS_SITE.sql"                 
# [64] "REMOVALS_SIZE_STAND.sql"            "REMOVALS_STAND_BA.sql"              "REMOVALS_STAND_ORIGIN.~sql"        
# [67] "REMOVALS_STAND_ORIGIN.sql"          "REMOVALS_STAND_PRODUCTIVITY.~sql"   "REMOVALS_STAND_PRODUCTIVITY.sql"   
# [70] "REMOVALS_STAND_SIZE.~sql"           "REMOVALS_STAND_SIZE.sql"            "REMOVALS_STAND_TYPE.~sql"          
# [73] "REMOVALS_STAND_TYPE.sql"            "REMOVALS_TRT.sql"                   "REMOVALS_TYPE.sql"     

#### LC_AG ####
# fa.lc.ag.sql <- read_file("SQL/BIVARIATE/FORESTAREA_LC_AG.sql")
# fa.lc.ag <- as_tibble(sqlQuery(nims, fa.lc.ag.sql))
# write_csv(fa.lc.ag, "DATA/BIVARIATE/FORESTAREA_LC_AG.csv")
# rmv.lc.ag.sql <- read_file("SQL/BIVARIATE/REMOVALS_LC_AG.sql")
# rmv.lc.ag <- as_tibble(sqlQuery(nims, rmv.lc.ag.sql))
# write_csv(rmv.lc.ag, "DATA/BIVARIATE/REMOVALS_LC_AG.csv")

#### OWN_AGE ####
fa.own.age.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_AGE.sql")
fa.own.age <- as_tibble(sqlQuery(nims, fa.own.age.sql))
write_csv(fa.own.age, "DATA/BIVARIATE/FORESTAREA_OWN_AGE.csv")
# rmv.own.age.sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_AGE.sql")
# rmv.own.age <- as_tibble(sqlQuery(nims, rmv.own.age.sql))
# write_csv(rmv.own.age, "DATA/BIVARIATE/REMOVALS_OWN_AGE.csv")

#### OWN_EDU  ####
fa.own.edu.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_EDU.sql")
fa.own.edu <- as_tibble(sqlQuery(nims, fa.own.edu.sql))
write_csv(fa.own.edu, "DATA/BIVARIATE/FORESTAREA_OWN_EDU.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_EDU.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_EDU.csv")

#### OWN_FARM ####
fa.own.farm.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_FARM.sql")
fa.own.farm <- as_tibble(sqlQuery(nims, fa.own.farm.sql))
write_csv(fa.own.farm, "DATA/BIVARIATE/FORESTAREA_OWN_FARM.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_FARM.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_FARM.csv")

#### OWN_HOME ####
fa.own.home.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_HOME.sql")
fa.own.home <- as_tibble(sqlQuery(nims, fa.own.home.sql))
write_csv(fa.own.home, "DATA/BIVARIATE/FORESTAREA_OWN_HOME.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_HOME.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_HOME.csv")

### OWN_INC ####
fa.own.inc.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_INC.sql")
fa.own.inc <- as_tibble(sqlQuery(nims, fa.own.inc.sql))
write_csv(fa.own.inc, "DATA/BIVARIATE/FORESTAREA_OWN_INC.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_INC.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_INC.csv")

### OWN_MANAGE_ADVICE ####
fa.own.man.adv.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_MANAGE_ADVICE.sql")
fa.own.man.adv <- as_tibble(sqlQuery(nims, fa.own.man.adv.sql))
write_csv(fa.own.man.adv, "DATA/BIVARIATE/FORESTAREA_OWN_MANAGE_ADVICE.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_INC.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_INC.csv")

### OWN_MANAGE_ADVICE ####
fa.own.man.plan.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_MANAGE_PLAN.sql")
fa.own.man.plan <- as_tibble(sqlQuery(nims, fa.own.man.plan.sql))
write_csv(fa.own.man.plan, "DATA/BIVARIATE/FORESTAREA_OWN_MANAGE_PLAN.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_INC.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_INC.csv")

#### OWN_OBJ_NAT ####
fa.own.obj.nat.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_OBJ_NAT.sql")
fa.own.obj.nat <- as_tibble(sqlQuery(nims, fa.own.obj.nat.sql))
write_csv(fa.own.obj.nat, "DATA/BIVARIATE/FORESTAREA_OWN_OBJ_NAT.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_OBJ_NAT.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_OBJ_NAT.csv")

#### OWN_OBJ_TIM ####
fa.own.obj.tim.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_OBJ_TIM.sql")
fa.own.obj.tim <- as_tibble(sqlQuery(nims, fa.own.obj.tim.sql))
write_csv(fa.own.obj.tim, "DATA/BIVARIATE/FORESTAREA_OWN_OBJ_TIM.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_OBJ_TIM.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_OBJ_TIM.csv")

#### OWN_PROGRAM_ANY ####
fa.own.program.any.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_PROGRAM_ANY.sql")
fa.own.program.any <- as_tibble(sqlQuery(nims, fa.own.program.any.sql))
write_csv(fa.own.program.any, "DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_ANY.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_TENURE.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_TENURE.csv")

#### REMOVALS_OWN_PROGRAM_CERT.sql ####
fa.own.program.cert.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_PROGRAM_CERT.sql")
fa.own.program.cert <- as_tibble(sqlQuery(nims, fa.own.program.cert.sql))
write_csv(fa.own.program.cert, "DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_CERT.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_TENURE.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_TENURE.csv")

#### REMOVALS_OWN_PROGRAM_COST.sql ####
fa.own.program.cost.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_PROGRAM_COST.sql")
fa.own.program.cost <- as_tibble(sqlQuery(nims, fa.own.program.cost.sql))
write_csv(fa.own.program.cost, "DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_COST.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_TENURE.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_TENURE.csv")

#### REMOVALS_OWN_PROGRAM_TAX.sql ####
fa.own.program.tax.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_PROGRAM_TAX.sql")
fa.own.program.tax <- as_tibble(sqlQuery(nims, fa.own.program.tax.sql))
write_csv(fa.own.program.tax, "DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_TAX.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_TENURE.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_TENURE.csv")

#### REMOVALS_OWN_SIZE ####
fa.own.size.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_SIZE.sql")
fa.own.size <- as_tibble(sqlQuery(nims, fa.own.size.sql))
write_csv(fa.own.size, "DATA/BIVARIATE/FORESTAREA_OWN_SIZE.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_SIZE.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_SIZE.csv")

#### REMOVALS_OWN_TENURE.sql ####
fa.own.tenure.sql <- read_file("SQL/BIVARIATE/FORESTAREA_OWN_TENURE.sql")
fa.own.tenure <- as_tibble(sqlQuery(nims, fa.own.tenure.sql))
write_csv(fa.own.tenure, "DATA/BIVARIATE/FORESTAREA_OWN_TENURE.csv")
# rmv..sql <- read_file("SQL/BIVARIATE/REMOVALS_OWN_TENURE.sql")
# rmv. <- as_tibble(sqlQuery(nims, rmv..sql))
# write_csv(rmv., "DATA/BIVARIATE/REMOVALS_OWN_TENURE.csv")

#### PLOT_ECO ####
fa.plot.eco.sql <- read_file("SQL/BIVARIATE/FORESTAREA_PLOT_ECO.sql")
fa.plot.eco <- as_tibble(sqlQuery(nims, fa.plot.eco.sql))
fa.plot.eco <- fa.plot.eco %>%
  mutate(GROUP_BY_FIELD = gsub(" ", "", GROUP_BY_FIELD),
         GROUP_BY_FIELD = if_else(nchar(GROUP_BY_FIELD) == 5,
                                  substr(GROUP_BY_FIELD, 1, 3),
                                  substr(GROUP_BY_FIELD, 1, 4)))
write_csv(fa.plot.eco, "DATA/BIVARIATE/FORESTAREA_PLOT_ECO.csv")
rmv.plot.eco.sql <- read_file("SQL/BIVARIATE/REMOVALS_PLOT_ECO.sql")
rmv.plot.eco <- as_tibble(sqlQuery(nims, rmv.plot.eco.sql))
rmv.plot.eco <- rmv.plot.eco %>%
  mutate(GROUP_BY_FIELD = gsub(" ", "", GROUP_BY_FIELD),
         GROUP_BY_FIELD = if_else(nchar(GROUP_BY_FIELD) == 5,
                                  substr(GROUP_BY_FIELD, 1, 3),
                                  substr(GROUP_BY_FIELD, 1, 4)))
write_csv(rmv.plot.eco, "DATA/BIVARIATE/REMOVALS_PLOT_ECO.csv")

#### PLOT_REMPER ####
fa.plot.remper.sql <- read_file("SQL/BIVARIATE/FORESTAREA_PLOT_REMPER.sql")
fa.plot.remper <- as_tibble(sqlQuery(nims, fa.plot.remper.sql))
write_csv(fa.plot.remper, "DATA/BIVARIATE/FORESTAREA_PLOT_REMPER.csv")
rmv.plot.remper.sql <- read_file("SQL/BIVARIATE/REMOVALS_PLOT_REMPER.sql")
rmv.plot.remper <- as_tibble(sqlQuery(nims, rmv.plot.remper.sql))
write_csv(rmv.plot.remper, "DATA/BIVARIATE/REMOVALS_PLOT_REMPER.csv")
