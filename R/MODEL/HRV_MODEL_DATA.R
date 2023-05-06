#### WOOD SUPPLY ####
#### Generate Model Data ####

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(measurements)

#### Functions ####
recode.factor <- function(x, 
                          yes.values = c(1), 
                          no.values = c(0, 9)) {
  factor(case_when(x %in% yes.values ~ 1,
                   x %in% no.values ~ 0),
         levels = c(0, 1), labels = c("No", "Yes")) }
recode.box2 <- function(x, 
                        yes.values = c(4, 5), 
                        no.values = c(1, 2, 3, 8)) {
  factor(case_when(x %in% yes.values ~ 1,
                   x %in% no.values ~ 0),
         levels = c(0, 1), labels = c("No", "Yes")) }

#### Import Data ####
plot <- read_csv("DATA/MODEL/MODEL_DATA_PLOTS_FFO.csv",
                 col_types = "____cn________") %>%
  separate_wider_delim(GROUP_BY_FIELD , delim = ",",
                       names = c("STATECD", "PLT_CN", "TRTCD1","REMPER", 
                                 "BALIVE", "STDORGCD", "FORTYPCD")) %>%
  rename(REMOVALS = ESTIMATE) %>%
  mutate(across(c(REMPER, BALIVE), as.numeric),
         across(c(STATECD, TRTCD1, STDORGCD, FORTYPCD), as.factor)) %>%
  select(PLT_CN, STATECD, REMOVALS, TRTCD1, REMPER, BALIVE, STDORGCD, FORTYPCD)
own <- read_csv("DATA/MODEL/MODEL_NWOS_DATA.csv",
                 col_types = "cfd") %>%
  pivot_wider(names_from = QUEST_NAME,
              values_from = RESPONSE_VALUE) %>%
  inner_join(read_csv("DATA/MODEL/MODEL_NWOSYR_DATA.csv",
                      col_types = "cd"), by = "PLT_CN") %>%
  select(PLT_CN, OWN1_AGE, HOME, INC_WOOD, ADVICE, OBJ_TIM, 
         CERT, COST_5YR, TAX, AC_WOOD)
mill <- read_csv("DATA/MILLS/PLOT_MILL_INDICES.csv",
                 col_types = "c__d_")
lc <- read_csv("DATA/LAND_COVER/PLOTS_LANDCOVER.csv",
               col_types = "c______d")
pop.dens <- read_csv("DATA/POP_DENS/POP_DENS.csv",
                     col_types = "cd")
state <- read_csv("DATA/REF/REF_STATE.csv",
                  col_types = "f_f____c____") %>%
  rename(STATECD = STATECD_FIA, STATE_ABB = STATE_FIA_ABB, 
         REGION = REGION_ALPHA) %>%
  distinct()

#### Combine Datasets ####
data.combined <- plot %>%
  inner_join(own, by = "PLT_CN") %>%
  left_join(mill, by = "PLT_CN") %>%
  left_join(lc, by = "PLT_CN") %>%
  left_join(pop.dens, by = "PLT_CN") %>%
  left_join(state, by = "STATECD")

#### Recode Variables ####
model.data <- data.combined %>%
  filter(!STATECD %in% c(53, 48)) %>%
  mutate(STATECD = factor(as.character(STATECD)),
         RMV = conv_unit(REMOVALS, "ft3", "m3"),
         HRV = factor(if_else(REMOVALS > 0 & TRTCD1 == 10, 1, 0), 
                      levels = c(0, 1), labels = c("No", "Yes")),
         LC_FOREST = FOREST_PROP,
         MILL_SAW = SAW_INDEX,
         OWN_AGE = if_else(OWN1_AGE >= 18, OWN1_AGE, as.numeric(NA)), 
         OWN_HOME  = recode.factor(HOME),
         OWN_INC = factor(case_when(INC_WOOD >= 0 & INC_WOOD < 5 ~ 0,
                                    INC_WOOD >= 0 ~ 1),
                          levels = c(0, 1), labels = c("No", "Yes")), 
         OWN_MAN_ADV = recode.factor(ADVICE),
         OWN_OBJ_TIM = recode.box2(OBJ_TIM),
         OWN_PROGRAM = if_else(CERT == 1 | COST_5YR == 1 | TAX == 1, 1,
                               if_else(CERT %in% c(0, 9) | 
                                         COST_5YR %in% c(0, 9) | 
                                         TAX %in% c(0, 9), 0,
                                       as.numeric(NA))),
         OWN_PROGRAM = recode.factor(OWN_PROGRAM),            
         OWN_SIZE = if_else(AC_WOOD >= 1, 
                            conv_unit(AC_WOOD, "acre", "hectare"), 
                            as.numeric(NA)),
         OWN_SIZE_LOG = log(OWN_SIZE),
         PLOT_REMPER = REMPER,
         POP_DENSITY = POP_DENS,
         REGION = factor(case_when(REGION %in% c("North") ~ "North",
                                        REGION %in% c("South") ~ "South",
                                        REGION %in% 
                                          c("Rocky Mountain", "Pacific Coast") ~ "West"),
                              levels = c("North", "South", "West")),
         STAND_BA = conv_multiunit(BALIVE, "ft2 / acre", "m2 / hectare"),
         STAND_BA_SQ = STAND_BA^2, 
         STAND_ORIGIN = recode.factor(STDORGCD),
         STAND_TYPE = factor(case_when(FORTYPCD %in% 100:399 ~ "Soft", 
                                       FORTYPCD %in% 400:998 ~ "Hard"),
                             levels = c("Hard", "Soft"))) %>%
  select(STATECD, RMV, HRV, 
         LC_FOREST, 
         MILL_SAW,
         OWN_AGE, OWN_HOME, OWN_INC, OWN_MAN_ADV, 
         OWN_OBJ_TIM, OWN_PROGRAM, OWN_SIZE, OWN_SIZE_LOG,
         PLOT_REMPER, POP_DENSITY, REGION,
         STAND_BA, STAND_BA_SQ, STAND_ORIGIN, STAND_TYPE) %>%
  filter(complete.cases(.))
summary(model.data)

#### Export Data ####
saveRDS(model.data, "DATA/MODEL/WOOD_SUPPLY_MODEL_DATA.RDS")
