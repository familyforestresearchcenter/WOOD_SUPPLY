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
plot <- read_csv("DATA/MODEL/MODEL_PLOT_DATA.csv",
                 col_types = "cdffffddffff")
own <- read_csv("DATA/MODEL/MODEL_NWOS_DATA.csv",
                 col_types = "cfd") %>%
  pivot_wider(names_from = QUEST_NAME,
              values_from = RESPONSE_VALUE) %>%
  inner_join(read_csv("DATA/MODEL/MODEL_NWOSYR_DATA.csv",
                      col_types = "cd"), by = "PLT_CN")
mill <- read_csv("DATA/MILLS/PLOT_MILL_INDICES.csv",
                 col_types = "cdddd")
lc <- read_csv("DATA/LAND_COVER/PLOTS_LANDCOVER.csv",
               col_types = "c____ddd")
pop.dens <- read_csv("DATA/POP_DENS/POP_DENS.csv",
                     col_types = "cd")
state <- read_csv("DATA/REF/REF_STATE.csv",
                  col_types = "f_f____c____") %>%
  rename(STATECD = STATECD_FIA, STATE_ABB = STATE_FIA_ABB, 
         REGION = REGION_ALPHA) %>%
  distinct()
eco.region <- read_csv("DATA/REF/REF_ECO_REGION.csv",
                       col_types = "f_f")

#### Combine Datasets ####
model.data <- plot %>%
  inner_join(own, by = "PLT_CN") %>%
  left_join(mill, by = "PLT_CN") %>%
  left_join(lc, by = "PLT_CN") %>%
  left_join(pop.dens, by = "PLT_CN") %>%
  left_join(state, by = "STATECD") %>%
  left_join(eco.region, by = c("ECOSUBCD" = "SUBSECTION"))

#### Recode Variables ####
model.data <- model.data %>%
  filter(!STATECD %in% c(53, 48)) %>%
  mutate(STATECD = factor(as.character(STATECD)),
         RMV = conv_unit(REMOVALS, "ft3", "m3"),
         HRV = factor(if_else(REMOVALS > 0 & TRTCD1 == 10, 1, 0), 
                      levels = c(0, 1), labels = c("No", "Yes")),
         LC_AG = AG_PROP,
         LC_FOREST = FOREST_PROP,
         LC_URBAN = URBAN_PROP,
         MILL_PULP = PULP_INDEX,
         MILL_SAW = SAW_INDEX,
         OWN_AGE = if_else(OWN1_AGE >= 18, OWN1_AGE, as.numeric(NA)), 
         OWN_EDU = OWN1_EDU,
         OWN_EDU_BIN = factor(case_when(OWN1_EDU %in% c(1, 2, 3) ~ 0,
                                    OWN1_EDU %in% c(4, 5, 6) ~ 1),
                          levels = c(0, 1), labels = c("No", "Yes")),
         OWN_FARM  = recode.factor(FARM),
         OWN_HOME  = recode.factor(HOME),
         OWN_INC = INC_WOOD,  
         OWN_INC_BIN = factor(case_when(INC_WOOD >= 0 & INC_WOOD < 5 ~ 0,
                                    INC_WOOD >= 0 ~ 1),
                          levels = c(0, 1), labels = c("No", "Yes")), 
         OWN_MANAGE_ADVICE = recode.factor(ADVICE),
         OWN_MANAGE_PLAN = recode.factor(MAN_PLAN),
         OWN_PROGRAM = if_else(CERT == 1 | COST_5YR == 1 | TAX == 1, 1,
                               if_else(CERT %in% c(0, 9) | 
                                         COST_5YR %in% c(0, 9) | 
                                         TAX %in% c(0, 9), 0,
                                       as.numeric(NA))),
         OWN_PROGRAM = recode.factor(OWN_PROGRAM),            
         OWN_PROGRAM_CERT = recode.factor(CERT),
         OWN_PROGRAM_COST = recode.factor(COST_5YR),
         OWN_PROGRAM_TAX = recode.factor(TAX),
         OWN_OBJ_NAT = recode.box2(OBJ_NAT),
         OWN_OBJ_TIM = recode.box2(OBJ_TIM),
         OWN_SIZE = if_else(AC_WOOD >= 1, 
                            conv_unit(AC_WOOD, "acre", "hectare"), 
                            as.numeric(NA)),
         OWN_SIZE_LOG = log(OWN_SIZE),
         OWN_TENURE = if_else(!is.na(ACQ_YEAR) & NWOSYR - ACQ_YEAR <= 75, NWOSYR - ACQ_YEAR,
                              as.numeric(NA)),
         PLOT_ECO = PROVINCE,
         PLOT_POP_DENSITY = POP_DENS,
         PLOT_REGION = factor(case_when(REGION %in% c("North") ~ "North",
                                        REGION %in% c("South") ~ "South",
                                        REGION %in% 
                                          c("Rocky Mountain", "Pacific Coast") ~ "West"),
                              levels = c("North", "South", "West")),
         PLOT_ROAD = recode.factor(RDDISTCD, yes.values = c(5:9),
                                   no.values = c(1:4)),
         PLOT_REMPER = REMPER,
         STAND_BA = conv_multiunit(BALIVE, "ft2 / acre", "m2 / hectare"),
         STAND_BA_SQ = STAND_BA^2, 
         STAND_ORIGIN = recode.factor(STDORGCD),
         STAND_PLOT_PROD = recode.factor(SITECLCD, yes.values =  c(1, 2, 3, 4, 5),
                                           no.values = c(6, 7)),
         STAND_SIZE= factor(case_when(STDSZCD == 1 ~ "Large", 
                                      STDSZCD == 2 ~ "Medium",
                                      STDSZCD %in% c(3, 5) ~ "Small"),
                            levels = c("Non-stocked", "Small",
                                       "Medium", "Large")),
         STAND_TYPE = factor(case_when(FORTYPCD %in% 100:399 ~ "Soft", 
                                       FORTYPCD %in% 400:998 ~ "Hard"),
                             levels = c("Hard", "Soft"))) %>%
  select(RMV, HRV, 
         LC_AG, LC_FOREST, LC_URBAN,
         MILL_PULP, MILL_SAW,
         OWN_AGE, OWN_EDU, OWN_EDU_BIN, OWN_FARM, OWN_HOME,
         OWN_INC, OWN_INC_BIN,
         OWN_MANAGE_ADVICE, OWN_MANAGE_PLAN, OWN_PROGRAM, 
         OWN_PROGRAM_CERT, OWN_PROGRAM_COST, OWN_PROGRAM_TAX,
         OWN_OBJ_NAT, OWN_OBJ_TIM, OWN_SIZE, OWN_SIZE_LOG, OWN_TENURE,
         PLOT_ECO, PLOT_POP_DENSITY, PLOT_REGION, PLOT_ROAD, PLOT_REMPER,
         STAND_BA, STAND_BA_SQ, STAND_ORIGIN, STAND_PLOT_PROD, 
         STAND_SIZE, STAND_TYPE) %>%
  filter(complete.cases(.))
summary(model.data)

#### Export Data ####
saveRDS(model.data, "DATA/MODEL/WOOD_SUPPLY_MODEL_DATA.RDS")
