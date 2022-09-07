#### WOOD SUPPLY ####
#### Model Data Summary ####

#### General Set up ####
rm(list = ls())
library(tidyverse)

#### Functions ####
summary.factor <- function(x) {
  model.data %>% count(!!sym(x)) %>% mutate(prop = n / sum(n))}
summary.factor.text <- function(x = "OWN_PROGRAM_TAX") {
  x.sum <- summary.factor(x)
  paste0(paste0("Yes = ", round(x.sum[2, 3] * 100, 0), "%"),
         "; ",
         paste0("No = ", round(x.sum[1, 3] * 100, 0), "%"))}
summary.numeric.text <- function(x, min.value = NA) {
  dat <- model.data
  if(!is.na(min.value)) {dat <- dat %>% filter(!!sym(x) > min.value)}
  paste0(paste0(paste0("Q_", 0:4), " = ", 
                formatC(summary(dat %>%
                                  pull(!!sym(x)))[c(1:3, 5:6)],
                        1, format = "f")), collapse = "; ")}
#### Import Data ####
model.data <- readRDS("DATA/MODEL/WOOD_SUPPLY_MODEL_DATA.RDS") %>%
  mutate(OWN_TENURE_LOG = log(OWN_TENURE))

#### Summarize ####
names(model.data)
summary(model.data)

#### Removals ####
summary(model.data$RMV)
data.summary <- 
  tibble(VARIABLE = "RMV_ALL",
         SUMMARY = summary.numeric.text("RMV"))
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "RMV_GR0",
                   SUMMARY = summary.numeric.text("RMV", 0)))

#### HRV ####
summary.factor("HRV")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "HRV",
                   SUMMARY = summary.factor.text("HRV")))

#### LC_AG ####
summary(model.data$LC_AG)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "LC_AG",
                   SUMMARY = summary.numeric.text("LC_AG")))

#### LC_FOREST ####
summary(model.data$LC_FOREST)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "LC_FOREST",
                   SUMMARY = summary.numeric.text("LC_FOREST")))

#### LC_URBAN ####
summary(model.data$LC_URBAN)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "LC_URBAN",
                   SUMMARY = summary.numeric.text("LC_URBAN")))

#### MILL_PULP ####
summary(model.data$MILL_PULP)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "MILL_PULP",
         SUMMARY = summary.numeric.text("MILL_PULP")))

#### MILL_SAW ####
summary(model.data$MILL_SAW)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "MILL_SAW",
         SUMMARY = summary.numeric.text("MILL_SAW")))

#### OWN_AGE ####
summary(model.data$OWN_AGE)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_AGE",
                   SUMMARY = summary.numeric.text("OWN_AGE")))

#### OWN_EDU ####
summary.factor("OWN_EDU_BIN")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_EDU",
                   SUMMARY = summary.factor.text("OWN_EDU_BIN")))

#### OWN_FARM ####
summary.factor("OWN_FARM")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_FARM",
                   SUMMARY = summary.factor.text("OWN_FARM")))

#### OWN_HOME ####
summary.factor("OWN_HOME")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_HOME",
                   SUMMARY = summary.factor.text("OWN_HOME")))

#### OWN_INC ####
summary.factor("OWN_INC_BIN")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_INC",
                   SUMMARY = summary.factor.text("OWN_INC_BIN")))

#### OWN_MANAGE_ADVICE ####
summary.factor("OWN_MANAGE_ADVICE")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_MANAGE_ADVICE",
                   SUMMARY = summary.factor.text("OWN_MANAGE_ADVICE")))

#### OWN_MANAGE_PLAN ####
summary.factor("OWN_MANAGE_PLAN")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_MANAGE_PLAN",
                   SUMMARY = summary.factor.text("OWN_MANAGE_PLAN")))

#### OWN_OBJ_NAT ####
summary.factor("OWN_OBJ_NAT")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_OBJ_NAT",
                   SUMMARY = summary.factor.text("OWN_OBJ_NAT")))

#### OWN_OBJ_TIM ####
summary.factor("OWN_OBJ_TIM")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_OBJ_TIM",
                   SUMMARY = summary.factor.text("OWN_OBJ_TIM")))

#### OWN_PROGRAM  ####
summary.factor("OWN_PROGRAM")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_PROGRAM",
                   SUMMARY = summary.factor.text("OWN_PROGRAM")))

#### OWN_PROGRAM_CERT ####
summary.factor("OWN_PROGRAM_CERT")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_PROGRAM_CERT",
                   SUMMARY = summary.factor.text("OWN_PROGRAM_CERT")))

#### OWN_PROGRAM_COST  ####
summary.factor("OWN_PROGRAM_COST")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_PROGRAM_COST",
                   SUMMARY = summary.factor.text("OWN_PROGRAM_COST")))

#### OWN_PROGRAM_TAX ####
summary.factor("OWN_PROGRAM_TAX")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_PROGRAM_TAX",
                   SUMMARY = summary.factor.text("OWN_PROGRAM_TAX")))

#### OWN_SIZE ####
summary(model.data$OWN_SIZE)
summary(model.data$OWN_SIZE_LOG)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_SIZE",
                   SUMMARY = summary.numeric.text("OWN_SIZE")))

#### OWN_TENURE ####
summary(model.data$OWN_TENURE)
# hist(model.data$OWN_TENURE)
summary(model.data$OWN_TENURE_LOG)
# hist(model.data$OWN_TENURE_LOG)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_TENURE",
                   SUMMARY = summary.numeric.text("OWN_TENURE")))

#### PLOT_ECO ####
summary.factor("PLOT_ECO") %>%
  arrange(desc(prop))
summary.factor("PLOT_ECO") %>%
  slice_max(prop, n = 5) %>%
  summarize(sum(prop))
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "PLOT_ECO",
                   SUMMARY = summary.factor.text("PLOT_ECO")))

#### PLOT_POP_DENSITY #####
summary(model.data$PLOT_POP_DENSITY)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "PLOT_POP_DENSITY",
                   SUMMARY = summary.numeric.text("PLOT_POP_DENSITY")))

#### PLOT_REGION #####
summary.factor("PLOT_REGION")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "PLOT_REGION",
                   SUMMARY = summary.factor.text("PLOT_REGION")))

#### PLOT_REMPER ####
summary(model.data$PLOT_REMPER)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "PLOT_REMPER",
                   SUMMARY = summary.numeric.text("PLOT_REMPER")))

#### PLOT_ROAD #####
summary.factor("PLOT_ROAD")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "PLOT_ROAD",
                   SUMMARY = summary.factor.text("PLOT_ROAD")))

#### STAND_BA ####
summary(model.data$STAND_BA)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "STAND_BA",
                   SUMMARY = summary.numeric.text("STAND_BA")))

#### STAND_ORIGIN #### 
summary.factor("STAND_ORIGIN")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "STAND_ORIGIN",
                   SUMMARY = summary.factor.text("STAND_ORIGIN")))

#### STAND_PLOT_PROD #### 
summary.factor("STAND_PLOT_PROD")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "STAND_PROD",
                   SUMMARY = summary.factor.text("STAND_PLOT_PROD")))

#### STAND_SIZE #### 
summary.factor("STAND_SIZE")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "STAND_SIZE",
                   SUMMARY = summary.factor.text("STAND_SIZE")))

#### STAND_TYPE #### 
summary.factor("STAND_TYPE")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "STAND_TYPE",
                   SUMMARY = summary.factor.text("STAND_TYPE")))

#### Export ####
data.summary
write_csv(data.summary, "DATA/MODEL/WOOD_SUPPLY_MODEL_DATA_SUMMARY.csv")
