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
  filter(OWN_SIZE >= 4.05)

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

#### LC_FOREST ####
summary(model.data$LC_FOREST)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "LC_FOREST",
                   SUMMARY = summary.numeric.text("LC_FOREST")))

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

#### OWN_HOME ####
summary.factor("OWN_HOME")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_HOME",
                   SUMMARY = summary.factor.text("OWN_HOME")))

#### OWN_INC ####
summary.factor("OWN_INC")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_INC",
                   SUMMARY = summary.factor.text("OWN_INC")))

#### OWN_MAN_ADV ####
summary.factor("OWN_MAN_ADV")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_MAN_ADV",
                   SUMMARY = summary.factor.text("OWN_MAN_ADV")))

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

#### OWN_SIZE ####
summary(model.data$OWN_SIZE)
summary(model.data$OWN_SIZE_LOG)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "OWN_SIZE",
                   SUMMARY = summary.numeric.text("OWN_SIZE")))

#### POP_DENSITY #####
summary(model.data$POP_DENSITY)
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "POP_DENSITY",
                   SUMMARY = summary.numeric.text("POP_DENSITY")))

#### REGION #####
summary.factor("REGION")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "REGION",
                   SUMMARY = summary.factor.text("REGION")))
model.data %>%
  count(REGION) %>%
  mutate(prop = n /sum(n))

#### PLOT_REMPER ####
summary(model.data$PLOT_REMPER)
mean(model.data$PLOT_REMPER)
sqrt(var(model.data$PLOT_REMPER))
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "PLOT_REMPER",
                   SUMMARY = summary.numeric.text("PLOT_REMPER")))

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

#### STAND_TYPE #### 
summary.factor("STAND_TYPE")
data.summary <- data.summary %>%
  bind_rows(tibble(VARIABLE = "STAND_TYPE",
                   SUMMARY = summary.factor.text("STAND_TYPE")))

#### Export ####
data.summary
write_csv(data.summary, "DATA/MODEL/WOOD_SUPPLY_MODEL_DATA_SUMMARY.csv")
