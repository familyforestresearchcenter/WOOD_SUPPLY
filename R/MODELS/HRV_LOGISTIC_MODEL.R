#### WOOD SUPPLY ####
#### Model ####

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(car)
library(ResourceSelection)
library(performance)
library(effects)

#### Import Data ####
model.data <- readRDS("DATA/MODEL/WOOD_SUPPLY_MODEL_DATA.RDS") %>%
  mutate(MILL_SAW = MILL_SAW / 1e3)

#### Model ####
hrv.logistic <- glm(HRV ~ 
                      LC_FOREST + 
                      MILL_SAW +
                      OWN_AGE + OWN_HOME +
                      OWN_INC + OWN_MANAGE_ADVICE + OWN_MANAGE_PLAN + 
                      OWN_OBJ_TIM + OWN_PROGRAM + OWN_SIZE_LOG + 
                      PLOT_POP_DENSITY + PLOT_REGION +
                      PLOT_REMPER + STAND_BA + STAND_ORIGIN + STAND_TYPE,
                    data = model.data,
                    family = "binomial")
hrv.logistic
summary(hrv.logistic)

hrv.logistic.or <- exp(cbind(OR = coef(hrv.logistic), confint(hrv.logistic)))
tibble(VARIABLE = rownames(hrv.logistic.or),
          as.data.frame(hrv.logistic.or)) %>%
  rename(CI_LOWER = `2.5 %`, CI_UPPER = `97.5 %`) %>%
  mutate(across(c(OR, CI_LOWER, CI_UPPER), formatC, digits = 3, format = "f"))
# summary(glm(HRV ~ OWN_SIZE_LOG, data = model.data, family = "binomial"))

#### Diagnostics ####
# Goodness of fit
hoslem.test(hrv.logistic$y, fitted(hrv.logistic))
# Tjur
r2_tjur(hrv.logistic) # 0.1228311

# VIF
vif(hrv.logistic)

#### Effect Plots ####
# https://strengejacke.github.io/ggeffects/
plot(allEffects(hrv.logistic))


