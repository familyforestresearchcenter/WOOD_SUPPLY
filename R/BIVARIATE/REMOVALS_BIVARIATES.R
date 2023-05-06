#### WOOD SUPPLY ####
## Bivariate Ananlyses of FIA Removals by ...

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(grid)
library(gridExtra)
library(measurements)
library(BSDA)

#### Functions ####
# https://www.researchgate.net/post/How-do-I-calculate-the-variance-of-the-ratio-of-two-independent-variables
var.ratio = function(x, var.x, y, var.y) {(x / y)^2 * ((var.x / x^2) + (var.y / y^2))}

area.x <- function(file, levels, values) {
  data <- read_csv(file,
                   col_types = "cciccnnnnnnn__")
  for(i in 1:length(levels)) {
    data <- data %>%
      mutate(GROUP_BY_FIELD = if_else(GROUP_BY_FIELD %in% unlist(values[i]), levels[i], GROUP_BY_FIELD)) }
  data %>%
    group_by(GROUP_BY_FIELD) %>%
    summarize(across(c("ESTIMATE", "VAR_OF_ESTIMATE"), sum), .groups = "drop") %>%
    mutate(ESTIMATE_TOT = sum(ESTIMATE), 
           VAR_OF_ESTIMATE_TOT = sum(VAR_OF_ESTIMATE)) %>%
    filter(!GROUP_BY_FIELD %in% c(-1, "NOT AVAILABLE")) %>%
    mutate(ESTIMATE_PROP = ESTIMATE / sum(ESTIMATE),
           VAR_OF_ESTIMATE_PROP = VAR_OF_ESTIMATE / sum(VAR_OF_ESTIMATE)) %>%
    mutate(ESTIMATE = ESTIMATE_PROP * ESTIMATE_TOT,
           VAR_OF_ESTIMATE = VAR_OF_ESTIMATE_PROP * VAR_OF_ESTIMATE_TOT) %>%
    mutate(HA = conv_unit(ESTIMATE, "acre", "hectare"),
           VAR_HA = (conv_unit(sqrt(VAR_OF_ESTIMATE), "acre", "hectare"))^2) %>%
    select(GROUP_BY_FIELD, HA, VAR_HA) }

rmv.x.avg <- function(file = "DATA/BIVARIATE/FFO_REMOVALS_OWN_HOME.csv", 
                      area.tot, levels = c("Yes", "No"), values = list(c("1"), c("0"))) {
  data <- read_csv(file, col_types = "cciccnnnnnnn__") 
  data.tot <- data %>%
    summarize(across(c("ESTIMATE", "VAR_OF_ESTIMATE"), sum)) %>%
    mutate(M3 = conv_unit(ESTIMATE, "ft3", "m3"),
           VAR_M3 = (conv_unit(sqrt(VAR_OF_ESTIMATE), "ft3", "m3"))^2)
  data <- data %>%
    filter(!GROUP_BY_FIELD %in% c("NOT AVAILABLE", "-2", "-1"))
  for(i in 1:length(levels)) {
    data <- data %>%
      mutate(GROUP_BY_FIELD = if_else(GROUP_BY_FIELD %in% unlist(values[i]), levels[i], GROUP_BY_FIELD)) }
  data %>%
    group_by(GROUP_BY_FIELD) %>%
    summarize(across(c("ESTIMATE", "VAR_OF_ESTIMATE", "NON_ZERO_PLOTS"), sum)) %>%
    mutate(ESTIMATE = conv_unit(ESTIMATE, "ft3", "m3"),
           VAR_OF_ESTIMATE = (conv_unit(sqrt(VAR_OF_ESTIMATE), "ft3", "m3"))^2) %>%
    rename(M3 = ESTIMATE, VAR_M3 = VAR_OF_ESTIMATE, N = NON_ZERO_PLOTS) %>%
    mutate(M3_PROP = M3 / sum(M3),
           VAR_M3_PROP = VAR_M3 / sum(VAR_M3),
           M3_2 = M3_PROP * data.tot$M3,
           VAR_M3_2 = VAR_M3_PROP * data.tot$VAR_M3) %>%
    select(GROUP_BY_FIELD, REMV_M3_YR = M3_2, VAR_REMV_M3_YR = VAR_M3_2, N) %>%
    left_join(area.tot, by = "GROUP_BY_FIELD") %>%
    mutate(REMV_M3_HA_YR = REMV_M3_YR / HA,
           VAR_REMV_M3_HA_YR = var.ratio(REMV_M3_YR, VAR_REMV_M3_YR, HA, VAR_HA),
           GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = levels, labels = levels)) }

rmv.x.tot <- function(file = "DATA/BIVARIATE/FFO_REMOVALS_OWN_HOME.csv", 
                      levels = c("Yes", "No"), values = list(c("1"), c("0"))) {
  data <- read_csv(file, col_types = "cciccnnnnnnn__") 
  data.tot <- data %>%
    summarize(across(c("ESTIMATE", "VAR_OF_ESTIMATE"), sum)) %>%
    mutate(M3 = conv_unit(ESTIMATE, "ft3", "m3"),
           VAR_M3 = (conv_unit(sqrt(VAR_OF_ESTIMATE), "ft3", "m3"))^2)
  data <- data %>%
    filter(!GROUP_BY_FIELD %in% c("NOT AVAILABLE", "-2", "-1"))
  for(i in 1:length(levels)) {
    data <- data %>%
      mutate(GROUP_BY_FIELD = if_else(GROUP_BY_FIELD %in% unlist(values[i]), levels[i], GROUP_BY_FIELD)) }
  data %>%
    group_by(GROUP_BY_FIELD) %>%
    summarize(across(c("ESTIMATE", "VAR_OF_ESTIMATE", "NON_ZERO_PLOTS"), sum)) %>%
    mutate(ESTIMATE = conv_unit(ESTIMATE, "ft3", "m3"),
           VAR_OF_ESTIMATE = (conv_unit(sqrt(VAR_OF_ESTIMATE), "ft3", "m3"))^2) %>%
    rename(M3 = ESTIMATE, VAR_M3 = VAR_OF_ESTIMATE, N = NON_ZERO_PLOTS) %>%
    mutate(M3_PROP = M3 / sum(M3),
           VAR_M3_PROP = VAR_M3 / sum(VAR_M3),
           M3_2 = M3_PROP * data.tot$M3,
           VAR_M3_2 = VAR_M3_PROP * data.tot$VAR_M3) %>%
    select(GROUP_BY_FIELD, REMV_M3_YR = M3_2, VAR_REMV_M3_YR = VAR_M3_2, N) }

rmv.t.test <- function(data, x.level = "Yes", y.level = "No") {
  data.x <- data %>% filter(GROUP_BY_FIELD == !!x.level)
  data.y <- data %>% filter(GROUP_BY_FIELD == !!y.level)
  tsum.test(mean.x = data.x$REMV_M3_HA_YR,
            s.x = sqrt(data.x$VAR_REMV_M3_HA_YR),
            n.x = data.x$N,
            mean.y = data.y$REMV_M3_HA_YR,
            s.y = sqrt(data.y$VAR_REMV_M3_HA_YR),
            n.y = data.y$N)$p.value}

rmv.effsize <- function(data, x.level = "Yes", y.level = "No") {
  data.x <- data %>% filter(GROUP_BY_FIELD == !!x.level)
  data.y <- data %>% filter(GROUP_BY_FIELD == !!y.level)
  mean.x = data.x$REMV_M3_HA_YR
  s.x = sqrt(data.x$VAR_REMV_M3_HA_YR)
  n.x = data.x$N
  mean.y = data.y$REMV_M3_HA_YR
  s.y = sqrt(data.y$VAR_REMV_M3_HA_YR)
  n.y = data.y$N
  s = sqrt((((n.x - 1) * s.x^2) + ((n.y - 1) * s.y^2)) / (n.x + n.y - 2))
  abs(mean.x - mean.y) / s}

rmv.plot <- function(data.tot, data.avg, title = NULL, x.lab = NULL) { 
  data <- data.tot %>%
    left_join(data.avg %>% 
                select(GROUP_BY_FIELD, REMV_M3_HA_YR, VAR_REMV_M3_HA_YR), 
              by = "GROUP_BY_FIELD")
  ggplot(data) +
    # geom_hline(yintercept = seq(50, 150, 50), size = 0.125) + 
    geom_hline(yintercept = seq(50, 150, 50), color = "blue", size = 0.125) +
    geom_hline(yintercept = seq(40, 150, 40), color = "red", size = 0.125) +
    geom_bar(aes(GROUP_BY_FIELD, REMV_M3_YR / 1e6), stat = "identity", 
             color = "blue", fill = "lightblue", size = 0.25) +
    geom_point(aes(GROUP_BY_FIELD, REMV_M3_HA_YR * 40), 
               color = "red", size = 0.5, position = position_nudge(x = 0.05)) +
    geom_errorbar(aes(x = GROUP_BY_FIELD,
                      ymin = (REMV_M3_YR / 1e6) - (1.96 * (sqrt(VAR_REMV_M3_YR) / 1e6)), 
                      ymax = (REMV_M3_YR / 1e6) + (1.96 * (sqrt(VAR_REMV_M3_YR) / 1e6))), 
                  width = 0.2, color = "blue", lwd = 0.25, position = position_nudge(x = -0.05)) +
    geom_errorbar(aes(x = GROUP_BY_FIELD,
                      ymin = (REMV_M3_HA_YR * 40) - (1.96 * (sqrt(VAR_REMV_M3_HA_YR) * 40)), 
                      ymax = (REMV_M3_HA_YR * 40) + (1.96 * (sqrt(VAR_REMV_M3_HA_YR) * 40))), 
                  width = 0.2, color = "red", lwd = 0.25, position = position_nudge(x = 0.05)) +
    scale_y_continuous(expand = c(0, 0),
                       expression(paste("Total (million ",m^3*yr^-1, ")")),
                       sec.axis = sec_axis(~ . / 40, name = expression(paste("Average (",m^3*ha^-1*yr^-1, ")")))) +
    coord_cartesian(ylim = c(0, 150)) + 
    labs(title = title,
         x = bquote(.(x.lab))) +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          text = element_text(size = 8),
          plot.title = element_text(size = 10), 
          axis.title.y.left = element_text(colour = "blue"),
          axis.title.y.right = element_text(colour = "red"),
          axis.text.y.left = element_text(colour = "blue"),
          axis.text.y.right = element_text(colour = "red")) }

####  LC_FOREST ####
area.lc.forest.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_LC_FOREST_AVERAGE.csv",
                             levels = c("0", "25", "50", "75"),
                             values = list("0", "25", "50", "75"))
rmv.lc.forest.avg <- rmv.x.avg(file = "DATA/BIVARIATE/FFO_REMOVALS_LC_FOREST_AVERAGE.csv", 
                               area.lc.forest.avg,
                               levels = c("0", "25", "50", "75"),
                               values = list("0", "25", "50", "75")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "<25",
                                        "25" = "25-49", 
                                        "50" = "50-74", 
                                        "75" = "75+"))
rmv.lc.forest.tot <- rmv.x.tot(file = "DATA/BIVARIATE/FFO_REMOVALS_LC_FOREST_TOTAL.csv",
                               levels = c("0", "25", "50", "75"),
                               values = list("0", "25", "50", "75")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "<25",
                                        "25" = "25-49", 
                                        "50" = "50-74", 
                                        "75" = "75+"))
# sum(area.lc.forest$HA / 1e6)
# sum(rmv.lc.forest$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.lc.forest, "<25", "75+")

####  MILL_SAW ####
area.mill.saw.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_MILL_SAW_AVERAGE.csv",
                            levels = c("0", "50", "250", "500", "1000"),
                            values = list("0", "50", "250", "500", "1000"))
rmv.mill.saw.avg <- rmv.x.avg(file = "DATA/BIVARIATE/FFO_REMOVALS_MILL_SAW_AVERAGE.csv", 
                              area.mill.saw.avg,
                              levels = c("0", "50", "250", "500", "1000"),
                              values = list("0", "50", "250", "500", "1000")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "<50",
                                        "50" = "50-249",
                                        "250" = "250-499", 
                                        "500" = "500-999", 
                                        "1000"= "1,000+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("<50", "50-249", "250-499", 
                                                            "500-999", "1,000+"))) %>%
  arrange(GROUP_BY_FIELD)
rmv.mill.saw.tot <- rmv.x.tot(file = "DATA/BIVARIATE/FFO_REMOVALS_MILL_SAW_TOTAL.csv",
                              levels = c("0", "50", "250", "500", "1000"),
                              values = list("0", "50", "250", "500", "1000")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "<50",
                                        "50" = "50-249",
                                        "250" = "250-499", 
                                        "500" = "500-999", 
                                        "1000"= "1,000+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("<50", "50-249", "250-499", 
                                                            "500-999", "1,000+"))) %>%
  arrange(GROUP_BY_FIELD)
rmv.mill.saw.tot  %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))
# sum(area.mill.saw$HA / 1e6)
# sum(rmv.mill.saw$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.mill.saw, "<50", "1,000+")

#### OWN_AGE ####
area.own.age.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_OWN_AGE_AVERAGE.csv", 
                           levels = c("18", "45", "55", "65", "75"),
                           values = (list("18", "45", "55", "65", "75")))
rmv.own.age.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_OWN_AGE_AVERAGE.csv", area.own.age.avg,
                             levels = c("18", "45", "55", "65", "75"),
                             values = (list("18", "45", "55", "65", "75"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "18" = "18-44",
                                        "45" = "45-54", 
                                        "55" = "55-64", 
                                        "65" = "65-74", 
                                        "75" = "75+"))
rmv.own.age.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_OWN_AGE_TOTAL.csv",
                             levels = c("18", "45", "55", "65", "75"),
                             values = (list("18", "45", "55", "65", "75"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "18" = "18-44",
                                        "45" = "45-54", 
                                        "55" = "55-64", 
                                        "65" = "65-74", 
                                        "75" = "75+"))
# sum(area.own.age$HA / 1e6)
# sum(rmv.own.age$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.own.age, "18-44", "75+")
# rmv.effsize(rmv.own.age)

#### OWN_HOME ####
area.home.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_OWN_HOME_AVERAGE.csv",
                        levels = c("1", "0"),
                        values = list(c("1"), c("0", "8")))
rmv.home.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_OWN_HOME_AVERAGE.csv", area.home.avg, 
                          levels = c("1", "0"),
                          values = list(c("1"), c("0", "8"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Yes", 
                                        "0" = "No"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Yes", "No")))
rmv.home.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_OWN_HOME_TOTAL.csv", 
                          levels = c("1", "0"),
                          values = list(c("1"), c("0", "8"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Yes", 
                                        "0" = "No"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Yes", "No")))
rmv.home.tot %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))
# rmv.t.test(rmv.home)

#### OWN_INC ####
area.inc.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_OWN_INC_AVERAGE.csv",
                       levels = c("0", "1", "5", "20", "50"),
                       values = list("0", "1", "5", "20", "50"))
rmv.inc.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_OWN_INC_AVERAGE.csv", area.inc.avg,
                         levels = c("0", "1", "5", "20", "50"),
                         values = list("0", "1", "5", "20", "50"))  %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "0", 
                                        "1" = "1-4", 
                                        "5" = "5-19", 
                                        "20" = "20-49", 
                                        "50" = "50+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0", "1-4", "5-19", 
                                                            "20-49", "50+")))
rmv.inc.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_OWN_INC_TOTAL.csv",
                         levels = c("0", "1", "5", "20", "50"),
                         values = list("0", "1", "5", "20", "50"))  %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "0", 
                                        "1" = "1-4", 
                                        "5" = "5-19", 
                                        "20" = "20-49", 
                                        "50" = "50+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0", "1-4", "5-19", 
                                                            "20-49", "50+")))
# rmv.t.test(rmv.inc, "0", "50+")
# rmv.inc %>%
#   mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))

#### MANAGE_ADVICE ####
area.adv.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_OWN_ADVICE_AVERAGE.csv",
                       levels = c("1", "0"),
                       values = list(c("1"), c("0")))
rmv.adv.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_OWN_ADVICE_AVERAGE.csv", area.adv.avg,
                         levels = c("1", "0"),
                         values = list(c("1"), c("0"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Yes", 
                                        "0" = "No"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Yes", "No")))
rmv.adv.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_OWN_ADVICE_TOTAL.csv",
                         levels = c("1", "0"),
                         values = list(c("1"), c("0"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Yes", 
                                        "0" = "No"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Yes", "No")))
rmv.adv.tot %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))
#### OWN_OBJ_TIM ####
area.obj.tim.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_OWN_OBJ_TIM_AVERAGE.csv", 
                           levels = c("1", "2", "3", "4", "5"),  
                           values = list(c("1", "8"), "2", "3", "4", "5"))
rmv.obj.tim.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_OWN_OBJ_TIM_AVERAGE.csv", area.obj.tim.avg, 
                             levels = c("1", "2", "3", "4", "5"),  
                             values = list(c("1", "8"), "2", "3", "4", "5")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Not\nimportant", 
                                        "2" = "Of little\nimportance",
                                        "3" = "Of moderate\nimportance", 
                                        "4" = "Important", 
                                        "5" = "Very\nimportant"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Not\nimportant", "Of little\nimportance",
                                                            "Of moderate\nimportance", "Important", 
                                                            "Very\nimportant")))
rmv.obj.tim.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_OWN_OBJ_TIM_TOTAL.csv", 
                             levels = c("1", "2", "3", "4", "5"),  
                             values = list(c("1", "8"), "2", "3", "4", "5")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Not\nimportant", 
                                        "2" = "Of little\nimportance",
                                        "3" = "Of moderate\nimportance", 
                                        "4" = "Important", 
                                        "5" = "Very\nimportant"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Not\nimportant", "Of little\nimportance",
                                                            "Of moderate\nimportance", "Important", 
                                                            "Very\nimportant")))

#### PROGRAM_ANY ####
area.pro.any.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_OWN_PROGRAM_AVERAGE.csv", 
                           levels = c("1", "0"),
                           values = list(c("1"), c("0", "9")))
rmv.pro.any.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_OWN_PROGRAM_AVERAGE.csv", area.pro.any.avg, 
                             levels = c("1", "0"),
                             values = list(c("1"), c("0", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Yes", 
                                        "0" = "No"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Yes", "No")))
rmv.pro.any.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_OWN_PROGRAM_TOTAL.csv", 
                             levels = c("1", "0"),
                             values = list(c("1"), c("0", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "Yes", 
                                        "0" = "No"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Yes", "No")))
rmv.pro.any.tot %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))
#### OWN_SIZE ####
area.ac.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_OWN_SIZE_AVERAGE.csv", 
                      levels = c("1", "10", "100", "500", "1000"),
                      values = list("1", c("10", "20", "50"), c("100", "200"), "500",  c("1000", "5000")))
rmv.ac.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_OWN_SIZE_AVERAGE.csv", area.ac.avg, 
                        levels = c("1", "10", "100", "500", "1000"),
                        values = list("1", c("10", "20", "50"), c("100", "200"), "500",  c("1000", "5000"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "0.4-3.9",
                                        "10" = "4-39", 
                                        "100" = "40-199",
                                        "500" = "200-399", 
                                        "1000" = "400+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0.4-3.9", "4-39", "40-199",
                                                            "200-399", "400+")))
rmv.ac.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_OWN_SIZE_TOTAL.csv", 
                        levels = c("1", "10", "100", "500", "1000"),
                        values = list("1", c("10", "20", "50"), c("100", "200"), "500",  c("1000", "5000"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "1" = "0.4-3.9",
                                        "10" = "4-39", 
                                        "100" = "40-199",
                                        "500" = "200-399", 
                                        "1000" = "400+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0.4-3.9", "4-39", "40-199",
                                                            "200-399", "400+")))

#### POP_DENSITY ####
area.pop.dens.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_POP_DENS_AVERAGE.csv",
                            levels = c("0", "5", "10", "20", "50"),
                            values = list("0", "5", "10", "20", c("50", "150", "400")))
rmv.pop.dens.avg <- rmv.x.avg(file = "DATA/BIVARIATE/FFO_REMOVALS_POP_DENS_AVERAGE.csv", area.pop.dens.avg,
                              levels = c("0", "5", "10", "20", "50"),
                              values = list("0", "5", "10", "20", c("50", "150", "400"))) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "0-4",
                                        "5" = "5-9", 
                                        "10" = "10-19",
                                        "20" = "20-49", 
                                        "50" = "50+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0-4", "5-9", "10-19", "20-49", "50+")))
rmv.pop.dens.tot <- rmv.x.tot(file = "DATA/BIVARIATE/FFO_REMOVALS_POP_DENS_TOTAL.csv",
                              levels = c("0", "5", "10", "20", "50"),
                              values = list("0", "5", "10", "20", c("50", "150", "400"))) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "0-4",
                                        "5" = "5-9", 
                                        "10" = "10-19",
                                        "20" = "20-49", 
                                        "50" = "50+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0-4", "5-9", "10-19", "20-49", "50+")))
rmv.pop.dens.tot %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))
#### REGION ####
area.region.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_REGION_AVERAGE.csv",
                          levels = c("North", "South", "West"),
                          values = list("1", "2", "3"))
rmv.region.avg <- rmv.x.avg("DATA/BIVARIATE/FFO_REMOVALS_REGION_AVERAGE.csv", area.region.avg,
                            levels = c("North", "South", "West"),
                            values = list("1", "2", "3"))
rmv.region.tot <- rmv.x.tot("DATA/BIVARIATE/FFO_REMOVALS_REGION_TOTAL.csv",
                            levels = c("North", "South", "West"),
                            values = list("1", "2", "3"))
rmv.region.tot %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))
#### STAND_BA ####
# conv_multiunit(43.56, "ft2 / acre", "m2 / hectare") # 10
# conv_multiunit(87.12, "ft2 / acre", "m2 / hectare") # 20
# conv_multiunit(130.68, "ft2 / acre", "m2 / hectare") # 30
# conv_multiunit(174.24, "ft2 / acre", "m2 / hectare") # 40
area.ba.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_BALIVE_AVERAGE.csv",
                      levels = c("0", "1", "2", "3", "4"),
                      values = list("0", "1", "2", "3", "4"))
rmv.ba.avg <- rmv.x.avg(file = "DATA/BIVARIATE/FFO_REMOVALS_BALIVE_AVERAGE.csv", area.ba.avg,
                        levels = c("0", "1", "2", "3", "4"),
                        values = list("0", "1", "2", "3", "4")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "0.0-9.9",
                                        "1" = "10.0-19.9",
                                        "2" = "20.0-29.9", 
                                        "3" = "30.0-39.9", 
                                        "4"= "40.0+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0.0-9.9", "10.0-19.9", "20.0-29.9", 
                                                            "30.0-39.9", "40.0+")))
rmv.ba.tot <- rmv.x.tot(file = "DATA/BIVARIATE/FFO_REMOVALS_BALIVE_TOTAL.csv",
                        levels = c("0", "1", "2", "3", "4"),
                        values = list("0", "1", "2", "3", "4")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "0.0-9.9",
                                        "1" = "10.0-19.9",
                                        "2" = "20.0-29.9", 
                                        "3" = "30.0-39.9", 
                                        "4"= "40.0+"),
         GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("0.0-9.9", "10.0-19.9", "20.0-29.9", 
                                                            "30.0-39.9", "40.0+")))

#### STAND_ORIGIN ####
area.origin.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_STDORGCD_AVERAGE.csv",
                          levels = c("0", "1"), values = list("0", "1"))
rmv.origin.avg <- rmv.x.avg(file = "DATA/BIVARIATE/FFO_REMOVALS_STDORGCD_AVERAGE.csv", area.origin.avg,
                            levels = c("0", "1"), values = list("0", "1")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "Natural",
                                        "1" = "Planted"))
rmv.origin.tot <- rmv.x.tot(file = "DATA/BIVARIATE/FFO_REMOVALS_STDORGCD_TOTAL.csv",
                            levels = c("0", "1"), values = list("0", "1")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                        "0" = "Natural",
                                        "1" = "Planted"))
rmv.origin.tot %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))

#### STAND_TYPE ####
area.stand.type.avg <- area.x("DATA/BIVARIATE/FFO_FORESTAREA_FORTYPCD_AVERAGE.csv",
                              levels = c("Softwood", "Hardwood"),
                              values = list(c("101", "102", "103", "104", "105", "121", "122", "123", "124", "125", "126", "127", 
                                              "128", "141", "142", "161", "162", "163", "164", "165", "166", "167", "168", "171", 
                                              "182", "184", "185", "201", "202", "221", "225", "226", "261", "262", "265", "266", 
                                              "267", "269", "280", "281", "301", "304", "305", "321", "341", "361", "363", "368", 
                                              "369", "371", "381", "383", "384", "385"), 
                                            c("401", "402", "403", "404", "405", "406", "407", "409", "501", "502", "503", "504", 
                                              "505", "506", "507", "508", "509", "510", "511", "512", "513", "514", "515", "516", 
                                              "517", "519", "520", "601", "602", "605", "606", "607", "608", "609", "701", "702", 
                                              "703", "704", "705", "706", "707", "708", "709", "722", "801", "802", "805", "809", 
                                              "901", "902", "903", "904", "905", "911", "912", "921", "922", "923", "924", "931", 
                                              "933", "934", "935", "941", "942", "943", "961", "962", "971", "972", "973", "974", 
                                              "975", "976", "982", "983", "989", "991", "992", "995", "999")))
rmv.stand.type.avg <- rmv.x.avg(file = "DATA/BIVARIATE/FFO_REMOVALS_FORTYPCD_AVERAGE.csv", area.stand.type.avg,
                                levels = c("Softwood", "Hardwood"),
                                values = list(c("101", "102", "103", "104", "105", "121", "122", "123", "124", "125", "126", "127", 
                                                "128", "141", "142", "161", "162", "163", "164", "165", "166", "167", "168", "171", 
                                                "182", "184", "185", "201", "202", "221", "225", "226", "261", "262", "265", "266", 
                                                "267", "269", "280", "281", "301", "304", "305", "321", "341", "361", "363", "368", 
                                                "369", "371", "381", "383", "384", "385"), 
                                              c("401", "402", "403", "404", "405", "406", "407", "409", "501", "502", "503", "504", 
                                                "505", "506", "507", "508", "509", "510", "511", "512", "513", "514", "515", "516", 
                                                "517", "519", "520", "601", "602", "605", "606", "607", "608", "609", "701", "702", 
                                                "703", "704", "705", "706", "707", "708", "709", "722", "801", "802", "805", "809", 
                                                "901", "902", "903", "904", "905", "911", "912", "921", "922", "923", "924", "931", 
                                                "933", "934", "935", "941", "942", "943", "961", "962", "971", "972", "973", "974", 
                                                "975", "976", "982", "983", "989", "991", "992", "995", "999"))) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Hardwood", "Softwood")))
rmv.stand.type.tot <- rmv.x.tot(file = "DATA/BIVARIATE/FFO_REMOVALS_FORTYPCD_TOTAL.csv",
                                levels = c("Softwood", "Hardwood"),
                                values = list(c("101", "102", "103", "104", "105", "121", "122", "123", "124", "125", "126", "127", 
                                                "128", "141", "142", "161", "162", "163", "164", "165", "166", "167", "168", "171", 
                                                "182", "184", "185", "201", "202", "221", "225", "226", "261", "262", "265", "266", 
                                                "267", "269", "280", "281", "301", "304", "305", "321", "341", "361", "363", "368", 
                                                "369", "371", "381", "383", "384", "385"), 
                                              c("401", "402", "403", "404", "405", "406", "407", "409", "501", "502", "503", "504", 
                                                "505", "506", "507", "508", "509", "510", "511", "512", "513", "514", "515", "516", 
                                                "517", "519", "520", "601", "602", "605", "606", "607", "608", "609", "701", "702", 
                                                "703", "704", "705", "706", "707", "708", "709", "722", "801", "802", "805", "809", 
                                                "901", "902", "903", "904", "905", "911", "912", "921", "922", "923", "924", "931", 
                                                "933", "934", "935", "941", "942", "943", "961", "962", "971", "972", "973", "974", 
                                                "975", "976", "982", "983", "989", "991", "992", "995", "999"))) %>%
  filter(N > 0) %>%
  mutate(GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = c("Hardwood", "Softwood")))
rmv.stand.type.tot %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))

#### Figures ####
setEPS()
postscript("FIGURES/FIG_03_RMV_BY_X.eps", width = 7, height = 9.66)
grid.arrange(rmv.plot(rmv.lc.forest.tot, rmv.lc.forest.avg, "A. Land cover - forest (% within 1 km)"),
             rmv.plot(rmv.mill.saw.tot, rmv.mill.saw.avg, "B. Mill - sawmill index"), 
             rmv.plot(rmv.own.age.tot, rmv.own.age.avg, "C. Owner - age (years)"), 
             rmv.plot(rmv.home.tot, rmv.home.avg, "D. Owner - home"), 
             rmv.plot(rmv.inc.tot, rmv.inc.avg, "E. Owner - income (% of annual income)"),
             rmv.plot(rmv.adv.tot, rmv.adv.avg, "F. Owner - management advice"),
             rmv.plot(rmv.obj.tim.tot, rmv.obj.tim.avg, "G. Owner - objective - timber importance"),
             rmv.plot(rmv.pro.any.tot, rmv.pro.any.avg, "H. Owner - program participation"), 
             rmv.plot(rmv.ac.tot, rmv.ac.avg, "I. Own - size of forest holdings (ha)"), 
             rmv.plot(rmv.pop.dens.tot, rmv.pop.dens.avg, expression(paste("J. Population density (people per ",km^2,")"))), # labs(x = expression(paste("People per ",km^2))
             rmv.plot(rmv.region.tot, rmv.region.avg, "K. Region"), 
             rmv.plot(rmv.ba.tot, rmv.ba.avg, expression(paste("L. Stand - basal area (", m^2*ha^-1, ")"))), # labs(x = expression(m^2*ha^-1))
             rmv.plot(rmv.stand.type.tot, rmv.stand.type.avg, "M. Stand - forest type"),
             rmv.plot(rmv.origin.tot, rmv.origin.avg, "N. Stand - origin"), 
             ncol = 2,
             top = textGrob("Figure 3", hjust = 0, x = 0))
dev.off()
