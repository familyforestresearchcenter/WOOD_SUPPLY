#### WOOD SUPPLY ####
## Bivariate Ananlyses of FIA Removals by ...

#### General Set up ####
rm(list = ls())
library(tidyverse)
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

rmv.x <- function(file = "DATA/BIVARIATE/REMOVALS_OWN_HOME.csv", 
                  area.tot, levels = c("Yes", "No"), values = list(c("1"), c("0"))) {
  data <- read_csv(file, col_types = "cciccnnnnnnn__") 
  data.tot <- data %>%
    summarize(across(c("ESTIMATE", "VAR_OF_ESTIMATE"), sum)) %>%
    mutate(M3 = conv_unit(ESTIMATE, "ft3", "m3"),
           VAR_M3 = (conv_unit(sqrt(VAR_OF_ESTIMATE), "ft3", "m3"))^2)
  data <- data %>%
    filter(!GROUP_BY_FIELD %in% c("NOT AVAILABLE", "-2"))
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

rmv.plot <- function(data, title = NULL, x.lab = NULL) { 
  ggplot(data) +
    geom_hline(yintercept = seq(50, 150, 50), size = 0.125) + 
    # geom_hline(yintercept = seq(50, 150, 50), color = "blue", size = 0.125) + 
    # geom_hline(yintercept = seq(50, 150, 50), color = "red", size = 0.125) +
    geom_bar(aes(GROUP_BY_FIELD, REMV_M3_YR / 1e6), stat = "identity", 
             color = "blue", fill = "lightblue", size = 0.25) +
    geom_point(aes(GROUP_BY_FIELD, REMV_M3_HA_YR * 50), 
               color = "red", size = 0.5, position = position_nudge(x = 0.05)) +
    geom_errorbar(aes(x = GROUP_BY_FIELD,
                      ymin = (REMV_M3_YR / 1e6) - (1.96 * (sqrt(VAR_REMV_M3_YR) / 1e6)), 
                      ymax = (REMV_M3_YR / 1e6) + (1.96 * (sqrt(VAR_REMV_M3_YR) / 1e6))), 
                  width = 0.2, color = "blue", lwd = 0.25, position = position_nudge(x = -0.05)) +
    geom_errorbar(aes(x = GROUP_BY_FIELD,
                      ymin = (REMV_M3_HA_YR * 50) - (1.96 * (sqrt(VAR_REMV_M3_HA_YR) * 50)), 
                      ymax = (REMV_M3_HA_YR * 50) + (1.96 * (sqrt(VAR_REMV_M3_HA_YR) * 50))), 
                  width = 0.2, color = "red", lwd = 0.25, position = position_nudge(x = 0.05)) +
    scale_y_continuous(expand = c(0, 0),
                       expression(paste("Total (million ",m^3*yr^-1, ")")),
                       sec.axis = sec_axis(~ . / 50, name = expression(paste("Average (",m^3*ha^-1*yr^-1, ")")))) +
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

#### LC_AG ####
area.lc.ag <- area.x("DATA/BIVARIATE/FORESTAREA_LC_AG.csv",
                     levels = c("0", "25", "50", "75"),
                     values = list("0", "25", "50", "75"))
rmv.lc.ag <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_LC_AG.csv", area.lc.ag,
                   levels = c("0", "25", "50", "75"),
                   values = list("0", "25", "50", "75")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "<25",
                                 "25" = "25-49", 
                                 "50" = "50-74", 
                                 "75" = "75+"))
rmv.lc.ag.plot <- rmv.plot(rmv.lc.ag, "A. Land cover - agriculture", "Percentage")
# sum(area.lc.ag$HA / 1e6)
# sum(rmv.lc.ag$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.lc.ag, "<25", "75+")

####  LC_FOREST ####
area.lc.forest <- area.x("DATA/BIVARIATE/FORESTAREA_LC_FOREST.csv",
                         levels = c("0", "25", "50", "75"),
                         values = list("0", "25", "50", "75"))
rmv.lc.forest <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_LC_FOREST.csv", area.lc.forest,
                       levels = c("0", "25", "50", "75"),
                       values = list("0", "25", "50", "75")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "<25",
                                 "25" = "25-49", 
                                 "50" = "50-74", 
                                 "75" = "75+"))
rmv.lc.forest.plot <- rmv.plot(rmv.lc.forest, "C. Land cover - forest", "Percentage")
# sum(area.lc.forest$HA / 1e6)
# sum(rmv.lc.forest$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.lc.forest, "<25", "75+")

####  LC_DEVELOPED ####
area.lc.urban <- area.x("DATA/BIVARIATE/FORESTAREA_LC_URBAN.csv",
                        levels = c("0", "25", "50", "75"),
                        values = list("0", "25", "50", "75"))
rmv.lc.urban <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_LC_URBAN.csv", area.lc.urban,
                      levels = c("0", "25", "50", "75"),
                      values = list("0", "25", "50", "75")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "<25",
                                 "25" = "25-49", 
                                 "50" = "50-74", 
                                 "75" = "75+"))
rmv.lc.urban.plot <- rmv.plot(rmv.lc.ag, "B. Land cover - developed", "Percentage")
# sum(area.lc.urban$HA / 1e6)
# sum(rmv.lc.urban$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.lc.urban, "<25", "75+")

####  MILL_PULP ####
area.mill.pulp <- area.x("DATA/BIVARIATE/FORESTAREA_MILL_PULP.csv",
                         levels = c("0", "50", "250", "500", "1000"),
                         values = list("0", "50", "250", "500", "1000"))
rmv.mill.pulp <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_MILL_PULP.csv", area.mill.pulp,
                       levels = c("0", "50", "250", "500", "1000"),
                       values = list("0", "50", "250", "500", "1000")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "<50",
                                 "50" = "50-249",
                                 "250" = "250-499", 
                                 "500" = "500-999", 
                                 "1000"= "1,000+"))
rmv.mill.pulp.plot <- rmv.plot(rmv.mill.pulp, "D. Mill - pulp", "Index")
# sum(area.mill.pulp$HA / 1e6)
# sum(rmv.mill.pulp$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.mill.pulp, "<50", "1,000+")

####  MILL_SAW ####
area.mill.saw <- area.x("DATA/BIVARIATE/FORESTAREA_MILL_SAW.csv",
                        levels = c("0", "50", "250", "500", "1000"),
                        values = list("0", "50", "250", "500", "1000"))
rmv.mill.saw <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_MILL_SAW.csv", area.mill.saw,
                      levels = c("0", "50", "250", "500", "1000"),
                      values = list("0", "50", "250", "500", "1000")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "<50",
                                 "50" = "50-249",
                                 "250" = "250-499", 
                                 "500" = "500-999", 
                                 "1000"= "1,000+"))
rmv.mill.saw.plot <- rmv.plot(rmv.mill.saw, "E. Mill - saw", "Index")
# sum(area.mill.saw$HA / 1e6)
# sum(rmv.mill.saw$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.mill.saw, "<50", "1,000+")

#### OWN_AGE ####
area.own.age <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_AGE.csv", 
                       levels = c("18", "45", "55", "65", "75"),
                       values = (list("18", "45", "55", "65", "75")))
rmv.own.age <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_AGE.csv", area.own.age,
                     levels = c("18", "45", "55", "65", "75"),
                     values = (list("18", "45", "55", "65", "75"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "18" = "18-44",
                                 "45" = "45-54", 
                                 "55" = "55-64", 
                                 "65" = "65-74", 
                                 "75" = "75+"))
rmv.own.age.plot <- rmv.plot(rmv.own.age, "F. Owner - age", "Years")
# sum(area.own.age$HA / 1e6)
# sum(rmv.own.age$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.own.age, "18-44", "75+")
# rmv.effsize(rmv.own.age)

# OWN_EDU ####
area.own.edu <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_EDU.csv", 
                       levels = c("1", "2", "3", "4", "5", "6"),
                       values = list("1", "2", "3", "4", "5", "6"))
rmv.own.edu <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_EDU.csv", area.own.edu,
                     levels = c("1", "2", "3", "4", "5", "6"),
                     values = list("1", "2", "3", "4", "5", "6")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Less than\n12th grade", 
                                 "2" = "GED/High\nschool", 
                                 "3" = "Some\ncollege", 
                                 "4" = "Associate\ndegree", 
                                 "5" = "Bachelor's\ndegree", 
                                 "6" = "Advanced\ndegree"))
rmv.own.edu.plot <- rmv.plot(rmv.own.edu, "G. Owner - education")
# sum(area.own.age$HA / 1e6)
# sum(rmv.own.age$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.own.edu, "Less than\n12th grade", "Advanced\ndegree")
# rmv.effsize(rmv.own.edu)

# OWN_FARM ####
area.own.farm <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_FARM.csv",
                        levels = c("1", "0"),
                        values = list(c("1"), c("0", "8")))
rmv.own.farm <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_FARM.csv", area.own.farm,
                      levels = c("1", "0"),
                      values = list(c("1"), c("0", "8"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.own.farm.plot <- rmv.plot(rmv.own.farm, "H. Owner - farm")
# sum(area.own.farm$HA / 1e6)
# sum(rmv.own.age$REMV_M3_YR / 1e6)
# rmv.t.test(rmv.own.farm)

#### OWN_HOME ####
area.home <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_HOME.csv",
                    levels = c("1", "0"),
                    values = list(c("1"), c("0", "8")))
rmv.home <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_HOME.csv", area.home, 
                  levels = c("1", "0"),
                  values = list(c("1"), c("0", "8"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.home.plot <- rmv.plot(rmv.home, "I. Owner - home")
# rmv.t.test(rmv.home)

#### OWN_INC ####
area.inc <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_INC.csv",
                   levels = c("0", "1", "5", "20", "50"),
                   values = list("0", "1", "5", "20", "50"))
rmv.inc <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_INC.csv", area.inc,
                 levels = c("0", "1", "5", "20", "50"),
                 values = list("0", "1", "5", "20", "50"))  %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "0", 
                                 "1" = "1-4", 
                                 "5" = "5-19", 
                                 "20" = "20-49", 
                                 "50" = "50+"))
rmv.inc.plot <- rmv.plot(rmv.inc, "J. Owner - income", "Percentage of Annual Income")
# rmv.t.test(rmv.inc, "0", "50+")
rmv.inc %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))

#### MANAGE_ADVICE ####
area.adv <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_MANAGE_ADVICE.csv",
                   levels = c("1", "0"),
                   values = list(c("1"), c("0")))
rmv.adv <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_MANAGE_ADVICE.csv", area.adv,
                 levels = c("1", "0"),
                 values = list(c("1"), c("0"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.adv.plot <- rmv.plot(rmv.adv, "K. Owner - management advice")

#### MANAGE_PLAN ####
area.plan <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_MANAGE_PLAN.csv", 
                    levels = c("1", "0"),
                    values = list(c("1"), c("0", "9")))
rmv.plan <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_MANAGE_PLAN.csv", area.plan,
                  levels = c("1", "0"),
                  values = list(c("1"), c("0", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.plan.plot <- rmv.plot(rmv.plan, "L. Owner - management plan")

#### OWN_OBJ_NAT ####
area.obj.nat <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_OBJ_NAT.csv", 
                       levels = c("1", "2", "3", "4", "5"),  
                       values = list(c("1", "8"), "2", "3", "4", "5")) 
rmv.obj.nat <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_OBJ_NAT.csv", area.obj.nat, 
                     levels = c("1", "2", "3", "4", "5"),  
                     values = list(c("1", "8"), "2", "3", "4", "5")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Not\nimportant", 
                                 "2" = "Of little\nimportance",
                                 "3" = "Of moderate\nimportance", 
                                 "4" = "Important", 
                                 "5" = "Very\nimportant"))
rmv.obj.nat.plot <- rmv.plot(rmv.obj.nat, "M. Owner - objective - nature")

#### OWN_OBJ_TIM ####
area.obj.tim <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_OBJ_TIM.csv", 
                       levels = c("1", "2", "3", "4", "5"),  
                       values = list(c("1", "8"), "2", "3", "4", "5"))
rmv.obj.tim <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_OBJ_TIM.csv", area.obj.tim, 
                     levels = c("1", "2", "3", "4", "5"),  
                     values = list(c("1", "8"), "2", "3", "4", "5")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Not\nimportant", 
                                 "2" = "Of little\nimportance",
                                 "3" = "Of moderate\nimportance", 
                                 "4" = "Important", 
                                 "5" = "Very\nimportant"))
rmv.obj.tim.plot <- rmv.plot(rmv.obj.tim, "N. Owner - objective - timber")


#### PROGRAM_ANY ####
area.pro.any <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_ANY.csv", 
                       levels = c("1", "0"),
                       values = list(c("1"), c("0", "9")))
rmv.pro.any <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_PROGRAM_ANY.csv", area.pro.any, 
                     levels = c("1", "0"),
                     values = list(c("1"), c("0", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.pro.any.plot <- rmv.plot(rmv.pro.any, "O. Owner - program - any")


#### PROGRAM_CERT ####
area.pro.cert <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_CERT.csv", 
                        levels = c("1", "0"),
                        values = list(c("1"), c("0", "9")))
rmv.pro.cert <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_PROGRAM_CERT.csv", area.pro.cert, 
                      levels = c("1", "0"),
                      values = list(c("1"), c("0", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.pro.cert.plot <- rmv.plot(rmv.pro.cert, "P. Owner - program - certification")


#### OWN_PROGRAM_COST ####
area.pro.cost <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_COST.csv", 
                        levels = c("1", "0"),
                        values = list(c("1"), c("0", "9")))
rmv.pro.cost <- rmv.x("DATA/BIVARIATE/REMOVALS_PROGRAM_COST.csv", area.pro.cost, 
                      levels = c("1", "0"),
                      values = list(c("1"), c("0", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.pro.cost.plot <- rmv.plot(rmv.pro.cost, "Q. Owner - program - cost-share")


#### OWN_PROGRAM_TAX ####
area.pro.tax <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_PROGRAM_TAX.csv", 
                       levels = c("1", "0"),
                       values = list(c("1"), c("0", "9")))
rmv.pro.tax <- rmv.x("DATA/BIVARIATE/REMOVALS_PROGRAM_TAX.csv", area.pro.tax, 
                     levels = c("1", "0"),
                     values = list(c("1"), c("0", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "Yes", 
                                 "0" = "No"))
rmv.pro.tax.plot <- rmv.plot(rmv.pro.tax, "R. Owner - program - tax")


#### OWN_SIZE ####
area.ac <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_SIZE.csv", 
                  levels = c("1", "20", "50", "100", "200", "500", "1000"),
                  values = list(c("1", "10"), "20", "50", "100", "200", "500",  c("1000", "5000")))
rmv.ac <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_SIZE.csv", area.ac, 
                levels = c("1", "20", "50", "100", "200", "500", "1000"),
                values = list(c("1", "10"), "20", "50", "100", "200", "500", c("1000", "5000"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "0.4-7.9",
                                 "20" = "8-19", 
                                 "50" = "20-39", 
                                 "100" = "40-79",
                                 "200" = "80-199",
                                 "500" = "200-399", 
                                 "1000" = "400+"))
rmv.ac.plot <- rmv.plot(rmv.ac, "S. Own - size of forest holdings", "Hectares")


#### OWN_TENURE ####
area.tenure <- area.x("DATA/BIVARIATE/FORESTAREA_OWN_TENURE.csv", 
                      levels = c("0", "10", "25", "50"),
                      values = list("0", "10", "25", "50"))
rmv.tenure <- rmv.x("DATA/BIVARIATE/REMOVALS_OWN_TENURE.csv", area.tenure, 
                    levels = c("0", "10", "25", "50"),
                    values = list("0", "10", "25", "50")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "0-9", 
                                 "10" = "10-24", 
                                 "25" = "25-49", 
                                 "50" = "50+"))
rmv.tenure.plot <- rmv.plot(rmv.tenure, "T. Owner - tenure", "Years")
rmv.t.test(rmv.tenure, "10-24", "50+")

#### PLOT_ECO ####
area.eco <- area.x("DATA/BIVARIATE/FORESTAREA_PLOT_ECO.csv",
                   levels = c("232", "231", "M242", "212", "221", "Other"),
                   values = list("232", "231", "M242", "212", "221",  
                                 c("211", "222", "223", "234", "242", 
                                   "251", "255", "261", "262", "263", 
                                   "315", "321", "322", "331", "332", 
                                   "341", "342", "411", "M211", "M221", 
                                   "M223", "M231", "M261", "M262", "M313", 
                                   "M332", "M333", "M334")))
rmv.eco <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_PLOT_ECO.csv", area.eco,
                 levels = c("232", "231", "M242", "212", "221", "Other"),
                 values = list("232", "231", "M242", "212", "221", 
                               c("211", "222", "223", "234", "242", 
                                 "251", "255", "261", "262", "263", 
                                 "315", "321", "322", "331", "332", 
                                 "341", "342", "411", "M211", "M221", 
                                 "M223", "M231", "M261", "M262", "M313", 
                                 "M332", "M333", "M334")))
rmv.eco.plot <- rmv.plot(rmv.eco, "U. Plot - ecoregion province") +
  annotate( "text", label = "5.4\n(SE=0.02)",
            x = 1, y = 135, size = 2, colour = "red")  +
  annotate( "text", label = "3.5\n(SE=0.01)",
            x = 2, y = 135, size = 2, colour = "red") +
  annotate( "text", label = "57.7\n(SE=31.1)",
            x = 3, y = 135, size = 2, colour = "red")


#### PLOT_POP_DENSITY ####
area.pop.dens <- area.x("DATA/BIVARIATE/FORESTAREA_POP_DENS.csv",
                        levels = c("0", "5", "10", "20", "50", "150", "400"),
                        values = list("0", "5", "10", "20", "50", "150", "400"))
rmv.pop.dens <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_POP_DENS.csv", area.pop.dens,
                      levels = c("0", "5", "10", "20", "50", "150", "400"),
                      values = list("0", "5", "10", "20", "50", "150", "400")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "0-4",
                                 "5" = "5-9", 
                                 "10" = "10-19",
                                 "20" = "20-49", 
                                 "50" = "50-149", 
                                 "150" = "150-399", 
                                 "400" = "400+"))
rmv.pop.dens.plot <- rmv.plot(rmv.pop.dens, "V. Plot - population density") +
  labs(x = expression(paste("People per ",km^2)))


#### PLOT_REGION ####
area.region <- area.x("DATA/BIVARIATE/FORESTAREA_PLOT_REGION.csv",
                      levels = c("North", "South", "West"),
                      values = list(c("9", "10", "17", "18", "19", "23", "24", "25", "26", "27", 
                                      "29", "33", "34", "36", "39", "42", "44", "50", "54", "55"),
                                    c("1", "5", "12", "13", "21", "22", "28", "37", "40", "45", "47", "48", "51"),
                                    c("4", "6", "8", "16", "20", "30", "31", "32", "35", "38", "41", "46", 
                                      "49", "53", "56")))
rmv.region <- rmv.x("DATA/BIVARIATE/REMOVALS_PLOT_REGION.csv", area.region,
                    levels = c("North", "South", "West"),
                    values = list(c("9", "10", "17", "18", "19", "23", "24", "25", "26", "27", 
                                    "29", "33", "34", "36", "39", "42", "44", "50", "54", "55"),
                                  c("1", "5", "12", "13", "21", "22", "28", "37", "40", "45", "47", "48", "51"),
                                  c("4", "6", "8", "16", "20", "30", "31", "32", "35", "38", "41", "46", 
                                    "49", "53", "56")))
rmv.region.plot <- rmv.plot(rmv.region, "W. Plot - region")


#### PLOT_REMPER ####
area.plot.remper <- area.x("DATA/BIVARIATE/FORESTAREA_PLOT_REMPER.csv",
                           levels = c("0", "5", "6", "7", "8", "9", "10"),
                           values = list(c("0", "1", "2", "3", "4"),
                                         "5", "6", "7", "8", "9", "10"))
rmv.plot.remper <- rmv.x("DATA/BIVARIATE/REMOVALS_PLOT_REMPER.csv", area.plot.remper,
                         levels = c("0", "5", "6", "7", "8", "9", "10"),
                         values = list(c("0", "1", "2", "3", "4"),
                                       "5", "6", "7", "8", "9", "10")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "<5", 
                                "10" = "10+"))
rmv.plot.remper.plot <- rmv.plot(rmv.plot.remper, "X. Plot - remeasurement period", "Years")


#### PLOT_ROAD ####
area.road <- area.x("DATA/BIVARIATE/FORESTAREA_PLOT_ROAD.csv",
                    levels = c("1", "2", "3", "4", "5", "6"),
                    values = list("1", "2", "3", "4", "5", c("6", "7", "8", "9")))
rmv.road <- rmv.x("DATA/BIVARIATE/REMOVALS_PLOT_ROAD.csv", area.road,
                  levels = c("1", "2", "3", "4", "5", "6"),
                  values = list("1", "2", "3", "4", "5", c("6", "7", "8", "9"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "1" = "<30", 
                                 "2" = "30-89", 
                                 "3" = "90 - 149", 
                                 "4" = "150-299", 
                                 "5" = "300-799", 
                                 "6" = "800+"))
rmv.road.plot <- rmv.plot(rmv.road, "Y. Plot - road distance", "Meters")


#### STAND_BA ####
area.ba <- area.x("DATA/BIVARIATE/FORESTAREA_STAND_BA.csv",
                  levels = c("0", "1", "2", "3", "4"),
                  values = list("0", "1", "2", "3", "4"))
rmv.ba <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_STAND_BA.csv", area.ba,
                levels = c("0", "1", "2", "3", "4"),
                values = list("0", "1", "2", "3", "4")) %>%
  filter(!is.na(GROUP_BY_FIELD)) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "0.0-3.9",
                                 "1" = "4.0-7.9",
                                 "2" = "8.0-11.9", 
                                 "3" = "12.0-15.9", 
                                 "4"= "16.0+"))
rmv.ba.plot <- rmv.plot(rmv.ba, "Z. Stand - basal area") +
  labs(x = expression(m^2*ha^-1))


#### STAND_ORIGIN ####
area.origin <- area.x("DATA/BIVARIATE/FORESTAREA_STAND_ORIGIN.csv",
                      levels = c("0", "1"), values = list("0", "1"))
rmv.origin <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_STAND_ORIGIN.csv", area.origin,
                    levels = c("0", "1"), values = list("0", "1")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "0" = "Natural",
                                 "1" = "Planted"))
rmv.origin.plot <- rmv.plot(rmv.origin, "AA. Stand - origin")


#### STAND_PRODCUTIVITY ####
area.productivity <- area.x("DATA/BIVARIATE/FORESTAREA_STAND_PRODUCTIVITY.csv",
                            levels = c("7", "6", "5", "4","3"),
                            values = list("7", "6", "5", "4", c("3", "2", "1")))
rmv.productivity <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_STAND_PRODUCTIVITY.csv", area.productivity,
                          levels = c("7", "6", "5", "4","3"),
                          values = list("7", "6", "5", "4", c("3", "2", "1"))) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "7" = "0-1.3",
                                 "6" = "1.4-3.4",
                                 "5" = "3.5-5.8",
                                 "4" = "5.9-8.3",
                                 "3" = "8.4+"))
rmv.productivity.plot <- rmv.plot(rmv.productivity, "AB. Stand - productivity") +
  labs(x = expression(m^3*ha^-1*yr^-1))


#### STAND_SIZE ####
area.stand.size <- area.x("DATA/BIVARIATE/FORESTAREA_STAND_SIZE.csv",
                          levels = c("5", "3", "2", "1"),
                          values = list("5", "3", "2", "1"))
rmv.stand.size <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_STAND_SIZE.csv", area.stand.size,
                        levels = c("5", "3", "2", "1"),
                        values = list("5", "3", "2", "1")) %>%
  mutate(GROUP_BY_FIELD = dplyr::recode(GROUP_BY_FIELD,
                                 "5" = "Non-stocked",
                                 "3" = "Small", 
                                 "2" = "Medium", 
                                 "1"= "Large"))
rmv.stand.size.plot <- rmv.plot(rmv.stand.size, "AC. Stand - size")


#### STAND_TYPE ####
area.stand.type <- area.x("DATA/BIVARIATE/FORESTAREA_STAND_TYPE.csv",
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
rmv.stand.type <- rmv.x(file = "DATA/BIVARIATE/REMOVALS_STAND_TYPE.csv", area.stand.type,
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
rmv.stand.type.plot <- rmv.plot(rmv.stand.type, "AD. Stand - type")


#### Combined Chart ####
pdf("FIGURES/RMV_BY_X_1.pdf", width = 6.5, height = 8)
grid.arrange(rmv.lc.ag.plot, rmv.lc.urban.plot, 
             rmv.lc.forest.plot, rmv.mill.pulp.plot,
             rmv.mill.saw.plot, rmv.own.age.plot,
             rmv.own.edu.plot, rmv.own.farm.plot,
             rmv.home.plot, rmv.inc.plot,
             ncol = 2)
dev.off()

pdf("FIGURES/RMV_BY_X_2.pdf", width = 6.5, height = 8)
grid.arrange(rmv.adv.plot, rmv.plan.plot,
             rmv.obj.nat.plot, rmv.obj.tim.plot,
             rmv.pro.any.plot, rmv.pro.cert.plot,
             rmv.pro.cost.plot, rmv.pro.tax.plot,
             rmv.ac.plot, rmv.tenure.plot,
             ncol = 2)
dev.off()

pdf("FIGURES/RMV_BY_X_3.pdf", width = 6.5, height = 8)
grid.arrange(rmv.eco.plot, rmv.pop.dens.plot,
             rmv.region.plot, rmv.plot.remper.plot,
             rmv.road.plot, rmv.ba.plot,
             rmv.origin.plot, rmv.productivity.plot,
             rmv.stand.size.plot, rmv.stand.type.plot,
             ncol = 2)
dev.off()
