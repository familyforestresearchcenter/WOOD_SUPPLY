#### WOOD SUPPLY ####
## Removals by Treatment

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(measurements)

#### Removals by OWNGRP ####
rmv.trt <- read_csv("DATA/SUMMARY/REMOVALS_TRT.csv",
                    col_types = "cciccnnnnnnn__") %>%
  mutate(REMV_M3_YR = conv_unit(ESTIMATE, "ft3", "m3"),
         VAR_REMV_M3_YR = (conv_unit(sqrt(VAR_OF_ESTIMATE), "ft3", "m3"))^2) %>%
  mutate(STATECD = case_when(nchar(EVAL_GRP) == 5 ~ substr(EVAL_GRP, 1, 1),
                             nchar(EVAL_GRP) == 6 ~ substr(EVAL_GRP, 1, 2),
                             TRUE ~ "NA"),
         TRT = case_when(GROUP_BY_FIELD == "00" ~ "No treatment",
                         GROUP_BY_FIELD == 10 ~ "Cutting",
                         GROUP_BY_FIELD == 20 ~ "Site preparation",
                         GROUP_BY_FIELD == 30 ~ "Artificial regeneration",
                         GROUP_BY_FIELD == 40 ~ "Natural regeneration",
                         GROUP_BY_FIELD == 50 ~ "Other silvicultural treatment",
                         TRUE ~ "NA")) %>%
  group_by(STATECD, TRT) %>% 
  summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum), .groups = "drop") %>%
  mutate(PERC = (REMV_M3_YR / sum(REMV_M3_YR) * 100)) %>%
  ungroup() %>%
  mutate(fips = str_pad(STATECD, 2, "left", "0"))
rmv.trt

#### Forest Area ####
fa <- readRDS("DATA/NWOS/NWOS_FOREST_AREA_2018_20190909.RDS") %>%
  mutate(HA = conv_unit(ACRES, "acre", "hectare"),
         VAR_HA = (conv_unit(sqrt(ACRES_VARIANCE), "acre", "hectare"))^2) %>%
  mutate(STATECD = as.character(STATECD),
         STATECD = recode(STATECD, 
                          "40.1" = "40",
                          "40.2" = "40",
                          "48.1" = "48",
                          "48.2" = "48"),
         OWNGRP = recode(OWNGRP, "Other private" = "Other\nprivate")) %>%
  filter(STATECD %in% (rmv.trt %>% distinct(STATECD) %>% pull())) %>%
  group_by(STATECD, OWNGRP) %>%
  summarize(across(c(HA, VAR_HA), sum), .groups = "drop")
fa.tot.ffo <- fa %>%
  filter(OWNGRP == "Family") %>%
  group_by(OWNGRP) %>%
  summarize(across(c(HA, VAR_HA), sum), .groups = "drop") 

#### Removals Totals ####
rmv.trt.tot <- rmv.trt %>%
  group_by(TRT) %>%
  summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum)) %>%
  bind_cols(fa.tot.ffo %>% select(-OWNGRP)) %>%
  mutate(REMV_M3_HA_YR = REMV_M3_YR / HA,
         VAR_REMV_M3_HA_YR = {(REMV_M3_YR / HA)^2 * ((VAR_REMV_M3_YR / REMV_M3_YR^2) + (VAR_HA / HA^2))}) %>%
  arrange(desc(REMV_M3_YR)) %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR),
         REMV_M3_E6_YR = REMV_M3_YR / 1e6,
         VAR_REMV_M3_E6_YR = (sqrt(VAR_REMV_M3_YR) / 1e6)^2,
         SE_REMV_M3_E6_YR = sqrt(VAR_REMV_M3_E6_YR),
         SE_REMV_M3_HA_YR = sqrt(VAR_REMV_M3_HA_YR)) 
rmv.trt.tot

