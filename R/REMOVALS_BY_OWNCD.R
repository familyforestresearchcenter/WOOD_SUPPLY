#### WOOD SUPPLY ####
## Summarize FIA Removals by OWNCD

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(measurements)
library(gridExtra)
library(usmap)
library(RColorBrewer)
library(ggpubr)

#### Removals by OWNGRP ####
rmv.own <- read_csv("DATA/OWNCD/REMOVALS_OWNCD.csv",
                       col_types = "cciccnnnnnnn__") %>%
  mutate(ESTIMATE = conv_unit(ESTIMATE, "ft3", "m3"),
         VAR_OF_ESTIMATE = (conv_unit(sqrt(VAR_OF_ESTIMATE), "ft3", "m3"))^2) %>%
  rename(REMV_M3_YR = ESTIMATE, VAR_REMV_M3_YR = VAR_OF_ESTIMATE) %>%
  mutate(STATECD = case_when(nchar(EVAL_GRP) == 5 ~ substr(EVAL_GRP, 1, 1),
                             nchar(EVAL_GRP) == 6 ~ substr(EVAL_GRP, 1, 2),
                             TRUE ~ "NA"),
         GROUP_BY_FIELD = case_when(GROUP_BY_FIELD %in% 11:25 ~ "Federal",
                            GROUP_BY_FIELD %in% 31 ~ "State",
                            GROUP_BY_FIELD %in% 32:33 ~ "Local",
                            GROUP_BY_FIELD %in% 41 ~ "Corporate",
                            GROUP_BY_FIELD %in% 42:43 ~ "Other\nprivate",
                            GROUP_BY_FIELD %in% 44 ~ "Tribal",
                            GROUP_BY_FIELD %in% 45  ~ "Family",
                            TRUE ~ "NA")) %>%
  group_by(STATECD, GROUP_BY_FIELD) %>% 
  summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum), .groups = "drop") %>%
  group_by(STATECD) %>% 
  mutate(PERC = (REMV_M3_YR / sum(REMV_M3_YR) * 100)) %>%
  ungroup() %>%
  mutate(fips = str_pad(STATECD, 2, "left", "0"))  
rmv.own

#### Forest Area ####
fa <- read_csv("DATA/OWNCD/FORESTAREA_OWNCD.csv",
                   col_types = "cciccnnnnnnn__") %>%
  mutate(STATECD = case_when(nchar(EVAL_GRP) == 5 ~ substr(EVAL_GRP, 1, 1),
                             nchar(EVAL_GRP) == 6 ~ substr(EVAL_GRP, 1, 2),
                             TRUE ~ "NA"),
         GROUP_BY_FIELD = case_when(GROUP_BY_FIELD %in% 11:25 ~ "Federal",
                            GROUP_BY_FIELD %in% 31 ~ "State",
                            GROUP_BY_FIELD %in% 32:33 ~ "Local",
                            GROUP_BY_FIELD %in% 41 ~ "Corporate",
                            GROUP_BY_FIELD %in% 42:43 ~ "Other\nprivate",
                            GROUP_BY_FIELD %in% 44 ~ "Tribal",
                            GROUP_BY_FIELD %in% 45  ~ "Family",
                            TRUE ~ "NA")) %>%
    group_by(STATECD, GROUP_BY_FIELD) %>%
    summarize(across(c("ESTIMATE", "VAR_OF_ESTIMATE"), sum), .groups = "drop") %>%
    mutate(ESTIMATE_TOT = sum(ESTIMATE), 
           VAR_OF_ESTIMATE_TOT = sum(VAR_OF_ESTIMATE)) %>%
    filter(!GROUP_BY_FIELD %in% c(-1)) %>%
    mutate(ESTIMATE_PROP = ESTIMATE / sum(ESTIMATE),
           VAR_OF_ESTIMATE_PROP = VAR_OF_ESTIMATE / sum(VAR_OF_ESTIMATE)) %>%
    mutate(ESTIMATE = ESTIMATE_PROP * ESTIMATE_TOT,
           VAR_OF_ESTIMATE = VAR_OF_ESTIMATE_PROP * VAR_OF_ESTIMATE_TOT) %>%
    mutate(HA = conv_unit(ESTIMATE, "acre", "hectare"),
           VAR_HA = (conv_unit(sqrt(VAR_OF_ESTIMATE), "acre", "hectare"))^2) %>%
    select(STATECD, GROUP_BY_FIELD, HA, VAR_HA)
fa.tot <- fa %>%
  group_by(GROUP_BY_FIELD) %>%
  summarize(across(c(HA, VAR_HA), sum), .groups = "drop")

#### Removals Totals ####
rmv.own.tot <- rmv.own %>%
  group_by(GROUP_BY_FIELD) %>%
  summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum)) %>%
  left_join(fa.tot, by = "GROUP_BY_FIELD") %>%
  mutate(REMV_M3_HA_YR = REMV_M3_YR / HA,
         VAR_REMV_M3_HA_YR = {(REMV_M3_YR / HA)^2 * ((VAR_REMV_M3_YR / REMV_M3_YR^2) + (VAR_HA / HA^2))}) %>%
  arrange(desc(REMV_M3_YR)) %>%
  mutate(GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = GROUP_BY_FIELD),
         PROP = REMV_M3_YR / sum(REMV_M3_YR),
         REMV_M3_E6_YR = REMV_M3_YR / 1e6,
         VAR_REMV_M3_E6_YR = (sqrt(VAR_REMV_M3_YR) / 1e6)^2,
         SE_REMV_M3_E6_YR = sqrt(VAR_REMV_M3_E6_YR),
         SE_REMV_M3_HA_YR = sqrt(VAR_REMV_M3_HA_YR)) 
rmv.own.tot

pdf("FIGURES/RMV_OWNGRP.pdf", width = 5, height = 4)
ggplot(rmv.own.tot) +
  geom_hline(yintercept = seq(50, 200, 50), color = "blue", size = 0.125) +
  geom_hline(yintercept = seq(60, 200, 60), color = "red", size = 0.125) +
  geom_bar(aes(GROUP_BY_FIELD, REMV_M3_YR / 1e6), stat = "identity", color = "blue", fill = "lightblue") +
  geom_point(aes(GROUP_BY_FIELD, REMV_M3_HA_YR * 60), color = "red", position = position_nudge(x = 0.05)) +
  geom_errorbar(aes(x = GROUP_BY_FIELD,
                    ymin = (REMV_M3_YR / 1e6) - (1.96 * (sqrt(VAR_REMV_M3_YR) / 1e6)), 
                    ymax = (REMV_M3_YR / 1e6) + (1.96 * (sqrt(VAR_REMV_M3_YR) / 1e6))), 
                width = 0.2, color = "blue", position = position_nudge(x = -0.05)) +
  geom_errorbar(aes(x = GROUP_BY_FIELD,
                    ymin = (REMV_M3_HA_YR * 60) - (1.96 * (sqrt(VAR_REMV_M3_HA_YR) * 60)), 
                    ymax = (REMV_M3_HA_YR * 60) + (1.96 * (sqrt(VAR_REMV_M3_HA_YR) * 60))), 
                width = 0.2, color = "red", position = position_nudge(x = 0.05)) +
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0),
                     expression(paste("Total (million ",m^3*yr^-1, ")")), 
                     sec.axis = sec_axis(~ . / 60, name = expression(paste("Average (",m^3*ha^-1*yr^-1, ")")))) +
  labs(x = "Ownership Group") +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y.left = element_text(colour = "blue"),
        axis.title.y.right = element_text(colour = "red"),
        axis.text.y.left = element_text(colour = "blue"),
        axis.text.y.right = element_text(colour = "red")) 
dev.off()

#### Map ####
# Map - % RMV by OWNCD by STATE - 7 Maps plus legend
rmv.own2 <- rmv.own %>%
  complete(GROUP_BY_FIELD, fips, fill = list(0)) %>%
  mutate(PERC = if_else(is.na(PERC), 0, PERC))

priv.tot.state <- rmv.own2 %>% 
  filter(GROUP_BY_FIELD %in% c("Corporate", "Family", "Other\nprivate")) %>%
  group_by(STATECD) %>%
  summarize(PERC = sum(PERC))
fam.state <- rmv.own2 %>% filter(GROUP_BY_FIELD %in% c("Family"))
corp.state <- rmv.own2 %>% filter(GROUP_BY_FIELD %in% c("Corporate"))

map.fam <- plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Family")), values = "PERC") +
  scale_fill_distiller(name = "Percentage", direction = 1, limits = c(0, 100)) +
  theme(legend.position = "none") +
  labs(title = "A. Family")
# pdf("FIGURES/RMV_FFO_MAP.pdf", width = 10, height = 4.75)
# plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Family")), values = "PERC") +
#   scale_fill_distiller(name = "Percentage", direction = 1, limits = c(0, 100)) +
#   theme(legend.position = "right")
# dev.off()
map.corp <-  plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Corporate")), values = "PERC") +
  scale_fill_distiller(direction = 1, limits = c(0, 100)) +
  theme(legend.position = "none") +
  labs(title = "B. Corporate")
map.opriv <-  plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Other\nprivate")), values = "PERC") +
  scale_fill_distiller(direction = 1, limits = c(0, 100)) +
  theme(legend.position = "none") +
  labs(title = "C. Other private")
map.fed <-  plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Federal")), values = "PERC") +
  scale_fill_distiller(direction = 1, limits = c(0, 100)) +
  theme(legend.position = "none") +
  labs(title = "D. Federal")
map.state <-  plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "State")), values = "PERC") +
  scale_fill_distiller(direction = 1, limits = c(0, 100)) +
  theme(legend.position = "none") +
  labs(title = "E. State")
map.local <-  plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Local")), values = "PERC") +
  scale_fill_distiller(direction = 1, limits = c(0, 100)) +
  theme(legend.position = "none") +
  labs(title = "F. Local")
map.tribal <-  plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Tribal")), values = "PERC") +
  scale_fill_distiller(direction = 1, limits = c(0, 100)) +
  theme(legend.position = "none") +
  labs(title = "G. Tribal")
legend <- as_ggplot(get_legend(plot_usmap(data = (rmv.own2 %>% filter(GROUP_BY_FIELD == "Family")), values = "PERC") +
                       scale_fill_distiller(name = "Percentage", direction = 1, limits = c(0, 100))))

pdf("FIGURES/RMV_MAP.pdf", width = 6.5, height = 8)
grid.arrange(map.fam, map.corp,
             map.opriv, map.fed,
             map.state, map.local,
             map.tribal, legend,
             layout_matrix = rbind(c(1, 1, 1, 2, 2, 2),
                                   c(3, 3, 3, 4, 4, 4),
                                   c(5, 5, 5, 6, 6, 6),
                                   c(7, 7, 7, NA, 8, NA)))
dev.off()
