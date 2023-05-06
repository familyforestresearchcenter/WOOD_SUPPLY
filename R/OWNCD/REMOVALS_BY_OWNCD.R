#### WOOD SUPPLY ####
## Summarize FIA Removals by OWNCD

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(measurements)
library(gridExtra)
library(grid)
library(usmap)
library(RColorBrewer)
library(ggpubr)

ref.state <- read_csv("DATA/REF/REF_STATE.csv", col_types = "fcc___________") %>% 
  distinct() %>%
  rename(STATECD = STATECD_FIA, STATE_NAME = STATE_FIA_ALPHA, STATE_ABB = STATE_FIA_ABB)

#### Total Removals by OWNGRP ####
rmv.own.tot.state <- read_csv("DATA/OWNCD/REMOVALS_OWNCD_TOTAL.csv",
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
rmv.own.tot.state
rmv.own.tot <- rmv.own.tot.state %>%
  filter(!GROUP_BY_FIELD %in% "NA") %>%
  group_by(GROUP_BY_FIELD) %>%
  summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum)) %>%
  mutate(PERC = (REMV_M3_YR / sum(REMV_M3_YR)) * 100) %>%
  arrange(desc(REMV_M3_YR)) %>%
  bind_rows(rmv.own.tot.state %>%
              summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum)) %>%
              mutate(GROUP_BY_FIELD = "Total")) %>%
  mutate(REMV_E6_M3_YR = REMV_M3_YR / 1e6,
         VAR_REMV_E6_M3_YR = VAR_REMV_M3_YR / 1e6^2,
         SE_REMV_E6_M3_YR = sqrt(VAR_REMV_E6_M3_YR))
rmv.own.tot

#### Average Removals by OWNGRP ####
# Removals
rmv.own.avg.state <- read_csv("DATA/OWNCD/REMOVALS_OWNCD_AVERAGE.csv",
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
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR)) %>%
  left_join(ref.state, by = "STATECD")
rmv.own.avg.state
rmv.own.avg.state %>%
  group_by(GROUP_BY_FIELD) %>%
  summarize(across( c(REMV_M3_YR, VAR_REMV_M3_YR), sum), .groups = "drop") %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))

rmv.pub.priv.avg.state <- rmv.own.avg.state %>%
  mutate(GROUP_BY_FIELD = recode(GROUP_BY_FIELD,
                                 'Federal' = "Public",
                                 'State' = "Public",
                                 'Local' = "Public",
                                 'Corporate' = "Private",
                                 'Other\nprivate' = "Private",
                                 'Tribal' = "Tribal",
                                 'Family' = "Private")) %>%
  group_by(STATECD, STATE_NAME, GROUP_BY_FIELD) %>%
  summarize(across( c(REMV_M3_YR, VAR_REMV_M3_YR, PROP), sum), .groups = "drop")
rmv.pub.priv.avg.state.max <- rmv.pub.priv.avg.state %>%
  group_by(STATECD, STATE_NAME) %>%
  slice_max(REMV_M3_YR)
rmv.pub.priv.avg.state %>%
  group_by(GROUP_BY_FIELD) %>%
  summarize(across( c(REMV_M3_YR, VAR_REMV_M3_YR), sum), .groups = "drop") %>%
  mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR))
rmv.own.avg.state.max <- rmv.own.avg.state %>%
  group_by(STATECD, STATE_NAME) %>%
  slice_max(REMV_M3_YR)
rmv.own.avg.state.max %>%
  filter(GROUP_BY_FIELD == "Corporate") %>%
  arrange(STATE_NAME) %>%
  pull(STATE_NAME)
rmv.own.avg.state.max %>%
  filter(GROUP_BY_FIELD == "Family") %>%
  arrange(STATE_NAME) %>%
  pull(STATE_NAME)
rmv.own.avg.state.max %>%
  filter(GROUP_BY_FIELD == "Federal") %>%
  arrange(STATE_NAME) %>%
  pull(STATE_NAME)
# Forest Area
fa.own.avg.state <- read_csv("DATA/OWNCD/FORESTAREA_OWNCD_AVERAGE.csv",
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
fa.own.avg.state
# Combine
rmv.own.avg <- 
  rmv.own.avg.state %>%
  left_join(fa.own.avg.state, by = c("STATECD", "GROUP_BY_FIELD")) %>%
  group_by(GROUP_BY_FIELD) %>%
  summarize(across(c(REMV_M3_YR, VAR_REMV_M3_YR,  HA, VAR_HA), sum)) %>%
  mutate(REMV_M3_HA_YR = REMV_M3_YR / HA,
         VAR_REMV_M3_HA_YR = (REMV_M3_YR / HA)^2 * ((VAR_REMV_M3_YR / REMV_M3_YR^2) + (VAR_HA / HA^2))) %>%
  arrange(desc(REMV_M3_YR)) %>%
  mutate(GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = GROUP_BY_FIELD),
         SE_REMV_M3_HA_YR = sqrt(VAR_REMV_M3_HA_YR)) 
rmv.own.avg

#### Combine Totals and Averages ####
rmv.own <- rmv.own.tot %>%
  filter(!GROUP_BY_FIELD %in% c("Total")) %>%
  select(GROUP_BY_FIELD, REMV_E6_M3_YR, SE_REMV_E6_M3_YR) %>%
  left_join(rmv.own.avg %>%
              select(GROUP_BY_FIELD, REMV_M3_HA_YR, SE_REMV_M3_HA_YR),
            by = "GROUP_BY_FIELD") %>%
  mutate(GROUP_BY_FIELD = factor(GROUP_BY_FIELD, 
                                 levels = rmv.own.tot %>% 
                                   filter(!GROUP_BY_FIELD %in% c("Total")) %>% 
                                   pull(GROUP_BY_FIELD)))
rmv.own
#### Removals Totals ####
# rmv.own.tot <- rmv.own %>%
#   group_by(GROUP_BY_FIELD) %>%
#   summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum)) %>%
#   bind_rows(rmv.own %>%
#               summarize(across(c("REMV_M3_YR", "VAR_REMV_M3_YR"), sum)) %>%
#               mutate(GROUP_BY_FIELD = "Total")) %>%
#   left_join(fa.tot, by = "GROUP_BY_FIELD") %>%
#   mutate(REMV_M3_HA_YR = REMV_M3_YR / HA,
#          VAR_REMV_M3_HA_YR = {(REMV_M3_YR / HA)^2 * ((VAR_REMV_M3_YR / REMV_M3_YR^2) + (VAR_HA / HA^2))}) %>%
#   arrange(desc(REMV_M3_YR)) %>%
#   mutate(GROUP_BY_FIELD = factor(GROUP_BY_FIELD, levels = GROUP_BY_FIELD),
#          # PROP = REMV_M3_YR / sum(REMV_M3_YR),
#          REMV_M3_E6_YR = REMV_M3_YR / 1e6,
#          VAR_REMV_M3_E6_YR = (sqrt(VAR_REMV_M3_YR) / 1e6)^2,
#          SE_REMV_M3_E6_YR = sqrt(VAR_REMV_M3_E6_YR),
#          SE_REMV_M3_HA_YR = sqrt(VAR_REMV_M3_HA_YR)) 
# rmv.own.tot <- rmv.own.tot %>%
#   left_join(rmv.own.tot %>%
#               filter(!GROUP_BY_FIELD %in% c("Total")) %>%
#               mutate(PROP = REMV_M3_YR / sum(REMV_M3_YR)) %>%
#               select(GROUP_BY_FIELD, PROP), by = "GROUP_BY_FIELD")
# rmv.own.tot
# rmv.own.tot %>%
#   filter(GROUP_BY_FIELD %in% c("Corporate", "Family", "Other\nprivate")) %>%
#   summarize(PROP = sum(PROP))
# rmv.own.tot %>%
#   filter(GROUP_BY_FIELD %in% c("Total")) %>%
#   mutate(REMV_M3_YR_E6 = REMV_M3_YR / 1e6)

#### WO-GTR-97 ###
# conv_unit(13041200e3, "ft3", "m3") / 1e6 # 2016
# conv_unit(15573983e3, "ft3", "m3") / 1e6 # 2006
# conv_unit(16056042e3, "ft3", "m3") / 1e6 # 1996

#### OWNGRP FIGURE ####
setEPS()
postscript("FIGURES/FIG_01_RMV_OWNGRP.eps", width = 5, height = 4)
# pdf("FIGURES/RMV_OWNGRP.pdf", width = 5, height = 4)
ggplot(rmv.own) +
  geom_hline(yintercept = seq(50, 200, 50), color = "blue", size = 0.125) +
  geom_hline(yintercept = seq(60, 200, 60), color = "red", size = 0.125) +
  geom_bar(aes(GROUP_BY_FIELD, REMV_E6_M3_YR), stat = "identity", color = "blue", fill = "lightblue") +
  geom_point(aes(GROUP_BY_FIELD, REMV_M3_HA_YR * 60), color = "red", position = position_nudge(x = 0.05)) +
  geom_errorbar(aes(x = GROUP_BY_FIELD,
                    ymin = (REMV_E6_M3_YR) - (1.96 * SE_REMV_E6_M3_YR), 
                    ymax = (REMV_E6_M3_YR) + (1.96 * SE_REMV_E6_M3_YR)), 
                width = 0.2, color = "blue", position = position_nudge(x = -0.05)) +
  geom_errorbar(aes(x = GROUP_BY_FIELD,
                    ymin = (REMV_M3_HA_YR * 60) - (1.96 * SE_REMV_M3_HA_YR * 60), 
                    ymax = (REMV_M3_HA_YR * 60) + (1.96 * SE_REMV_M3_HA_YR) * 60), 
                width = 0.2, color = "red", position = position_nudge(x = 0.05)) +
  scale_y_continuous(limits = c(0, 220), expand = c(0, 0),
                     expression(paste("Total (million ",m^3*yr^-1, ")")), 
                     sec.axis = sec_axis(~ . / 60, name = expression(paste("Average (",m^3*ha^-1*yr^-1, ")")))) +
  labs(x = "Ownership Group") +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y.left = element_text(colour = "blue"),
        axis.title.y.right = element_text(colour = "red"),
        axis.text.y.left = element_text(colour = "blue"),
        axis.text.y.right = element_text(colour = "red")) +
  ggtitle("Figure 1")
dev.off()

#### Map ####
# Map - % RMV by OWNCD by STATE - 7 Maps plus legend
ref.state <- read_csv("DATA/REF/REF_STATE.csv",
                      col_types = "ccc___________") %>%
  distinct() %>%
  mutate(fips = str_pad(STATECD_FIA, 2, pad = "0"))

rmv.own.tot.state2 <- rmv.own.tot.state %>%
  complete(GROUP_BY_FIELD, fips, fill = list(0)) %>%
  mutate(PERC = if_else(is.na(PERC), 0, round(PERC)),
         PERC_CAT = cut(PERC, c(0, 25, 50, 75, 100),
                        labels = c("<25", "25-49", "50-74", "75+"),
                        right = F, include.lowest = T)) %>%
  bind_rows(ref.state %>%
              select(fips) %>%
              filter(!fips %in% rmv.own.tot.state$fips)) %>%
  complete(fips, GROUP_BY_FIELD) %>%
  mutate(PERC_CAT = if_else(is.na(PERC_CAT), "No data", as.character(PERC_CAT)),
         PERC_CAT = if_else(fips %in% rmv.own.tot.state$fips & PERC_CAT == "No data",
                            "<25",
                            PERC_CAT),
         PERC_CAT = factor(PERC_CAT,
                           levels = rev(c("No data", "<25", "25-49", "50-74", "75+"))))

priv.tot.state <- rmv.own.tot.state2 %>% 
  filter(GROUP_BY_FIELD %in% c("Corporate", "Family", "Other\nprivate")) %>%
  group_by(STATECD) %>%
  summarize(PERC = sum(PERC))
fam.state <- rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD %in% c("Family"))
corp.state <- rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD %in% c("Corporate"))

fill.colors <- c('#ca0020','#f4a582','#92c5de','#0571b0', "gray")
# fill.colors <-  c('#018571', '#80cdc1', '#dfc27d', '#a6611a',"gray")
# fill.colors <- c('#e66101','#fdb863','#b2abd2','#5e3c99',"gray")

rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Family") %>% distinct(PERC_CAT)
map.fam <- plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Family")),
                      values = "PERC_CAT", include = ref.state$fips) +
  scale_fill_manual(name = "Percentage", values = fill.colors) +
  theme(legend.position = "none") +
  labs(title = "A. Family")
rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Corporate") %>% distinct(PERC_CAT)
map.corp <-  plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Corporate")), 
                        values = "PERC_CAT") +
  scale_fill_manual(name = "Percentage", values = fill.colors[2:5]) +
  theme(legend.position = "none") +
  labs(title = "B. Corporate")
rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Other\nprivate") %>% distinct(PERC_CAT)
map.opriv <-  plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Other\nprivate")), 
                         values = "PERC_CAT") +
  scale_fill_manual(name = "Percentage", values = fill.colors[4:5]) +  
  theme(legend.position = "none") +
  labs(title = "C. Other private")
rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Federal") %>% distinct(PERC_CAT)
map.fed <-  plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Federal")), 
                       values = "PERC_CAT") +
  scale_fill_manual(name = "Percentage", values = fill.colors) +
  theme(legend.position = "none") +
  labs(title = "D. Federal")
rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "State") %>% distinct(PERC_CAT)
map.state <-  plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "State")), 
                         values = "PERC_CAT") +
  scale_fill_manual(name = "Percentage", values = fill.colors[3:5]) +
  theme(legend.position = "none") +
  labs(title = "E. State")
rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Local") %>% distinct(PERC_CAT)
map.local <-  plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Local")),
                         values = "PERC_CAT") +
  scale_fill_manual(name = "Percentage", values = fill.colors[3:5]) +
  theme(legend.position = "none") +
  labs(title = "F. Local")
rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Tribal") %>% distinct(PERC_CAT)
map.tribal <-  plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Tribal")), 
                          values = "PERC_CAT") +
  scale_fill_manual(name = "Percentage", values = fill.colors[c(2:5)]) +
  theme(legend.position = "none") +
  labs(title = "G. Tribal")
legend <- as_ggplot(get_legend(plot_usmap(data = (rmv.own.tot.state2 %>% filter(GROUP_BY_FIELD == "Family")), 
                                          values = "PERC_CAT", include = ref.state$fips) +
                                 scale_fill_manual(name = "Percentage",
                                                   values = fill.colors)))
setEPS()
postscript("FIGURES/FIG_02_RMV_MAP.eps", width = 6.5, height = 8)
# pdf("FIGURES/RMV_MAP.pdf", width = 6.5, height = 8)
grid.arrange(map.fam, map.corp,
             map.opriv, map.fed,
             map.state, map.local,
             map.tribal, legend,
             layout_matrix = rbind(c(1, 1, 1, 2, 2, 2),
                                   c(3, 3, 3, 4, 4, 4),
                                   c(5, 5, 5, 6, 6, 6),
                                   c(7, 7, 7, NA, 8, NA)),
             top=textGrob("Figure 2", hjust = 0, x = 0))
dev.off()
