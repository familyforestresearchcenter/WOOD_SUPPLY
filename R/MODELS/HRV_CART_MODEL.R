#### WOOD SUPPLY ####
#### CART Model ####

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

#### Import Data ####
model.data <- readRDS("DATA/MODEL/WOOD_SUPPLY_MODEL_DATA.RDS")

#### Full Model ####
hrv.cart <- rpart(HRV ~ 
                    LC_AG + LC_FOREST + LC_URBAN +
                    MILL_PULP + MILL_SAW +
                    OWN_AGE + OWN_FARM + OWN_HOME + 
                    OWN_INC + OWN_MANAGE_ADVICE + OWN_MANAGE_PLAN + 
                    OWN_PROGRAM + OWN_PROGRAM_CERT + OWN_PROGRAM_COST +
                    OWN_PROGRAM_TAX + OWN_OBJ_NAT + OWN_OBJ_TIM +
                    OWN_SIZE_LOG + OWN_TENURE + 
                    PLOT_ECO + PLOT_POP_DENSITY + PLOT_REGION +
                    PLOT_ROAD + PLOT_REMPER + 
                    STAND_BA + STAND_ORIGIN + STAND_PLOT_PROD + 
                    STAND_SIZE + STAND_TYPE,
      data = model.data)
hrv.cart
prp(hrv.cart, faclen = 0, extra = 1, digits=2)

#### Full Model - RIV ####

# hrv.cart.var.imp <- varImp(hrv.cart)
# hrv.cart.riv <- tibble(Variable = row.names(hrv.cart.var.imp),
#                            hrv.cart.var.imp) %>%
#   rename(Importance = Overall) %>%
#   mutate(RIV = Importance / max(Importance)) %>%
#   filter(Importance > 0) %>%
#   arrange(Importance) %>%
#   mutate(Variable = factor(Variable, levels = Variable)) %>%
#   mutate(Variable = dplyr::recode(Variable,
#                            MILL_SAW = "Mill - saw",
#                            OWN_INC = "Owner - income",
#                            PLOT_POP_DENSITY = "Plot - pop. density",
#                            LC_URBAN = "Land cover - urban",
#                            LC_FOREST = "Land cover - forest",
#                            LC_AG = "Land cover - agriculture",
#                            OWN_MANAGE_ADVICE = "Owner - man. advice",
#                            OWN_AGE = "Owner - age",
#                            PLOT_ROAD = "Plot - road dist.",
#                            PLOT_REMPER = "Plot - remeasurement",
#                            OWN_SIZE_LOG = "Owner - size",
#                            STAND_SIZE = "Stand - size",
#                            OWN_OBJ_TIM = "Owner - obj. - timber",
#                            OWN_OBJ_NAT = "Owner - obj. - nature",
#                            PLOT_REGION = "Plot - region",
#                            MILL_PULP = "Mill - pulp",
#                            STAND_BA = "Stand - basal area", 
#                            STAND_TYPE = "Stand - type", 
#                            PLOT_ECO = "Plot - ecoregion", 
#                            STAND_ORIGIN = "Stand - origin"))
# hrv.cart.riv
# hrv.cart.riv %>% arrange(as.character(Variable)) %>% pull(Variable)
# 
# hrv.cart.riv.gg <- ggplot(hrv.cart.riv, aes(Variable, RIV)) +
#   geom_hline(yintercept = seq(0, 1, 0.25), size = 0.125) +
#   geom_bar(stat = "identity", color = "blue", fill = "lightblue") +
#   coord_flip(ylim = c(0, 1), expand = F) +
#   labs(y = "Relative Importance") +
#   theme_linedraw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.title.y = element_blank(),
#         plot.margin = margin(t = 5.5, r = 10, b = 5.5, l = 5.5, unit = "pt")) 
# hrv.cart.riv.gg
# ggsave("FIGURES/CART_RIV.pdf", hrv.cart.riv.gg, width = 5, height = 5, units = "in")

##### Model - Pruned ####
hrv.cart.pruned <- prune(hrv.cart,
                         cp = hrv.cart$cptable[which.min(hrv.cart$cptable[,"xerror"]),"CP"])
hrv.cart.pruned
summary(hrv.cart.pruned)
prp(hrv.cart.pruned, faclen = 0, extra = 1, digits=2)

##### Model - Pruned - Goodness-of-fit ####
hrv.cart.pruned.pred <- tibble(OBS = hrv.cart.pruned$y,
                               PRED = predict(hrv.cart.pruned, type = "class")) %>%
  mutate(OBS = recode(OBS,
                      `1` = "No",
                      `2` = "Yes"),
         CORRECT = OBS == PRED)
hrv.cart.pruned.pred %>%
  count(CORRECT) %>%
  mutate(PERC = n / sum(n))

#### Model - Pruned - RIV ####
hrv.cart.pruned.var.imp <- varImp(hrv.cart.pruned)
hrv.cart.pruned.riv <- tibble(Variable = row.names(hrv.cart.pruned.var.imp),
                       hrv.cart.pruned.var.imp) %>%
  rename(Importance = Overall) %>%
  mutate(RIV = Importance / max(Importance)) %>%
  filter(Importance > 0) %>%
  arrange(Importance) %>%
  mutate(Variable = factor(Variable, levels = Variable)) %>%
  mutate(Variable = dplyr::recode(Variable,
                                  MILL_SAW = "Mill - saw",
                                  OWN_INC = "Owner - income",
                                  PLOT_POP_DENSITY = "Plot - pop. density",
                                  LC_URBAN = "Land cover - urban",
                                  LC_FOREST = "Land cover - forest",
                                  LC_AG = "Land cover - agriculture",
                                  OWN_MANAGE_ADVICE = "Owner - man. advice",
                                  OWN_AGE = "Owner - age",
                                  PLOT_ROAD = "Plot - road dist.",
                                  PLOT_REMPER = "Plot - remeasurement",
                                  OWN_SIZE_LOG = "Owner - size",
                                  STAND_SIZE = "Stand - size",
                                  OWN_OBJ_TIM = "Owner - obj. - timber",
                                  OWN_OBJ_NAT = "Owner - obj. - nature",
                                  PLOT_REGION = "Plot - region",
                                  MILL_PULP = "Mill - pulp",
                                  STAND_BA = "Stand - basal area", 
                                  STAND_TYPE = "Stand - type", 
                                  PLOT_ECO = "Plot - ecoregion", 
                                  STAND_ORIGIN = "Stand - origin"))
hrv.cart.pruned.riv
hrv.cart.pruned.riv %>% arrange(as.character(Variable)) %>% pull(Variable)

hrv.cart.pruned.riv.gg <- ggplot(hrv.cart.pruned.riv, aes(Variable, RIV)) +
  geom_hline(yintercept = seq(0, 1, 0.25), size = 0.125) +
  geom_bar(stat = "identity", color = "blue", fill = "lightblue") +
  coord_flip(ylim = c(0, 1), expand = F) +
  labs(y = "Relative Importance") +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 5.5, r = 10, b = 5.5, l = 5.5, unit = "pt")) 
hrv.cart.pruned.riv.gg
ggsave("FIGURES/CART_RIV.pdf", hrv.cart.pruned.riv.gg, width = 5, height = 5, units = "in")
