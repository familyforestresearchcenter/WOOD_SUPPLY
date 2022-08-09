#### WOOD SUPPLY ####
#### CART Model ####

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
# library(tidyverse)
# library(car)
# library(ResourceSelection)
# library(performance)

#### Import Data ####
model.data <- readRDS("DATA/MODEL/WOOD_SUPPLY_MODEL_DATA.RDS")

#### Model ####
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
# plot(hrv.cart)
# plot(hrv.cart, compress = TRUE)
# text(hrv.cart, use.n = TRUE)

prp(hrv.cart,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5)

hrv.cart.var.imp <- varImp(hrv.cart)
hrv.cart.riv <- tibble(Variable = row.names(hrv.cart.var.imp),
                           hrv.cart.var.imp) %>%
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
hrv.cart.riv
hrv.cart.riv %>% pull(Variable)

hrv.cart.riv.gg <- ggplot(hrv.cart.riv, aes(Variable, RIV)) +
  geom_hline(yintercept = seq(0, 1, 0.25), size = 0.125) +
  geom_bar(stat = "identity", color = "blue", fill = "lightblue") +
  coord_flip(ylim = c(0, 1), expand = F) +
  labs(y = "Relative Importance") +
  theme_linedraw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(t = 5.5, r = 10, b = 5.5, l = 5.5, unit = "pt")) 
hrv.cart.riv.gg
ggsave("FIGURES/CART_RIV.pdf", hrv.cart.riv.gg, width = 5, height = 5, units = "in")
