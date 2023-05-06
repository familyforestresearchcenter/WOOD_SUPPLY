#### WOOD SUPPLY ####
#### Land Cover (NLCD) ####
# Initial processing done in ArcGIS

#### General Set up ####
rm(list = ls())
library(tidyverse)
# library(parallel)
# library(measurements)

#### Load Data ####
plots.xy <- read_csv("DATA/PLOTS/PLOT_LAT_LON.csv",
                  col_types = "cfnn") 
plots <- read_csv("DATA/LAND_COVER/PLOTS_1000M_IDS.csv",
                  col_types = "_cf___c__") %>%
  rename(PLT_CN_GIS = PLT_CN, STATECD_GIS = STATECD)
ag <- read_csv("DATA/LAND_COVER/PLOTS_1000M_AG.csv",
               col_types = "_c__n") %>%
  rename(AG_PROP = MEAN)
urban <- read_csv("DATA/LAND_COVER/PLOTS_1000M_URBAN.csv",
                  col_types = "_c__n") %>%
  rename(URBAN_PROP = MEAN)
forest <- read_csv("DATA/LAND_COVER/PLOTS_1000M_FOREST.csv",
                   col_types = "_c__n") %>%
  rename(FOREST_PROP = MEAN)

#### Combine ####
plots_lc <- plots.xy %>%
  dplyr::select(PLT_CN, STATECD) %>%
  bind_cols(plots) %>%
  left_join(ag, by = "ORIG_FID") %>%
  left_join(urban, by = "ORIG_FID") %>%
  left_join(forest, by = "ORIG_FID") %>%
  mutate(AG_PROP = round(AG_PROP, 5),
         URBAN_PROP = round(URBAN_PROP, 5),
         FOREST_PROP = round(FOREST_PROP, 5))
plots_lc

write_csv(plots_lc, "DATA/LAND_COVER/PLOTS_LANDCOVER.csv")
summary(plots_lc)
