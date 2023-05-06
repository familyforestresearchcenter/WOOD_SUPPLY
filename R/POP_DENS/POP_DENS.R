#### WOOD SUPPLY ####
#### Population Density ####

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(tidycensus)
census_api_key("c898d3555c3a2152eebb24f0101518e50952b122")
library(sf)
# library(parallel)
# library(measurements)

#### Load Data ####
ref_states <- read_csv("DATA/REF/REF_STATE.csv",
                       col_types = "fcf____f____") %>%
  distinct() %>%
  rename(fips = STATECD_FIA, name = STATE_FIA_ALPHA,
         abb = STATE_FIA_ABB, region = REGION_ALPHA) %>%
  filter(!fips %in% c(2, 15))
plots <- read_csv("DATA/PLOTS/PLOT_LAT_LON.csv",
                  col_types = "cfnn") 
plots <- read_csv("DATA/PLOTS/PLOT_RMRS_LAT_LON.csv",
                  col_types = "cnn") 
plots_sf <- plots %>%
  # filter(STATECD %in% c(9, 25, 44)) %>%
  st_as_sf(., coords=c("ACTUAL_LON", "ACTUAL_LAT"),
                       crs = 4269)
# variables.asc <- load_variables(2018, "acs5")
# get_acs("tract", "B01003_001", year = 2018, 
#         survey = "acs5", 
#         state = 1,
#         show_call = T)
# https://api.census.gov/data/2018/acs/acs5?get=B01003_001E%2CB01003_001M%2CNAME&for=tract%3A%2A&in=state%3A01
# https://api.census.gov/data/2018/acs/acs5?get=B01003_001E%2CB01003_001M%2CNAME&for=tract%3A%2A&in=(state%3A01,state%3A04)
acs <- get_acs("tract", "B01003_001", year = 2018,
               keep_geo_vars = T, geometry = T,
               survey = "acs5", 
               # state = (ref_states %>% pull(fips)),
               state = c(4, 8, 30, 49),
               show_call = T)
acs_sf <- acs %>% st_as_sf(crs = 4269)

# ggplot() +
#   geom_sf(aes(fill = estimate), color = NA, data = acs_sf) +
#   geom_sf(data = plots_sf)

# acs$ALAND

#### Extract data ####
acs_plots <- st_intersection(plots_sf, acs_sf)

#### Calculate Pop Density (people / km2) ####
acs_plots_df <- as_tibble(acs_plots) %>% 
  mutate(POP_DENS = estimate / (ALAND / 1000^2)) %>%
  select(PLT_CN, POP_DENS)
summary(acs_plots_df$POP_DENS)

#### Save Results ####
# write_csv(acs_plots_df, "DATA/POP_DENS/POP_DENS.csv")
write_csv(acs_plots_df, "DATA/POP_DENS/POP_DENS_RMRS.csv")
# acs_plots_df <- read_csv("DATA/POP_DENS/POP_DENS.csv")
# summary(acs_plots_df$POP_DENS)
# summary(log(acs_plots_df$POP_DENS))
# round(exp(c(1,2,3,4,5,6)), -1)
# hist(log(acs_plots_df$POP_DENS))
# hist((acs_plots_df$POP_DENS[acs_plots_df$POP_DENS<100]))
# 5, 10, 20, 50, 150, 400