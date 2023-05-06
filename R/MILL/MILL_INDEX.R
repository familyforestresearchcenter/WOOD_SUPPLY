#### WOOD SUPPLY ####
#### TPO         ####

#### General Set up ####
rm(list = ls())
library(tidyverse)
library(geosphere)
library(parallel)
library(measurements)

#### Functions ####
mill.index <- function(plot.long, plot.lat,  
                       mill.long, mill.lat, mill.vol,
                       mill.num = 3, min.dist = Inf) {
  plot.xy <- cbind(plot.long, plot.lat)
  mill.xy <- cbind(mill.long, mill.lat)
  dist.mill.plot <- distm(mill.xy, plot.xy)
  tibble(dist = dist.mill.plot[,1] / 1000) %>%
    bind_cols(vol = mill.vol) %>%
    filter(dist <= min.dist) %>%
    mutate(mill_index = vol / (dist^2)) %>%
    slice_max(mill_index, n = mill.num) %>%
    summarize(sum(mill_index) / mill.num) %>%
    pull()}

#### Load Data ####
states <- read_csv("DATA/REF/REF_STATE.csv") %>%
  select(STATECD = STATECD_FIA, STATE_ABB = STATE_FIA_ABB, 
         REGION_FIA = FIAREGION_ALPHA) %>%
  distinct()
plots <- as.data.frame(read_csv("DATA/MILLS/PLOT_LAT_LON.csv", 
                                col_types = "_cnn") %>%
                         rename(LAT = ACTUAL_LAT, LONG = ACTUAL_LON))
summary(plots)
model.plots.cn <- readRDS("DATA/MODEL/WOOD_SUPPLY_MODEL_DATA.RDS") %>%
  select(PLT_CN) %>% pull(PLT_CN)
model.plots <- plots %>%
  filter(PLT_CN %in% model.plots.cn)
plots <- as.data.frame(read_csv("DATA/PLOTS/PLOT_RMRS_LAT_LON.csv", 
                                col_types = "cnn") %>%
                         rename(LAT = ACTUAL_LAT, LONG = ACTUAL_LON))
model.plots <- plots
mills <- read_csv("DATA/MILLS/MILL_LOCATIONS.csv", 
                  col_types = "_ffnnn") %>%
  rename(LAT = MILL_LAT, LONG = MILL_LON) %>%
  mutate(LONG = if_else(LONG > 0, -LONG, LONG)) %>%
  filter(MILL_STATECD %in% 
           (states %>% 
              filter(REGION_FIA %in% c("NRS", "SRS")) %>%
              pull(STATECD)),
         !MILL_STATECD %in% c(23, 48))
mills_maine <- read_csv("DATA/MILLS/MAINE_MILL_LOCATIONS.csv") %>%
  select(MILL_STATECD = MILL_STATE, MILL_TYPE_CD = MILL_TYPE_,
         TOT_MCF, LAT = coords.x2, LONG = coords.x1) %>%
  mutate(MILL_STATECD = factor(MILL_STATECD),
         MILL_TYPE_CD = factor(MILL_TYPE_CD))
mills_west <- read_csv("DATA/MILLS/West_MillCoord2Brett.csv") %>%
  select(MILL_STATECD = stcd, MILL_TYPE_CD = type,
         TOT_MCF = mos_mcf, LAT = lat, LONG = lon) %>%
  filter(!MILL_STATECD %in% c(2)) %>%
  mutate(MILL_STATECD = factor(MILL_STATECD),
         MILL_TYPE_CD = factor(MILL_TYPE_CD))
mills <- mills %>%
  bind_rows(mills_maine) %>%
  bind_rows(mills_west) %>%
  filter(!is.na(TOT_MCF), !is.na(LAT), !is.na(LONG)) %>%
  mutate(TOT_M3 = conv_unit(TOT_MCF * 1000, "ft3", "m3"))
mills <- as.data.frame(mills)
# summary(mills)

# state <- map_data("state")
# pdf("FIGURES/MILL_LOCATIONS.pdf", width = 11, height = 8.5)
# ggplot() + 
#   geom_polygon(data=state, aes(x=long, y=lat, group=group),
#                color = "white") +
#   guides(fill = "none") +
#   geom_point(data = mills, aes(LONG, LAT, 
#                                color = MILL_TYPE_CD,
#                                size = TOT_MCF)) +
#   theme(aspect.ratio = 3 /4)
# dev.off()


#### TEST ####
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, .libPaths("C:/Program Files/R/R-4.0.3/library"))
clusterEvalQ(cl, .libPaths("C:/Users/bbutler01/Documents/R/win-library/4.0"))
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(geosphere))

test.mills <- mills %>%
  filter(!is.na(TOT_MCF), !is.na(LAT), !is.na(LONG)) %>%
  slice(1:100)
test.plots <- plots %>%
  slice(1:10)

clusterExport( cl, c('test.mills', 'test.plots') )
test.mill.index <- 
  cbind(PLT_CN = test.plots$PLT_CN,
        PULP_INDEX = clusterMap(
          cl,
          mill.index, 
          test.plots$LONG, test.plots$LAT, 
          MoreArgs = list(mill.long = test.mills$LONG, 
                          mill.lat = test.mills$LAT, 
                          mill.vol = test.mills$TOT_M3)))
test.mill.index.df <-
  tibble(PLT_CN = unlist(test.mill.index[,1]),
         PULP_INDEX = unlist(test.mill.index[,2]))
stopCluster(cl)
rm(cl)

### Calculate Indices ####
# Pulp/Paper Mills
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, .libPaths("C:/Program Files/R/R-4.0.3/library"))
clusterEvalQ(cl, .libPaths("C:/Users/bbutler01/Documents/R/win-library/4.0"))
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(geosphere))

pulp.mills <- mills %>%
  filter(MILL_TYPE_CD %in% c(20, 30, 40, 50), 
         !is.na(TOT_MCF), !is.na(LAT), !is.na(LONG))

clusterExport( cl, c('pulp.mills', 'plots') )
pulp.mill.index <- 
  cbind(PLT_CN = plots$PLT_CN,
        PULP_INDEX = clusterMap(
          cl,
          mill.index, 
          plots$LONG, plots$LAT, 
          MoreArgs = list(mill.lat = pulp.mills$LAT, 
                          mill.long = pulp.mills$LONG, 
                          mill.vol = pulp.mills$TOT_M3,
                          mill.num = 1)))
pulp.mill.index.df <-
  tibble(PLT_CN = unlist(pulp.mill.index[,1]),
         PULP_INDEX = unlist(pulp.mill.index[,2]))

write_csv(pulp.mill.index.df, "DATA/MILLS/PULPMILL_INDEX.csv")

stopCluster(cl)
rm(cl)

pulp.mill.index.df <- read_csv("DATA/MILLS/PULPMILL_INDEX.csv")
summary(pulp.mill.index.df)
max(pulp.mill.index.df$PULP_INDEX)
# Pulp/Paper Mills 100 km (62 mi)
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, .libPaths("C:/Program Files/R/R-4.0.3/library"))
clusterEvalQ(cl, .libPaths("C:/Users/bbutler01/Documents/R/win-library/4.0"))
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(geosphere))

pulp.mills <- mills %>%
  filter(MILL_TYPE_CD %in% c(20, 30, 40, 50), 
         !is.na(TOT_MCF), !is.na(LAT), !is.na(LONG))

clusterExport( cl, c('pulp.mills', 'plots') )
pulp.mill.index.100 <- 
  cbind(PLT_CN = plots$PLT_CN,
        PULP_INDEX = clusterMap(
          cl,
          mill.index, 
          plots$LONG, plots$LAT, 
          MoreArgs = list(mill.lat = pulp.mills$LAT, 
                          mill.long = pulp.mills$LONG, 
                          mill.vol = pulp.mills$TOT_M3,
                          mill.num = 1,
                          min.dist = 100)))
pulp.mill.index.100.df <-
  tibble(PLT_CN = unlist(pulp.mill.index.100[,1]),
         PULP_INDEX = unlist(pulp.mill.index.100[,2]))

write_csv(pulp.mill.index.100.df, "DATA/MILLS/PULPMILL_INDEX_100.csv")

stopCluster(cl)
rm(cl)

# Sawmill
cl <- makeCluster(1)
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, .libPaths("C:/Program Files/R/R-4.0.3/library"))
clusterEvalQ(cl, .libPaths("C:/Users/bbutler01/Documents/R/win-library/4.0"))
clusterEvalQ(cl, install.packages(c("tidyverse", "geosphere")))
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(geosphere))

saw.mills <- mills %>%
  filter(!MILL_TYPE_CD %in% c(20, 30, 40, 50), 
         !is.na(TOT_MCF), !is.na(LAT), !is.na(LONG))

clusterExport( cl, c('saw.mills', 'plots') )
saw.mill.index <- 
  cbind(PLT_CN = plots$PLT_CN,
        PULP_INDEX = clusterMap(
          cl,
          mill.index, 
          plots$LONG, plots$LAT, 
          MoreArgs = list(mill.lat = saw.mills$LAT, 
                          mill.long = saw.mills$LONG, 
                          mill.vol = saw.mills$TOT_M3)))

saw.mill.index <- 
  cbind(PLT_CN = plots$PLT_CN,
        SAW_INDEX = mapply(
          mill.index, 
          plots$LONG, plots$LAT, 
          MoreArgs = list(mill.lat = saw.mills$LAT, 
                          mill.long = saw.mills$LONG, 
                          mill.vol = saw.mills$TOT_M3)))

saw.mill.index.df <-
  tibble(PLT_CN = unlist(saw.mill.index[,1]),
         SAW_INDEX = unlist(saw.mill.index[,2]))

# write_csv(saw.mill.index.df, "DATA/MILLS/SAWMILL_INDEX.csv")
write_csv(saw.mill.index.df, "DATA/MILLS/SAWMILL_INDEX_RMRS.csv")
stopCluster(cl)
rm(cl)

# Sawmill - 500 km (311 mi)
cl <- makeCluster(detectCores() - 1)
clusterEvalQ(cl, .libPaths("C:/Program Files/R/R-4.0.3/library"))
clusterEvalQ(cl, .libPaths("C:/Users/bbutler01/Documents/R/win-library/4.0"))
clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(geosphere))

saw.mills <- mills %>%
  filter(!MILL_TYPE_CD %in% c(20, 30, 40, 50), 
         !is.na(TOT_MCF), !is.na(LAT), !is.na(LONG))

clusterExport( cl, c('saw.mills', 'plots') )
saw.mill.index.500 <- 
  cbind(PLT_CN = plots$PLT_CN,
        PULP_INDEX = clusterMap(
          cl,
          mill.index, 
          plots$LONG, plots$LAT, 
          MoreArgs = list(mill.lat = saw.mills$LAT, 
                          mill.long = saw.mills$LONG, 
                          mill.vol = saw.mills$TOT_M3,
                          min.dist = 500)))
saw.mill.index.500.df <-
  tibble(PLT_CN = unlist(saw.mill.index.500[,1]),
         SAW_INDEX = unlist(saw.mill.index.500[,2]))

write_csv(saw.mill.index.500.df, "DATA/MILLS/SAWMILL_INDEX_500.csv")

stopCluster(cl)
rm(cl)

#### Combnine ####
pulp.mill.index.df <- read_csv("DATA/MILLS/PULPMILL_INDEX.csv",
                               col_types = "cn")
pulp.mill.index.100.df <- read_csv("DATA/MILLS/PULPMILL_INDEX_100.csv",
                                   col_types = "cn") %>%
  rename(PULP_INDEX_100 = PULP_INDEX)
saw.mill.index.df <- read_csv("DATA/MILLS/SAWMILL_INDEX.csv",
                                   col_types = "cn")
saw.mill.index.500.df <- read_csv("DATA/MILLS/SAWMILL_INDEX_500.csv",
                                   col_types = "cn") %>%
  rename(SAW_INDEX_500 = SAW_INDEX)

mill.indices <-
  pulp.mill.index.df %>%
  full_join(pulp.mill.index.100.df, by = "PLT_CN") %>%
  full_join(saw.mill.index.df, by = "PLT_CN") %>%
  full_join(saw.mill.index.500.df, by = "PLT_CN")
summary(mill.indices)
ggplot(mill.indices, aes(log(PULP_INDEX))) + geom_histogram()
ggplot(mill.indices, aes((SAW_INDEX))) + geom_histogram()
ggplot(mill.indices, aes(log(SAW_INDEX))) + geom_histogram()
round(exp(1:10))
#  3     7    20    55   148   403  1097  2981  8103 22026
write_csv(mill.indices, "DATA/MILLS/PLOT_MILL_INDICES.csv")
