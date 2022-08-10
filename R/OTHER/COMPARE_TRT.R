plot2 <- read_csv("DATA/MODEL/MODEL_PLOT_DATA2.csv")
plot2.ffo <- plot2 %>%
  add_count(PLT_CN) %>%
  filter(n == 1, OWNCD == 45) %>% # One own type
  select(PLT_CN, STATECD, OWNCD, TRTCD1, REMOVALS)
plot2.ffo %>%
  group_by(TRTCD1) %>%
  summarize(SUM = sum(REMOVALS)) %>%
  mutate(PROP = SUM / sum(SUM))
plot2.ffo %>%
  filter(REMOVALS > 0) %>%
  group_by(TRTCD1) %>%
  count() %>%
  ungroup() %>%
  mutate(PROP = n / sum(n))
