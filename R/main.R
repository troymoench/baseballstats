library(tidyverse)

source("R/basic_metrics.R")

file <- "R/clean_batters_overall.csv"

data <- read_csv(file)

columns <- c("name", "team", "season", "pa", "ab", "h", "x2b", "x3b", "hr",
             "bb", "so", "hbp", "sf", "sh", "sb", "cs")

data <- data %>%
  select(columns)



basic_calcs <- data %>%
  mutate(hbp_p = hbp_p(.)) %>%
  mutate(so_p = so_p(.)) %>%
  mutate(bb_p = bb_p(.)) %>%
  mutate(iso = iso(.)) %>%
  mutate(babip = babip(.)) %>%
  mutate(avg = avg(.)) %>%
  mutate(obp = obp(.)) %>%
  mutate(slg = slg(.)) %>%
  mutate(ops = ops(.)) %>%
  mutate(sar = sar(.))

basic_calcs

basic_calcs %>%
  filter(team == "WLC", pa >= 50, season >= 2015) %>%
  arrange(desc(ops))




