library(tidyverse)

source("metrics.R")

file <- "league_offense_overall.csv"
data <- read_csv(file)

columns <- c("season", "pa", "ab", "h", "x2b", "x3b", "hr", 
             "bb", "so", "hbp", "gdp", "sf", "sh", "sb", "cs", "r")

data <- data %>%
  select(columns)

data

totals <- data %>% 
  mutate(obp = obp(.)) %>% 
  mutate(bsr = bsr(.)) %>% 
  mutate(bmult = bsr_bmult(.)) %>% 
  mutate(new_bsr = bsr(., bmult = bsr_bmult(.)))

totals

# totals_17 <- totals %>% 
#   filter(season == 2017)
# 
# totals_17
# linear_weights_incr1(totals_17)
# linear_weights_incr(totals_17)

lw <- totals %>% 
  apply(1, linear_weights_incr) %>% 
  t() %>% 
  tbl_df()
lw

lw_totals <- bind_cols(totals, lw)
lw_totals

# lw_totals_17 <- lw_totals %>% 
#   filter(season == 2017)
# lw_totals_17
# 
# woba_weights(lw_totals_17, lw_totals_17$obp)

ww <- lw_totals %>%  
  apply(1, function(x) woba_weights(x, x["obp"])) %>% 
  t() %>% 
  tbl_df()
ww

ww_totals <- bind_cols(lw_totals, ww)
select(ww_totals, season, lw_hbp:ww_hr)


mlb <- tbl_df(list(pa=185295, ab=165567, h=42215, x2b=8397, x3b=795, hr=6105, 
                      bb=15829, so=40104, hbp=1763, gdp=3804, sf=1168, sh=925, sb=2527, cs=934, r=22582))


mlb <- mlb %>% 
  mutate(obp = obp(.)) %>% 
  mutate(bsr = bsr(.)) %>% 
  mutate(bmult = bsr_bmult(.)) %>% 
  mutate(new_bsr = bsr(., bmult = bsr_bmult(.)))
mlb

lw_mlb <- mlb %>% 
  apply(1, linear_weights_incr) %>% 
  t() %>% 
  tbl_df()
lw_mlb

lw_mlb <- bind_cols(mlb, lw_mlb)

ww_mlb <- lw_mlb %>%  
  apply(1, function(x) woba_weights(x, x["obp"])) %>% 
  t() %>% 
  tbl_df()

ww_mlb
