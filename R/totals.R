# This script calculates the league metrics and constants for each season
# output to csv or database

# TODO: Add wRAA, wRC+, OFF, OFF+, replacement level (OFF/PA), RAR
# TODO: Export data to csv or database

library(tidyverse)

source("R/basic_metrics.R")
source("R/advanced_metrics.R")

file <- "R/league_offense_overall.csv"
data <- read_csv(file)

columns <- c("season", "pa", "ab", "h", "x2b", "x3b", "hr",
             "bb", "so", "hbp", "gdp", "sf", "sh", "sb", "cs", "r")

data <- data %>%
  select(columns)

data

totals <- data %>%
  mutate(obp = obp(.)) %>%
  mutate(est_bsr = bsr(.)) %>%
  mutate(bmult = bsr_bmult(.)) %>%
  mutate(bsr = bsr(., bmult = bsr_bmult(.)))

totals

get_lw <- function(league_totals, incr = TRUE, bind = TRUE) {
  # calculate linear weights from league totals

  if (incr) {
    lw <- league_totals %>%
      apply(1, linear_weights_incr) %>%
      t() %>%
      as_tibble()
  }
  else {
    lw <- league_totals %>%
      apply(1, linear_weights_calc) %>%
      t() %>%
      as_tibble()
  }
  if (bind) {
    dplyr::bind_cols(league_totals, lw)
  }
  else {
    lw
  }
}

get_ww <- function(league_totals, incr = TRUE, bind = TRUE) {
  lw <- get_lw(league_totals, incr = incr, bind = TRUE)
  ww <- lw %>%
    apply(1, function(x) woba_weights(x, x["obp"])) %>%
    t() %>%
    tbl_df()
  if (bind) {
    dplyr::bind_cols(lw, ww)
  }
  else {
    ww
  }
}


get_lw(totals)
totals %>%
  filter(season == 2017) %>%
  get_lw(bind = FALSE)


get_ww(totals, incr = TRUE, bind = FALSE)
get_ww(totals, incr = FALSE, bind = FALSE)

totals %>%
  filter(season == 2017) %>%
  get_ww() %>%
  select(season, obp, lw_hbp:woba_scale)

weights17 <- totals %>%
  filter(season == 2017) %>%
  get_ww() %>%
  select(lw_hbp:woba_scale)
weights17

totals %>%
  filter(season == 2017) %>%
  mutate(woba = woba(., weights17)) %>%
  mutate(sbr = sbr(., weights17)) %>%
  mutate(lg_wsb = lg_wsb(., weights17)) %>%
  mutate(wsb = wsb(., lg_wsb))


weights <- totals %>%
  get_ww() %>%
  select(lw_hbp:woba_scale)
weights
totals %>%
  mutate(woba = woba(., weights)) %>%
  mutate(sbr = sbr(., weights)) %>%
  mutate(lg_wsb = lg_wsb(., weights)) %>%
  mutate(wsb = wsb(., lg_wsb))



# season_17 <- totals %>%
#   filter(season == 2017)
# map(seq(.0001, .0000001, length.out = 100), function (i) linear_weights_incr(season_17, i))


mlb <- tbl_df(list(pa=185295, ab=165567, h=42215, x2b=8397, x3b=795, hr=6105,
                      bb=15829, so=40104, hbp=1763, gdp=3804, sf=1168, sh=925, sb=2527, cs=934, r=22582))

mlb <- mlb %>%
  mutate(obp = obp(.)) %>%
  mutate(bsr = bsr(.)) %>%
  mutate(bmult = bsr_bmult(.)) %>%
  mutate(new_bsr = bsr(., bmult = bsr_bmult(.)))
mlb

get_lw(mlb)
mlb %>%
  get_ww(incr = FALSE) %>%
  select(ww_hbp:woba_scale)

mlb %>%
  get_ww(incr = TRUE) %>%
  select(ww_hbp:woba_scale)
