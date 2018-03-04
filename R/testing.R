library(tidyverse)
library(purrrlyr)

source("metrics.R")

so_p2 <- function(pa, so) {
  so / pa
}


file <- "clean_batters_overall.csv"

data <- read_csv(file)

columns <- c("name", "team", "season", "pa", "ab", "r", "h", "x2b", "x3b", "hr", "bb", "so", "hbp", "sf", "sh", "sb", "cs")

data <- data %>%
  select(columns)


# apply function on each row
# by_row is deprecated please use combination of
# tidyr::nest(); dplyr::mutate(); purrr::map()
# want to convert a data.frame into a list of data.frames or lists
by_row(data, so_p1, .to = "so_p", .collate = "rows")

nested <- data %>%
  rownames_to_column() %>%
  nest(-rowname) %>%
  select(data) %>%
  flatten() %>%
  map_dbl(so_p)


nested
class(nested)
class(nested$data)


data <- mutate(data, so_p2 = nested)


# since so_p(data) returns a vector, we can create a new column with it
# equivalent to data$so_p <- so_p(data)
data <- mutate(data, so_p = so_p(data))
data <- mutate(data, bb_p = bb_p(data))
data <- mutate(data, iso = iso(data))
data <- mutate(data, babip = babip(data))
data <- mutate(data, avg = avg(data))
data <- mutate(data, obp = obp(data))
data <- mutate(data, slg = slg(data))
data <- mutate(data, ops = ops(data))
data

# equivalent to above
data <- data %>%
  mutate(so_p = so_p(.)) %>%
  mutate(bb_p = bb_p(.)) %>%
  mutate(iso = iso(.)) %>%
  mutate(babip = babip(.)) %>%
  mutate(avg = avg(.)) %>%
  mutate(obp = obp(.)) %>%
  mutate(slg = slg(.)) %>%
  mutate(ops = ops(.))
data

data %>%
  filter(team == "WLC", pa >= 50) %>%
  arrange(desc(ops))



# this is ok for functions with few arguments
# but becomes unwieldy for functions with many arguments
mutate(data, 
       soPct = so_p2(pa, so),
       bbPct = bb_p(pa, bb),
       babip = babip(ab)
)


data %>% 
  select(pa, so) %>%
  pmap(so_p2) %>%
  head()



new_col <- c()
for (row in 1:nrow(data)) {
  new_col <- c(new_col, so_p1(slice(data, row)))
}
new_col[1:5]

data
