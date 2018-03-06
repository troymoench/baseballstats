source("utils.R")

# *********************
# *** Basic Metrics ***
# *********************

# takes list or dataframe as input

go_fo <- function(.d) {
  .d$go / .d$fo
}

so_p <- function(.d) {
  (.d$so / .d$pa)*100
}

bb_p <- function(.d) {
  (.d$bb / .d$pa)*100
}

hbp_p <- function(.d) {
  (.d$hbp / .d$pa)*100
}

iso <- function(.d) {
  (.d$x2b + 2*.d$x3b + 3*.d$hr) / .d$ab
}

babip <- function(.d) {
  (.d$h - .d$hr) / (.d$ab - .d$so - .d$hr + .d$sf)
}

avg <- function(.d) {
  .d$h / .d$ab
}

obp <- function(.d) {
  (.d$h + .d$bb + .d$hbp) / (.d$ab + .d$bb + .d$hbp + .d$sf)
}

slg <- function(.d) {
  x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  (x1b + 2*.d$x2b + 3*.d$x3b + 4*.d$hr) / .d$ab
}

ops <- function(.d) {
  obp(.d) + slg(.d)
}

sar <- function(.d) {
  x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  (.d$sb + .d$cs) / (.d$hbp + .d$bb + x1b)
}
