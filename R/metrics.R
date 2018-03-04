library(tidyverse)
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

# *********************
# * Advanced Metrics **
# *********************

bsr <- function(.d, bmult = 1) {
  # Base Runs
  # BsR = A(B/(B+C)) + D
  # requires ab, h, 2b, 3b, hr, bb, hbp, sf, sh, gdp, sb, cs
  
  
  
  x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  
  a <- .d$h + .d$bb + .d$hbp - .d$hr - .d$cs - .d$gdp
  b <- .777*x1b + 2.61*.d$x2b + 4.29*.d$x3b + 2.43*.d$hr + 0.03*(.d$bb + .d$hbp) + 
    1.30*.d$sb + .13*.d$cs + 1.08*.d$sh + 1.81*.d$sf + 0.70*.d$gdp - 0.04*(.d$ab - .d$h)
  c <- .d$ab - .d$h + .d$sh + .d$sf
  d <- .d$hr
  
  b <- b*bmult
  
  a*(b/(b+c)) + d
}

bsr_bmult <- function(.d) {
  # Base Runs B multiplier
  
  x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  
  a <- .d$h + .d$bb + .d$hbp - .d$hr - .d$cs - .d$gdp
  b <- .777*x1b + 2.61*.d$x2b + 4.29*.d$x3b + 2.43*.d$hr + 0.03*(.d$bb + .d$hbp) + 
    1.30*.d$sb + .13*.d$cs + 1.08*.d$sh + 1.81*.d$sf + 0.70*.d$gdp - 0.04*(.d$ab - .d$h)
  c <- .d$ab - .d$h + .d$sh + .d$sf
  d <- .d$hr
  
  b_act <- c*(d - .d$r)/(.d$r - d - a)
  b_est <- b
  b_act/b_est
}

linear_weights_incr1 <- function(.d) {
  # Calculate Linear Weights using the increment method (plus one method) 

  incr <- 0.00000001
  
  .d$x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  labels <- c("ab", "bb", "hbp", "h", "x1b", "x2b", "x3b", "hr", "sb", "cs")
  x <- rep(0, length(labels))
  names(x) <- labels
  x["ab"] <- x["ab"] + incr
  x["h"] <- x["h"] + incr
  x["x1b"] <- x["x1b"] + incr
  
    
  # inc_list <- list(
  #   hbp = c(0, 0, incr, 0, 0, 0, 0, 0, 0, 0),
  #   bb = c(0, incr, 0, 0, 0, 0, 0, 0, 0, 0),
  #   x1b = c(incr, 0, 0, incr, incr, 0, 0, 0, 0, 0),
  #   x2b = c(incr, 0, 0, incr, 0, incr, 0, 0, 0, 0),
  #   x3b = c(incr, 0, 0, incr, 0, 0, incr, 0, 0, 0),
  #   hr = c(incr, 0, 0, incr, 0, 0, 0, incr, 0, 0),
  #   sb = c(0, 0, 0, 0, 0, 0, 0, 0, incr, 0),
  #   cs = c(0, 0, 0, 0, 0, 0, 0, 0, 0, incr),
  #   out = c(incr, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  # ) 

  
  
  bmult <- bsr_bmult(.d)
  base <- bsr(.d, bmult = bmult)

  # .d <- select(.d, labels)
  .d_labels <- names(.d)
  .d <- as.vector(.d, mode="numeric")
  names(.d) <- .d_labels
  
  
  # print(.d)
  # print(x)
  
  temp <- c(.d, x)
  added <- tapply(temp, names(temp), sum)
  # print("Added:")
  # print(added)
  
  
  new <- as.list(added)
  # print("Base:")
  # print(base)
  # print("New:")
  # print(round(bsr(new, bmult), 6))
  
  
  (bsr(new, bmult) - base)*(1/incr)
}

linear_weights_incr <- function(.d) {
  
  # Calculate Linear Weights using the increment method (plus one method)
  
  # print(.d)
  # print(class(.d))
  
  # coerce to a tibble
  .d <- tbl_df(as.list(.d))
  # print(.d)
  # print(class(.d))

  
  incr <- 0.00000001
  
  # .d$x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  input_labels <- c("bb", "hbp", "ab", "h", "x2b", "x3b", "hr", "sb", "cs", "sf", "sh", "gdp")
  lw_labels <- c("lw_hbp", "lw_bb", "lw_x1b", "lw_x2b", "lw_x3b", "lw_hr", "lw_sb", "lw_cs", "lw_out")
  
  v_input <- .d %>% 
    select(input_labels) %>% 
    as.vector(mode = "integer")
  
  names(v_input) <- input_labels
  # print(v_input)
  
  M_incr <- matrix(c(
    c(0,incr,0,0,0,0,0,0,0,0,0,0), # hbp
    c(incr,0,0,0,0,0,0,0,0,0,0,0), # bb
    c(0,0,incr,incr,0,0,0,0,0,0,0,0), # 1b
    c(0,0,incr,incr,incr,0,0,0,0,0,0,0), # 2b
    c(0,0,incr,incr,0,incr,0,0,0,0,0,0), # 3b
    c(0,0,incr,incr,0,0,incr,0,0,0,0,0), # hr
    c(0,0,0,0,0,0,0,incr,0,0,0,0), # sb
    c(0,0,0,0,0,0,0,0,incr,0,0,0), # cs
    c(0,0,incr,0,0,0,0,0,0,0,0,0) # out
  ), nrow = length(input_labels))
  
  # print(M_incr)
  M_input <- M_incr + v_input
  dimnames(M_input) <- list(input_labels, lw_labels)
  # print(M_input)
  
  
  bmult <- bsr_bmult(.d)
  base <- bsr(.d, bmult = bmult)
  
  apply(M_input, 2, function(x) {(bsr(as.list(x), bmult) - base)*(1/incr)})
}

linear_weights_calc <- function(.d) {
  
  # Calculate Linear Weights using multivariable calculus
  
  # coerce to a tibble
  .d <- tbl_df(as.list(.d))
  
  input_labels <- c("bb", "hbp", "ab", "h", "x2b", "x3b", "hr", "sb", "cs", "sf", "sh", "gdp")
  lw_labels <- c("lw_hbp", "lw_bb", "lw_x1b", "lw_x2b", "lw_x3b", "lw_hr", "lw_sb", "lw_cs", "lw_out")
  
  v_input <- .d %>% 
    select(input_labels) %>% 
    as.vector(mode = "integer")
  
  names(v_input) <- input_labels
  
  # TODO: Finish
}

woba_weights <- function(.d, target) {
  # calculate woba weights for hbp, bb, 1b, 2b, 3b, hr
  if (length(target) != 1) {
    message("Length of target must be 1!")
    return()
  }
  
  # coerce to a tibble
  .d <- tbl_df(as.list(.d))
  
  .d$x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  
  totals <- .d %>% 
    select(c("hbp", "bb", "x1b", "x2b", "x3b", "hr")) %>% 
    as_vector()
    
  lw <- .d %>% 
    select(lw_hbp, lw_bb, lw_x1b, lw_x2b, lw_x3b, lw_hr, lw_sb, lw_cs, lw_out) %>% 
    as_vector()
  
  lw <- lw - lw["lw_out"] # subtract value of an out
  
  drop <- c("lw_sb", "lw_cs", "lw_out")
  lw <- lw[setdiff(names(lw), drop)] # remove elements from named vector

  
  acc <- dot(lw, totals)
  
  raw <- acc / .d$pa
  scale <- target / raw
  ww <- lw * scale
  names(ww) <- c("ww_hbp", "ww_bb", "ww_x1b", "ww_x2b", "ww_x3b", "ww_hr")
  ww
}

woba <- function(.d, .w) { 
  .d$x1b <- .d$h - .d$x2b - .d$x3b - .d$hr
  (.w["ww_hbp"]*.d$hbp + .w["ww_bb"]*.d$bb + .w["ww_x1b"]*.d$x1b + .w["ww_x2b"]*.d$x2b + .w["ww_x3b"]*.d$x3b + .w["ww_hr"]*.d$hr) / .d$pa   
}
