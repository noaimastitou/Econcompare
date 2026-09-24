panel_fixture <- function() {
  set.seed(120011)
  d <- expand.grid(period = 1:9, id = sprintf("unit%02d", 1:32), stringsAsFactors = FALSE)
  a <- rnorm(32); u <- match(d$id, unique(d$id))
  d$x <- rnorm(nrow(d)) + .4 * a[u]
  d$w <- rnorm(nrow(d))
  d$z <- rep(rnorm(32), each = 9)
  d$y <- 2 + .8 * d$x - .3 * d$w + a[u] + .1 * d$period + rnorm(nrow(d))
  d
}


panel_special_fixture <- function() {
  d <- panel_fixture(); set.seed(140014)
  d$binary <- rbinom(nrow(d), 1, plogis(.3 * d$x - .2 * d$w))
  d$binary[d$period == 1] <- 0; d$binary[d$period == 2] <- 1
  d$count <- rpois(nrow(d), exp(.5 + .25 * d$x - .15 * d$w))
  d$iv1 <- rnorm(nrow(d)); d$iv2 <- rnorm(nrow(d)); u <- rnorm(nrow(d))
  d$endo <- d$iv1 + .6 * d$iv2 + .5 * d$w + u
  d$iv_y <- 1.2 * d$endo + .3 * d$w + d$z + .7 * u + rnorm(nrow(d))
  d
}

